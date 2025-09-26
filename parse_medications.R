# Function to match a single raw drug string against reference medication list
match_med <- function(drug_raw, reference_df, max_dist = 2) {
  drug_raw <- tolower(drug_raw)
  
  # Try matching full drug string at increasing distance thresholds
  for (dist_level in 0:max_dist) {
    dists <- stringdist(drug_raw, reference_df$reference, method = "osa")
    matched_rows <- reference_df[dists <= dist_level, ]
    
    if (nrow(matched_rows) > 0) {
      return(list(
        degree_matched = as.character(dist_level),
        string_split_needed = "No",
        num_matches = nrow(matched_rows),
        drugs_matched = paste(unique(matched_rows$reference), collapse = "; "),
        matched_cats = unique(matched_rows$category)
      ))
    }
  }
  
  # If no match, try splitting the string on delimiters and retry matching tokens
  parts <- unlist(strsplit(drug_raw, "\\s|/|\\+|,|-"))
  parts <- parts[parts != ""]  # remove empty strings if any
  
  all_matched_rows <- tibble()
  
  # try tracking dist_level
  dist_list <- list()
  u <- 1
  
  for (part in parts) {
    for (dist_level in 0:max_dist) {
      dists <- stringdist(part, reference_df$reference, method = "osa")
      matched_rows <- reference_df[dists <= dist_level, ]
      
      dist_list[[u]] <- dist_level
      u <- u+1
      
      if (nrow(matched_rows) > 0) {
        all_matched_rows <- bind_rows(all_matched_rows, matched_rows)
        # Once matched at one dist_level for a part, no need to check higher dist for that part
        break
      }
    }
  }
  
  if (nrow(all_matched_rows) > 0) {
    all_matched_rows <- distinct(all_matched_rows)
    # Degree matched is minimal distance found among matched rows
    # For simplicity, set degree_matched = min distance across all matched rows for parts
    # But here we don't have individual distances for parts; approximate as "1" (since token matching)
    # You could enhance this by keeping track of distances per token if needed.
    return(list(
      degree_matched = max(unlist(dist_list)),
      #degree_matched = "1",  # token split match considered degree 1
      string_split_needed = "Yes",
      num_matches = nrow(all_matched_rows),
      drugs_matched = paste(unique(all_matched_rows$reference), collapse = "; "),
      matched_cats = unique(all_matched_rows$category)
    ))
  }
  
  # No match found at all
  return(list(
    degree_matched = "no_match",
    string_split_needed = "Yes",
    num_matches = 0,
    drugs_matched = NA_character_,
    matched_cats = "Non-Psychotropic"
  ))
}

# Main parsing function
parse_medications <- function(merged_phq_patient_data, osa_distance_threshold = 2, path_data = NULL, path_out = NULL, save_meds_table = FALSE) {
  
  # Load med classifications from Excel sheet, normalize categories and references
  med_classifications <- read_xlsx(
    file.path(path_data, 'Medication_List_Clasifications_V2.xlsx'),
    sheet = 'v3 Med Table for Manuscripts'
  ) %>%
    clean_names() %>%
    select(drug_name, final_classification) %>%
    distinct() %>%
    rename(reference = drug_name, category = final_classification) %>%
    mutate(
      reference = tolower(reference),
      # Normalize categories to consistent labels matching your target list:
      category = case_when(
        str_detect(category, regex("antidepressant", ignore_case = TRUE)) ~ "Antidepressant",
        str_detect(category, regex("antipsychotic", ignore_case = TRUE)) ~ "Antipsychotic",
        str_detect(category, regex("benzodiazepine", ignore_case = TRUE)) ~ "Benzodiazepine",
        str_detect(category, regex("mood stabilizer", ignore_case = TRUE)) ~ "Mood Stabilizer",
        #str_detect(category, regex("non-benzodiazepine anxiolytic/sedative", ignore_case = TRUE)) ~ "Non-Benzodiazepine Anxiolytic/Sedative",
        str_detect(category, regex("nbas", ignore_case = TRUE)) ~ "NBAS",
        str_detect(category, regex("other psychotropic medication", ignore_case = TRUE)) ~ "Other Psychotropic medication",
        str_detect(category, regex("stimulant", ignore_case = TRUE)) ~ "Stimulant",
        TRUE ~ "Non-Psychotropic"
        # Try adding a null category here; if NA 
      )
    )
  
  # Define your medication categories
  categories <- c( # Changed "Non-Benzodiazepine Anxiolytic/Sedative" to NBAS to prevent mismatch with Benzodiazepine
    "Antidepressant", "Antipsychotic", "Benzodiazepine", "Mood Stabilizer",
    "NBAS", "Other Psychotropic medication", "Stimulant"
  )
  
  # Prepare normalized column names for categories
  category_cols <- gsub("[^a-zA-Z]", "_", tolower(categories))
  
  # Identify medication columns ending with "medication_name"
  med_cols <- names(merged_phq_patient_data)[grepl("medication_name$", names(merged_phq_patient_data))]
  if (length(med_cols) == 0) stop("No columns ending with 'medication_name' found.")
  
  # Pivot to long format: one row per patient per medication
  med_long <- merged_phq_patient_data %>%
    select(client_id, all_of(med_cols)) %>%
    pivot_longer(cols = all_of(med_cols), names_to = "med_col", values_to = "medication_raw") %>%
    filter(!is.na(medication_raw), medication_raw != "")
  
  
  ## Clean med_long
  #1. remove extraneous strings
  med_long$medication_raw <- gsub("vitamins and herbs only:", "", med_long$medication_raw)
  med_long$medication_raw <- gsub("this list is very long and a sheet can be sent to you with all meds and doses", "", med_long$medication_raw)
  med_long$medication_raw <- gsub("various vitamines and supplements", "", med_long$medication_raw)
  med_long$medication_raw <- gsub("oral|Oral", "", med_long$medication_raw)
  med_long$medication_raw <- gsub("none", "", med_long$medication_raw) # changed to "" from "blank"
  med_long$medication_raw <- gsub("None", "", med_long$medication_raw) # changed to "" from "Blank"
  med_long$medication_raw <- gsub("NONE", "", med_long$medication_raw) # changed to "" from "Blank"
  med_long$medication_raw <- gsub("Calcium", "", med_long$medication_raw)
  med_long$medication_raw <- gsub("nasal spray", "", med_long$medication_raw)
  med_long$medication_raw <- gsub("Will review when I begin. Too many to list right now", "", med_long$medication_raw)
  med_long$medication_raw <- gsub("All this was covered by my psychiatrist", "", med_long$medication_raw)
  med_long$medication_raw <- gsub("I am going to email a medication list separately because it would be very time-consuming to input the info here.", "", med_long$medication_raw)
  med_long$medication_raw <- gsub("birth control", "birthcontrol", med_long$medication_raw)
  med_long$medication_raw <- gsub("Birth Control", "birthcontrol", med_long$medication_raw)
  med_long$medication_raw <- gsub("nose spray", "", med_long$medication_raw)
  med_long$medication_raw <- gsub("THC", "tetrahydrocannabinol", med_long$medication_raw)
  
  # 2. commas to separate rows: Notes: This will map elements separated by commas to separate rows but I havne't changed the "med_col" name to be re-indexed (e.g., all will be medication_1_medication_name)
  med_long <- med_long %>%
    tidyr::separate_rows(medication_raw, sep = ",")
  
  # 3. rename vitamin to vitamine to prevent match with ritalin
  med_long$medication_raw <- gsub('vitamin', 'vitaminsupplements', med_long$medication_raw)
  med_long$medication_raw <- gsub('Vitamin', 'vitaminsupplements', med_long$medication_raw)
  med_long$medication_raw <- gsub('VITAMIN', 'vitaminsupplements', med_long$medication_raw)
  med_long$medication_raw <- gsub('CBD', 'cannabidiol', med_long$medication_raw)
  med_long$medication_raw <- gsub('cbd', 'cannabidiol', med_long$medication_raw)
  
  # 4. replace Clonipin with Klonopin
  med_long$medication_raw <- gsub("Clonipin", "Klonopin", med_long$medication_raw)
  med_long$medication_raw <- gsub("clonipin", "Klonopin", med_long$medication_raw)
  
  # 5. change empty spaces to no spaces: " " -> ""
  med_long$medication_raw <- gsub("^ ", "", med_long$medication_raw)
  med_long$medication_raw <- gsub(" $", "", med_long$medication_raw)
  
  # Run matching for each medication string
  matched_results <- med_long %>%
    rowwise() %>%
    mutate(match_info = list(match_med(medication_raw, med_classifications, osa_distance_threshold))) %>%
    ungroup() %>%
    tidyr::hoist(
      match_info,
      degree_matched = "degree_matched",
      string_split_needed = "string_split_needed",
      num_matches = "num_matches",
      drugs_matched = "drugs_matched",
      categories = "matched_cats"
    )
  
  # Save medication table if condition true
  if (save_meds_table) {
    # write.csv(match_metadata_wide, file = file.path(path_out, "medication_metadata.csv"), row.names = FALSE)
    #write.csv(matched_results, file = file.path(path_out, "medication_metadata.csv"), row.names = FALSE)
    #write.csv(matched_results, file = paste0(path_out, "medication_metadata.csv"), row.names = FALSE)
    save(matched_results, file = paste0(path_out, "medication_metadata.Rdata"))
  }
  
  # Initialize category count columns with 0
  for (col in category_cols) {
    matched_results[[col]] <- 0L
  }
  
  for (i in seq_len(nrow(matched_results))) {
    cats <- matched_results$categories[[i]]
    drug_str <- matched_results$drugs_matched[[i]] # new: This may not be an NA value at this point; print out and check
    # Ensure it's a character vector, and normalize category names
    cats <- as.character(cats)
    cats <- unique(gsub("[^a-zA-Z]", "_", tolower(cats)))
    if (length(cats) > 0 && any(!is.na(cats)) && !is.na(drug_str)) { # new
      for (c in cats) {
        if (c %in% category_cols) {
          matched_results[i, c] <- 1L  # Only count 1 per category per medication string
        }
      }
    }
  }
  
  # drop empty character values
  matched_results <- matched_results[!matched_results$medication_raw=="", ] # new
  
  # Determine psychotropic status per drug row: 1 if ANY matched category is NOT Non-Psychotropic
  matched_results <- matched_results %>%
    mutate(
      is_psychotropic = if_else(
        # If matched categories contain any psychotropic categories
        map_lgl(categories, ~ any(.x != "Non-Psychotropic")),
        1L, 0L
      )
    )
  
  # Summarize category counts and psychotropic loads per patient
  category_summaries <- matched_results %>%
    distinct(client_id, medication_raw, .keep_all = TRUE) %>%  # de-duplicate meds per client
    group_by(client_id) %>%
    summarise(
      across(all_of(category_cols), sum, na.rm = TRUE),
      total_psychotropic_medication_load = sum(is_psychotropic, na.rm = TRUE),
      total_non_psychotropic_medication_load = sum(1L - is_psychotropic, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    clean_names()
  
  # Extract matching metadata per medication column
  match_metadata_wide <- matched_results %>%
    select(client_id, med_col, degree_matched, string_split_needed, num_matches) %>%
    pivot_wider(
      names_from = med_col,
      values_from = c(degree_matched, string_split_needed, num_matches),
      names_glue = "{med_col}_{.value}"
    )
  
  # Join summary back to original patient-level data
  final_df <- merged_phq_patient_data %>%
    left_join(category_summaries, by = "client_id")
  
  return(final_df)
}