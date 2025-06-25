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
  
  for (part in parts) {
    for (dist_level in 0:max_dist) {
      dists <- stringdist(part, reference_df$reference, method = "osa")
      matched_rows <- reference_df[dists <= dist_level, ]
      
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
      degree_matched = "1",  # token split match considered degree 1
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
        str_detect(category, regex("non-benzodiazepine anxiolytic/sedative", ignore_case = TRUE)) ~ "Non-Benzodiazepine Anxiolytic/Sedative",
        str_detect(category, regex("other psychotropic medication", ignore_case = TRUE)) ~ "Other Psychotropic medication",
        str_detect(category, regex("stimulant", ignore_case = TRUE)) ~ "Stimulant",
        TRUE ~ "Non-Psychotropic"
      )
    )
  
  # Define your medication categories
  categories <- c(
    "Antidepressant", "Antipsychotic", "Benzodiazepine", "Mood Stabilizer",
    "Non-Benzodiazepine Anxiolytic/Sedative", "Other Psychotropic medication", "Stimulant"
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
    write.csv(matched_results, file = file.path(path_out, "medication_metadata.csv"), row.names = FALSE)
  }
  
  # Initialize category count columns with 0
  for (col in category_cols) {
    matched_results[[col]] <- 0L
  }
  
  for (i in seq_len(nrow(matched_results))) {
    cats <- matched_results$categories[[i]]
    # Ensure it's a character vector, and normalize category names
    cats <- as.character(cats)
    cats <- unique(gsub("[^a-zA-Z]", "_", tolower(cats)))
    if (length(cats) > 0 && any(!is.na(cats))) {
      for (c in cats) {
        if (c %in% category_cols) {
          matched_results[i, c] <- 1L  # Only count 1 per category per medication string
        }
      }
    }
  }
  
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


parse_medications2 <- function(merged_phq_patient_data, osa_distance_threshold){
  
  require(tidystringdist)
  
  # read medication classifications: note using edited sheet to drop dual matches (Desyrel and Symbyax)
  medication_classifications <- read_xlsx(paste0(path_data, 'Medication_List_Clasifications_V2.xlsx'), sheet = 'v3 Med Table for Manuscripts')
  
  # clean names 
  medication_classifications <- medication_classifications %>%
    clean_names() %>%
    as.data.frame()
  
  # get name and trade names
  med_cols <- 'drug_name'
  
  groupings_df <- data.frame(
    medication_regular_expressions=medication_classifications$drug_name,
    medication_grouping=medication_classifications$final_classification
  )
  
  # list of medications and brand names
  medication_list <- groupings_df$medication_regular_expressions
  
  # subset medication columns
  meds <- merged_phq_patient_data[, grep('medication_name', names(merged_phq_patient_data))]
  
  # get unique medication strings across columns
  meds_unique <- unique(unlist(meds, use.names=FALSE))
  
  # remove missing/NAs
  meds_unique <- meds_unique[!meds_unique %in% c("", NA)]
  
  # make mapping data frame to link individual names to class and row number for groupings_df
  mapping_df <- data.frame(name=groupings_df$medication_regular_expressions,
                           class=groupings_df$medication_grouping,
                           reference_row = seq(1:nrow(groupings_df)))
  
  # calculate string distances
  combinations <- tidy_comb_all(c(medication_list, meds_unique))
  distances <- tidy_stringdist(combinations, method = "osa")
  
  final_distances <- distances %>%
    filter((V1 %in% medication_list & V2 %in% meds_unique) | (V1 %in% meds_unique & V2 %in% medication_list)) %>%
    dplyr::select(V1, V2, osa) %>%
    dplyr::rename(ref = V1, expr = V2, distance = osa) %>%
    as.data.frame()
  
  # filter distances
  distances_filtered <- final_distances[final_distances$distance <= osa_distance_threshold, ]
  
  # display matches
  print(paste0(distances_filtered$ref, ' : ', distances_filtered$expr))
  
  # initialize medications dataframe
  df_medication_use <- data.frame(client_id = merged_phq_patient_data$client_id)
  
  # copy meds dataframe to set matches to null for subsequent identification of non-psychotropic meds
  meds_copy <- meds
  
  # initialize list of string distances used
  med_str_dist_list <- list()
  
  for(r in 1:length(unique(mapping_df$reference_row))){
    
    print(r)
    
    # combine medication and brand names
    medication_regex <- paste0(paste0(mapping_df[mapping_df$reference_row==r, 'name'], collapse = '$|'), '$')
    
    # grab medication name: first in dataframe subset
    medication_name <- mapping_df[mapping_df$reference_row==r, 'name'][1]
    
    # grab medication class
    medication_class <- mapping_df[mapping_df$reference_row==r, 'class'][1]
    
    # find medication name in distance data frame
    index <- grep(medication_regex, distances_filtered$ref)
    
    # if no matches, add zero-valued indicator variable and skip to next
    if(length(index)==0){
      print("No matching medications; skipping")
      
      # create indicator variable from identified indices
      medication_indicator <- rep(0, nrow(merged_phq_patient_data))
      
      # assign indicator to named variable in dataframe  
      df_medication_use[[paste0('medication_use_', medication_name, '_', medication_class)]] <- medication_indicator
      
      # clear variables
      rm(list=c('medication_regex', 'medication_name', 'medication_class',
                'index', 'medication_indicator'))
      
      next
      
    }
    
    # collect correct and misspelled variants
    medication_string_variants <- unique(unlist(c(distances_filtered[index, c('ref', 'expr')])))
    
    # initialize dataframe for distance measures for given string
    med_str_dist <- data.frame(string = rep(NA, length(medication_string_variants)),
                               distance = rep(NA, length(medication_string_variants)))
    
    for(m in 1:length(medication_string_variants)){
      
      # find distance from distance_filtered object
      #osa_dist <- distances_filtered[grep(medication_string_variants[m], distances_filtered[, c('ref', 'expr')])[1], 'distance']
      
      match_index <- which(distances_filtered == medication_string_variants[m], arr.ind = TRUE)
      match_index <- match_index[1, 1] # assumes best match is first
      osa_dist <- distances_filtered[match_index, 'distance']
      
      # record string and distance
      med_str_dist$string[m] <- medication_string_variants[m]
      med_str_dist$distance[m] <- osa_dist
    }
    
    med_str_dist_list[[r]] <- med_str_dist
    
    # create regular expression
    variants_regex <- paste0(paste0(medication_string_variants, collapse = '$|'), '$')
    
    medication_subject_indices <- lapply(1:ncol(meds), function(c){
      grep(variants_regex, meds[, c])
    })
    medication_subject_indices <- unique(unlist(medication_subject_indices))
    
    # create indicator variable from identified indices
    medication_indicator <- rep(0, nrow(merged_phq_patient_data))
    medication_indicator[medication_subject_indices] <- 1
    
    table(medication_indicator)
    
    
    # create variable to store distance of matched string
    medication_string_distance <- rep(NA, nrow(merged_phq_patient_data))
    
    for(r in 1:nrow(med_str_dist)){
      current_string <- paste0(med_str_dist$string[r], '$')
      medication_subject_indices_distance_match <- lapply(1:ncol(meds), function(k){
        grep(current_string, meds[, k])
      })
      medication_subject_indices_distance_match <- unique(unlist(medication_subject_indices_distance_match))
      
      # store distance of current string
      medication_string_distance[medication_subject_indices_distance_match] <- med_str_dist$distance[r]
    }
    
    #table(medication_string_distance)
    #table(medication_indicator)
    
    ## sanity check: indices from medication_string_distance should all be in indices for medication indicator
    unique_distances <- unique(medication_string_distance)[!is.na(unique(medication_string_distance))]
    msd_indices <- lapply(unique_distances, function(d){
      which(medication_string_distance==d)
    })
    msd_indices <- unique(unlist(msd_indices))
    
    diff_length <- length(setdiff(msd_indices, which(medication_indicator==1)))
    
    # stop and report error
    if(diff_length>0){
      print("mismatch between distance record match and medication indicator. Stopping...")
      print(sprintf("Error at mapping_df row index %s, medication %s", r, variants_regex))
      stop
    }
    
    # set matches to "" in meds_copy to null identified matches
    for(n in 1:ncol(meds_copy)){
      idx_null <- grep(variants_regex, meds_copy[, n])
      meds_copy[idx_null, n] <- ""
    }
    
    # assign indicator to named variable in dataframe  
    df_medication_use[[paste0('medication_use_', medication_name, '_', medication_class)]] <- medication_indicator
    
    # assign distance of medication match to named variable in dataframe
    df_medication_use[[paste0('medication_string_match_distance_', medication_name)]] <- medication_string_distance
    
    # clear variables
    rm(list=c('medication_regex', 'medication_name', 'medication_class',
              'index', 'medication_string_variants', 'variants_regex', 
              'medication_subject_indices', 'medication_indicator'))
    
  }
  
  # format and bind med_str_dist_list
  #med_str_dist_list_saved <- med_str_dist_list
  
  for(r in 1:nrow(mapping_df)){
    
    if(is.null(med_str_dist_list[[r]])){
      med_str_dist_list[[r]] <- data.frame(
        string = mapping_df$name[r],
        reference_string = mapping_df$name[r],
        distance = 0,
        class = mapping_df$class[r],
        match_found = FALSE
      )
      
    }else{
      
      med_str_dist_list[[r]]$reference_string <- mapping_df$name[r]
      med_str_dist_list[[r]]$class <- mapping_df$class[r]
      med_str_dist_list[[r]]$match_found <- TRUE
      
      # reorder columns
      med_str_dist_list[[r]] <- med_str_dist_list[[r]][, c('string', 'reference_string', 'distance', 'class', 'match_found')]
      
    }
    
  }
  
  med_str_dist_list <- do.call(rbind, med_str_dist_list)
  write.csv(x = med_str_dist_list, file = paste0(path_out, 'medication_match_summary.csv'), quote = FALSE, row.names = FALSE)
  
  # clean names
  df_medication_use <- df_medication_use %>%
    clean_names()
  
  # calculate medication class loads
  df_medication_use$antidepressant_load <- rowSums(df_medication_use[, grep('_antidepressant$', names(df_medication_use), value = T)])
  df_medication_use$antipsychotic_load <- rowSums(df_medication_use[, grep('_antipsychotic$', names(df_medication_use), value = T)])
  df_medication_use$benzodiazepine_load <- rowSums(df_medication_use[, grep('_benzodiazepine$', names(df_medication_use), value = T)])
  df_medication_use$mood_stabilizer_load <- rowSums(df_medication_use[, grep('_mood_stabilizer$', names(df_medication_use), value = T)])
  df_medication_use$non_benzodiazepine_anxiolytic_sedative_load <- rowSums(df_medication_use[, grep('_non_benzodiazepine_anxiolytic_sedative$', names(df_medication_use), value = T)])
  df_medication_use$stimulant_load <- rowSums(df_medication_use[, grep('_stimulant$', names(df_medication_use), value = T)])
  df_medication_use$other_psychotropic_medication_load <- rowSums(df_medication_use[, grep('_other_psychotropic_medication$', names(df_medication_use), value = T)])
  df_medication_use$total_psychotropic_medication_load <- rowSums(df_medication_use[, grep('medication_use_', names(df_medication_use), value = T)])
  
  # add non-psychotropic medication load
  df_medication_use$total_non_psychotropic_medication_load <- rowSums(meds_copy != "")
  
  # merge medication info
  df_medication_use <- df_medication_use %>%
    dplyr::select(-client_id)
  
  merged_phq_patient_data <- cbind(merged_phq_patient_data, df_medication_use)
  
  return(merged_phq_patient_data)
  
}


