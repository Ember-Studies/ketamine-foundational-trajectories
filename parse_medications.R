
parse_medications <- function(merged_phq_patient_data){

  require(tidystringdist)
  
  # read medication classifications: note using edited sheet to drop dual matches (Desyrel and Symbyax)
  medication_classifications <- read_xlsx(paste0(path_data, 'Medication_List_Clasifications.xlsx'), sheet = 'V3 Med Table Edited')
  
  # clean names 
  medication_classifications <- medication_classifications %>%
    clean_names() %>%
    as.data.frame()
  
  # get name and trade names
  med_cols <- grep('genetic_name|trade_name', names(medication_classifications), value = T)
  
  # collapse as string for regular expression and remove NAs
  medication_regular_expressions <- sapply(1:nrow(medication_classifications), function(r){
    string <- paste0(medication_classifications[r, med_cols], collapse = '|')  
    string <- gsub('\\|NA', '', string)
    return(string)
  })
  
  groupings_df <- data.frame(
    medication_regular_expressions=medication_regular_expressions,
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
  
  # separate reference names
  medication_names_unlisted <- unlist(strsplit(medication_list, '\\|'))
  
  # make mapping data frame to link individual names to class and row number for groupings_df
  mapping_df <- data.frame()
  
  u <- 1
  for(m in 1:length(medication_list)){
    
    split_string <- strsplit(medication_list[m], '\\|')
    split_string <- unlist(split_string)
    
    for(s in 1:length(split_string)){
      
      mapping_df[u, 'name'] <- split_string[s]
      mapping_df[u, 'class'] <- groupings_df$medication_grouping[m]
      mapping_df[u, 'reference_row'] <- m
      
      u <- u+1
      
    }
    
  }
  
  # calculate string distances
  combinations <- tidy_comb_all(c(medication_names_unlisted, meds_unique))
  distances <- tidy_stringdist(combinations, method = "osa")
  
  final_distances <- distances %>%
    filter((V1 %in% medication_names_unlisted & V2 %in% meds_unique) | (V1 %in% meds_unique & V2 %in% medication_names_unlisted)) %>%
    dplyr::select(V1, V2, osa) %>%
    dplyr::rename(ref = V1, expr = V2, distance = osa) %>%
    as.data.frame()
  
  # filter distances
  distances_filtered <- final_distances[final_distances$distance<2, ]
  
  # display matches
  print(paste0(distances_filtered$ref, ' : ', distances_filtered$expr))
  
  # initialize medications dataframe
  df_medication_use <- data.frame(client_id = merged_phq_patient_data$client_id)
  
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
  
    # assign indicator to named variable in dataframe  
    df_medication_use[[paste0('medication_use_', medication_name, '_', medication_class)]] <- medication_indicator
  
    # clear variables
    rm(list=c('medication_regex', 'medication_name', 'medication_class',
              'index', 'medication_string_variants', 'variants_regex', 
              'medication_subject_indices', 'medication_indicator'))
    
  }
  
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
  df_medication_use$total_medication_load <- rowSums(df_medication_use[, grep('medication_use_', names(df_medication_use), value = T)])
  
  # merge medication info
  df_medication_use <- df_medication_use %>%
    dplyr::select(-client_id)
  
  merged_phq_patient_data <- cbind(merged_phq_patient_data, df_medication_use)
  
  return(merged_phq_patient_data)

}


