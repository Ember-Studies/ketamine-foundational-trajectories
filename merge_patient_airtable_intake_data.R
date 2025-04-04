merge_patient_airtable_intake_data <- function(airtable_data_filtered, intake_data_filtered){

  # format dates with lubridate
  airtable_data_filtered <- airtable_data_filtered %>%
    mutate(first_infusion_completed=ymd(first_infusion_completed),
           last_foundational_infusion=mdy(last_foundational_infusion)) %>%
    rename('client_id'='intake_q_id')
  
  intake_data_filtered <- intake_data_filtered %>%
    mutate(date_submitted=ymd(date_submitted))
  
  # merge patient table and intake data
  data_merged <- merge(airtable_data_filtered, intake_data_filtered, by = 'client_id')
  
  # find ids from duplicated intake forms
  duplicated_patients <- names(table(data_merged$client_id))[table(data_merged$client_id) > 1]
  
  # get dataframe of only duplicated patients 
  duplicated_df <- data_merged[data_merged$client_id %in% duplicated_patients, ]
  
  # drop duplicated patients from merged dataframe
  data_merged_dropped_duplicates <- data_merged[!data_merged$client_id %in% duplicated_patients, ]
  
  # fill missing sex with record from first visit across duplicates
  tmp_split <- split(duplicated_df, f=duplicated_df$client_id)
  
  tmp_filled <- lapply(tmp_split, function(x){
    glist <- unique(x$sex_assigned_at_birth)
    g <- glist[glist!=""]
    
    # fill with first observed non-missing
    x[, 'sex_assigned_at_birth'] <- g[1]
    return(x)
  })
  
  duplicated_df <- do.call(rbind, tmp_filled) 
  
  # filter duplicates according:
  # 1. Date intake forms (Intake forms table) is BEFORE date of 1st infusion (Patient table)
  # 2. If multiple intake forms are before date of first infusion, take the form with the minimum of the absolute difference between “DATE FIRST INFUSION” vs “DATE INTAKE FORM submitted”
  duplicated_df_transformed <- duplicated_df
  duplicated_df_transformed$diff <- duplicated_df$date_submitted - duplicated_df$first_infusion_completed # switched so intake before infusion is negative-valued
  
  # keep only intakes before infusions
  duplicated_df_transformed <- duplicated_df_transformed[duplicated_df_transformed$diff <= 0, ]
  #hist(as.numeric(duplicated_df_transformed$diff))
  #dim(duplicated_df_transformed)
  
  # where duplicates remain, take the one with minimum absolute difference between intake and first infusion
  duplicated_df_transformed_split <- split(duplicated_df_transformed, f = duplicated_df_transformed$client_id)
  
  filtered_list <- lapply(duplicated_df_transformed_split, function(s){
    idx_min <- which.max(s$diff) # switched to which.max for diff encoding above
    return(s[idx_min, ])
  })
  
  duplicated_df_filtered <- do.call(rbind, filtered_list)
  #dim(duplicated_df_filtered)
  
  # # sanity check
  #which(table(duplicated_df_filtered$client_id)>1)
  #hist(as.numeric(duplicated_df_filtered$diff)) # note that some intakes occur months before first infusion
  
  # remerge dataframes
  duplicated_df_filtered <- subset(duplicated_df_filtered, select = -diff)
  
  data_merged_filtered_intake_and_patient_data <- rbind(data_merged_dropped_duplicates, duplicated_df_filtered)
  
  return(data_merged_filtered_intake_and_patient_data)

}
