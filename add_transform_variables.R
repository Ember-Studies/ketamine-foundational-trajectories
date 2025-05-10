## Use this script to add additional variables
# - comorbidities
# - age categories
# - ketamine dose: TBD
# - medication indicator variables: TBD

add_transform_variables <- function(merged_phq_patient_data, max_prior_infusion_threshold, path_data, path_code, plot_hist = FALSE){
  
  ## Parse comorbidities
  source(paste0(path_code, 'diagnoses_comorbidities_from_icd_and_self_report.R'))
  merged_phq_patient_data <- diagnoses_and_comorbidities_from_icd_and_self_report(merged_phq_patient_data = merged_phq_patient_data, path_data = path_data)
  
  ## Create age categories
  merged_phq_patient_data$age_category_adolescent <- (merged_phq_patient_data$age_years < 18)*1
  merged_phq_patient_data$age_category_adult <- (merged_phq_patient_data$age_years >= 18 & merged_phq_patient_data$age_years < 65)*1
  merged_phq_patient_data$age_category_senior <- (merged_phq_patient_data$age_years >= 65)*1
  merged_phq_patient_data$age_bin <- rep(NA, nrow(merged_phq_patient_data))
  merged_phq_patient_data$age_bin[which(merged_phq_patient_data$age_category_adolescent == 1)] <- 'adolescent'
  merged_phq_patient_data$age_bin[which(merged_phq_patient_data$age_category_adult == 1)] <- 'adult'
  merged_phq_patient_data$age_bin[which(merged_phq_patient_data$age_category_senior == 1)] <- 'senior'
  merged_phq_patient_data$age_bin <- as.factor(merged_phq_patient_data$age_bin)
  
  ## Dose
  # get dose data
  source(paste0(path_code, 'format_dose.R'))
  dose_data <- format_dose(path_data = path_data, plot_hist = plot_hist)
  
  # add dose data to main data frame
  source(paste0(path_code, 'add_dose.R'))
  merged_phq_patient_data <- add_dose(merged_phq_patient_data = merged_phq_patient_data, 
                                      dose_data = dose_data, 
                                      max_prior_infusion_threshold = max_prior_infusion_threshold)
  
  ## Medication indicator variables: TBD
  source(paste0(path_code, 'parse_medications.R'))
  merged_phq_patient_data <- parse_medications(merged_phq_patient_data = merged_phq_patient_data)
  
  ## Per-protocol indicator
  per_protocol_index <- which(((merged_phq_patient_data$last_foundational_infusion - 
                                  merged_phq_patient_data$first_infusion_completed) <= 14 ) & 
                                ( merged_phq_patient_data$number_foundational_infusions_including_today == 4 ) &
                                (merged_phq_patient_data$foundation_ember_recommended == 'Yes - Full Foundation'))
  
  merged_phq_patient_data$per_protocol <- ifelse(1:nrow(merged_phq_patient_data) %in% per_protocol_index, 1, 0)
  
  # sanity checks
  #summary(as.numeric((merged_phq_patient_data$last_foundational_infusion - merged_phq_patient_data$first_infusion_completed)[merged_phq_patient_data$per_protocol==1]))
  #summary(merged_phq_patient_data$number_foundational_infusions_including_today[merged_phq_patient_data$per_protocol==1])
  
  return(merged_phq_patient_data)
  
}