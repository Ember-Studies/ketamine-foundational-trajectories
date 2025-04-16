## Use this script to add additional variables
# - comorbidities
# - age categories
# - ketamine dose: TBD
# - medication indicator variables: TBD

add_transform_variables <- function(merged_phq_patient_data, max_prior_infusion_threshold, path_data, path_code){
  
  ## Parse comorbidities
  merged_phq_patient_data$comorbid_anxiety <- grepl('Anxiety', merged_phq_patient_data$do_you_have_any_of_the_following_mental_health_conditions_check_all_that_apply)*1
  merged_phq_patient_data$comorbid_ocd <- grepl('OCD', merged_phq_patient_data$do_you_have_any_of_the_following_mental_health_conditions_check_all_that_apply)*1
  merged_phq_patient_data$comorbid_ptsd <- grepl('PTSD', merged_phq_patient_data$do_you_have_any_of_the_following_mental_health_conditions_check_all_that_apply)*1
  merged_phq_patient_data$comorbid_sud <- grepl('Substance Use Disorder', merged_phq_patient_data$do_you_have_any_of_the_following_mental_health_conditions_check_all_that_apply)*1
  merged_phq_patient_data$comorbid_aud <- grepl('Alcohol Use Disorder', merged_phq_patient_data$do_you_have_any_of_the_following_mental_health_conditions_check_all_that_apply)*1
  merged_phq_patient_data$comorbid_bipolar <- grepl('Bipolar disorder', merged_phq_patient_data$do_you_have_any_of_the_following_mental_health_conditions_check_all_that_apply)*1
  merged_phq_patient_data$comorbid_depression_mdd <- grepl('Major Depressive Disorder', merged_phq_patient_data$do_you_have_any_of_the_following_mental_health_conditions_check_all_that_apply)*1
  merged_phq_patient_data$comorbid_depression_other <- grepl('other depressive disorder', merged_phq_patient_data$do_you_have_any_of_the_following_mental_health_conditions_check_all_that_apply)*1
  merged_phq_patient_data$comorbid_other_condition <- grepl('Another condition not listed|Another issue not listed', merged_phq_patient_data$do_you_have_any_of_the_following_mental_health_conditions_check_all_that_apply)*1
  
  ## Create age categories
  merged_phq_patient_data$age_category_children <- (merged_phq_patient_data$age_years < 15)*1
  merged_phq_patient_data$age_category_youth <- (merged_phq_patient_data$age_years >= 15 & merged_phq_patient_data$age_years < 25)*1
  merged_phq_patient_data$age_category_adult <- (merged_phq_patient_data$age_years >= 25 & merged_phq_patient_data$age_years < 65)*1
  merged_phq_patient_data$age_category_senior <- (merged_phq_patient_data$age_years >= 65)*1
  merged_phq_patient_data$age_bin <- rep(NA, nrow(merged_phq_patient_data))
  merged_phq_patient_data$age_bin[which(merged_phq_patient_data$age_category_children == 1)] <- 'children'
  merged_phq_patient_data$age_bin[which(merged_phq_patient_data$age_category_youth == 1)] <- 'youth'
  merged_phq_patient_data$age_bin[which(merged_phq_patient_data$age_category_adult == 1)] <- 'adult'
  merged_phq_patient_data$age_bin[which(merged_phq_patient_data$age_category_senior == 1)] <- 'senior'
  merged_phq_patient_data$age_bin <- as.factor(merged_phq_patient_data$age_bin)
  
  ## Dose
  # get dose data
  source(paste0(path_code, 'format_dose.R'))
  dose_data <- format_dose(path_data = path_data)
  
  # add dose data to main data frame
  source(paste0(path_code, 'add_dose.R'))
  merged_phq_patient_data <- add_dose(merged_phq_patient_data = merged_phq_patient_data, 
                                      dose_data = dose_data, 
                                      max_prior_infusion_threshold = max_prior_infusion_threshold)
  
  ## Medication indicator variables: TBD
  source(paste0(path_code, 'parse_medications.R'))
  merged_phq_patient_data <- parse_medications(merged_phq_patient_data = merged_phq_patient_data)
  
  return(merged_phq_patient_data)
  
}