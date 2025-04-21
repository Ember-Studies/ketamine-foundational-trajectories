
## Notes:
# - Give precedence to ICD codes and fill secondarily with self-report
# - Give precedence to Bipolar diagnosis 
# - All patients who are not bipolar (ICD contains F31) will be MDD per Ember inclusion criteria

diagnoses_and_comorbidities_from_icd_and_self_report <- function(merged_phq_patient_data, path_data){
  
  # read ICD data from all clients data
  all_clients <- readxl::read_xlsx(paste0(path_data, 'MGH - List - All Clients.xlsx'), sheet = '2025Data')
  
  icd_data <- all_clients %>%
    clean_names() %>%
    dplyr::select(client_id, diagnosis) %>%
    as.data.frame()
  
  ## format ICD codes
  icd_data$primary_diagnosis_bipolar <- grepl("F31", icd_data$diagnosis)*1
  icd_data$comorbid_anxiety <- grepl("F41|F40", icd_data$diagnosis)*1
  icd_data$comorbid_ocd <- grepl("F42", icd_data$diagnosis)*1
  icd_data$comorbid_ptsd <- grepl("F43", icd_data$diagnosis)*1
  icd_data$comorbid_sud <- grepl("F1[0-9]", icd_data$diagnosis)*1 # alcohol use and substance use combined
  
  ## format self-report 
  merged_phq_patient_data$primary_diagnosis_bipolar <- grepl('Bipolar disorder', merged_phq_patient_data$do_you_have_any_of_the_following_mental_health_conditions_check_all_that_apply)*1
  merged_phq_patient_data$comorbid_anxiety <- grepl('Anxiety', merged_phq_patient_data$do_you_have_any_of_the_following_mental_health_conditions_check_all_that_apply)*1
  merged_phq_patient_data$comorbid_ocd <- grepl('OCD', merged_phq_patient_data$do_you_have_any_of_the_following_mental_health_conditions_check_all_that_apply)*1
  merged_phq_patient_data$comorbid_ptsd <- grepl('PTSD', merged_phq_patient_data$do_you_have_any_of_the_following_mental_health_conditions_check_all_that_apply)*1
  merged_phq_patient_data$comorbid_sud <- grepl('Substance Use Disorder|Alcohol Use Disorder', merged_phq_patient_data$do_you_have_any_of_the_following_mental_health_conditions_check_all_that_apply)*1
  
  ## fill diagnosis and comorbidities in merged_phq_patient_data with ICD coded diagnoses/comorbidities
  for(s in 1:nrow(icd_data)){
    
    sid <- icd_data$client_id[s]
    
    # bipolar disorder
    if( (0 %in% merged_phq_patient_data[merged_phq_patient_data$client_id==sid, 'primary_diagnosis_bipolar']) & ( icd_data[icd_data$client_id==sid, 'primary_diagnosis_bipolar'] == 1) ){
      print("adding from ICD")
      merged_phq_patient_data[merged_phq_patient_data$client_id==sid, 'primary_diagnosis_bipolar'] <- 1
    }
    
    # Anxiety
    if( (0 %in% merged_phq_patient_data[merged_phq_patient_data$client_id==sid, 'comorbid_anxiety']) & ( icd_data[icd_data$client_id==sid, 'comorbid_anxiety'] == 1) ){
      print("adding from ICD")
      merged_phq_patient_data[merged_phq_patient_data$client_id==sid, 'comorbid_anxiety'] <- 1
    }
    
    # OCD
    if( (0 %in% merged_phq_patient_data[merged_phq_patient_data$client_id==sid, 'comorbid_ocd']) & ( icd_data[icd_data$client_id==sid, 'comorbid_ocd'] == 1) ){
      print("adding from ICD")
      merged_phq_patient_data[merged_phq_patient_data$client_id==sid, 'comorbid_ocd'] <- 1
    }
    
    # PTSD
    if( (0 %in% merged_phq_patient_data[merged_phq_patient_data$client_id==sid, 'comorbid_ptsd']) & ( icd_data[icd_data$client_id==sid, 'comorbid_ptsd'] == 1) ){
      print("adding from ICD")
      merged_phq_patient_data[merged_phq_patient_data$client_id==sid, 'comorbid_ptsd'] <- 1
    }
    
    # SUD
    if( (0 %in% merged_phq_patient_data[merged_phq_patient_data$client_id==sid, 'comorbid_sud']) & ( icd_data[icd_data$client_id==sid, 'comorbid_sud'] == 1) ){
      print("adding from ICD")
      merged_phq_patient_data[merged_phq_patient_data$client_id==sid, 'comorbid_sud'] <- 1
    }
    
  }
  
  return(merged_phq_patient_data) 
  
}
  
  
