## Note, data from intake tables is now combined in "MGH - All Intake Data - Combined.csv"

format_intake_tables <- function(path_data){

  # read combined intake form
  intake_data <- read.csv(paste0(path_data, 'MGH - All Intake Data - Combined.csv'))
  
  # clean names
  intake_data <- intake_data %>%
    janitor::clean_names()
  
  # fix variable names
  intake_data <- intake_data %>%
    rename('date_submitted'='date_submited',
           'have_you_tried_more_than_two_medications_for_your_depressive_symptoms_in_the_past_five_years'='have_you_tried_more_than_2_medications_for_your_depressive_symptoms_in_the_past_5_years',
           'have_you_tried_more_than_two_medications_for_your_depressive_symptoms_in_your_lifetime'='have_you_tried_more_than_2_medications_for_your_depressive_symptoms_in_your_lifetime')
  
  # fix medication names
  names(intake_data) <- gsub('^.*?([0-9]+)', 'medication_\\1', names(intake_data))
  
  ## TBD: let's use pattern matching here to create indicator variables for key medications
  
  ## get missing sex info from All Appointments file
  all_appointments <- read.csv(paste0(path_data, 'MGH - List - All Appointments - 2025Data.csv'))
  
  # clean names
  all_appointments <- all_appointments %>%
    janitor::clean_names()
  
  # get ids of patients with missing sex
  ids_sex_missing <- intake_data$client_id[which(intake_data$sex_assigned_at_birth=="")]
  
  for(s in ids_sex_missing){
    
    # find/filter sex from all appointments
    replacement <- all_appointments[all_appointments$client_id==s, 'gender']
    if(length(replacement)==0){
      replacement <- ""
    }else if(unique(replacement)==""){
     replacement="" 
    }else{
      replacement <- replacement[!replacement==""]
      replacement <- names(which.max(table(replacement))) # in case multiple reports, take most frequent 
    }
    
    # replace
    intake_data[intake_data$client_id==s, 'sex_assigned_at_birth'] <- replacement
    
  }
  
  ## TBD: we can fill in some gender identity from other variables but this changes the variable from sex to gender identity / messier
  # ids_sex_missing <- intake_data$client_id[which(intake_data$sex_assigned_at_birth=="")]
  # intake_data[intake_data$client_id %in% ids_sex_missing, 'gender_identity']
  # intake_data[intake_data$client_id %in% ids_sex_missing, 'pronoun_s']
  
  
  # select relevant variables
  intake_data_filtered <- intake_data %>%
    dplyr::select(client_id, date_submitted, date_of_birth, city, state, zip_code, gender_identity,
           pronoun_s, sex_assigned_at_birth, do_you_have_any_of_the_following_mental_health_conditions_check_all_that_apply,
           have_you_tried_more_than_two_medications_for_your_depressive_symptoms_in_the_past_five_years,
           have_you_tried_more_than_two_medications_for_your_depressive_symptoms_in_your_lifetime,
           have_you_tried_transcranial_magnetic_stimulation_tms, have_you_tried_electroconvulsive_therapy_ect,
           do_you_have_mental_health_professionals_helping_you_at_this_time_please_provide_full_names,
           medication_1_medication_name, medication_2_medication_name, medication_3_medication_name, 
           medication_4_medication_name, medication_5_medication_name, medication_6_medication_name, 
           medication_7_medication_name, medication_8_medication_name, medication_9_medication_name, 
           medication_10_medication_name, use_tobacco_yes, use_cannabis_yes, drink_alcohol_yes, 
           use_other_recreational_substances_yes, heart_disease_stroke_yes, high_blood_pressure_yes,
           diabetes_yes, cancer_specify_type_yes, depression_yes, anxiety_yes, other_yes, 
           do_you_have_health_insurance, do_you_have_out_of_network_benefits_with_your_health_insurance)
  
  return(intake_data_filtered)
  
}
