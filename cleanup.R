# Drops some unneeded variables and recodes Yes/No/"" responses to 1/0/NA

cleanup <- function(ember_data){
  
  # drop unwanted variables
  ember_data <- ember_data %>%
    dplyr::select(-contains('_medication_name')) %>%
    dplyr::select(-time_point_uncorrected, -do_you_have_any_of_the_following_mental_health_conditions_check_all_that_apply,
                  -do_you_have_mental_health_professionals_helping_you_at_this_time_please_provide_full_names,
                  -date_submited_tx, -fx_historical_psychiatrist)
  
  # rename variables
  ember_data <- ember_data %>%
    dplyr::rename('number_of_foundational_infusions_including_today'='x_foundational_infusions_including_today')
  
  
  # function to handle strings with missings
  str_to_numeric_yn <- function(tmp){
    tmp[tmp==""] <- NA
    tmp[tmp=="Yes"] <- 1
    tmp[tmp=="No"] <- 0
    tmp <- as.numeric(tmp)
    return(tmp)
  }
  
  # convert Yes/No/"" responses to 1/0/NA
  ember_data$have_you_tried_more_than_two_medications_for_your_depressive_symptoms_in_the_past_five_years <- str_to_numeric_yn(ember_data$have_you_tried_more_than_two_medications_for_your_depressive_symptoms_in_the_past_five_years)
  ember_data$have_you_tried_more_than_two_medications_for_your_depressive_symptoms_in_your_lifetime <- str_to_numeric_yn(ember_data$have_you_tried_more_than_two_medications_for_your_depressive_symptoms_in_your_lifetime)
  ember_data$have_you_tried_transcranial_magnetic_stimulation_tms <- str_to_numeric_yn(ember_data$have_you_tried_transcranial_magnetic_stimulation_tms)
  ember_data$have_you_tried_electroconvulsive_therapy_ect <- str_to_numeric_yn(ember_data$have_you_tried_electroconvulsive_therapy_ect)
  ember_data$use_tobacco_yes <- str_to_numeric_yn(ember_data$use_tobacco_yes)
  ember_data$use_cannabis_yes <- str_to_numeric_yn(ember_data$use_cannabis_yes)
  ember_data$drink_alcohol_yes <- str_to_numeric_yn(ember_data$drink_alcohol_yes)
  ember_data$use_other_recreational_substances_yes <- str_to_numeric_yn(ember_data$use_other_recreational_substances_yes)
  ember_data$heart_disease_stroke_yes <- str_to_numeric_yn(ember_data$heart_disease_stroke_yes)
  ember_data$high_blood_pressure_yes <- str_to_numeric_yn(ember_data$high_blood_pressure_yes)
  ember_data$diabetes_yes <- str_to_numeric_yn(ember_data$diabetes_yes)
  ember_data$cancer_specify_type_yes <- str_to_numeric_yn(ember_data$cancer_specify_type_yes)
  ember_data$depression_yes <- str_to_numeric_yn(ember_data$depression_yes)
  ember_data$anxiety_yes <- str_to_numeric_yn(ember_data$anxiety_yes)
  ember_data$other_yes <- str_to_numeric_yn(ember_data$other_yes)
  ember_data$do_you_have_health_insurance <- str_to_numeric_yn(ember_data$do_you_have_health_insurance)
  ember_data$do_you_have_out_of_network_benefits_with_your_health_insurance <- str_to_numeric_yn(ember_data$do_you_have_out_of_network_benefits_with_your_health_insurance)
 
  return(ember_data)
  
}  
  
  
  
