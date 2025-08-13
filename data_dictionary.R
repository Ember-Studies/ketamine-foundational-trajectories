# Script to create data dictionary

data_dictionary <- function(ember_data, path_out){
  
  ember_labels <- c(
    client_id="ID",
    phq1='PHQ item 1',
    phq2='PHQ item 2',
    phq3='PHQ item 3',
    phq4='PHQ item 4',
    phq5='PHQ item 5',
    phq6='PHQ item 6',
    phq7='PHQ item 7',
    phq8='PHQ item 8',
    phq9='PHQ item 9',
    phq_tot='PHQ total score',
    phq9_functionality='From question: have_these_problems_made_it_difficult_for_you_to_do_your_work_take_care_of_things_at_home_or_get_along_with_other_people',
    first_opened_tx = 'Date PHQ first opened. Used to create phq9_date',
    date_submitted_phq='Date of PHQ submission',
    date_of_birth_transformed='DOB',
    time_point= 'Time point linked to PHQ submission order with T1 as PHQ as highest PHQ score prior to or on day of first infusion',
    time_interval='Time since first PHQ submission (days)',
    time_since_previous = 'Time since last PHQ submission (days)',
    age_years = 'Age (years) as date of birth to current PHQ',
    first_infusion_completed = 'Date of first infusion',
    number_foundational_infusions_including_today = 'Number of foundational infusions up to and not including current PHQ submission',
    foundation_ember_recommended = 'Ember suggested treatment course',
    foundation_completion_status = 'Status of suggested treatment course',
    last_foundational_infusion = 'Date of last foundational infusion',
    first_ltc_infusion_date = 'Date of first long-term care infusion',
    foundation_outcome = 'Ember-defined clinical outcome',
    fx_therapistbinary = 'Unknown',
    primary_office = 'Primary office/clinic',
    phase_of_care = 'Current phase of care at Ember',
    intake_client_source = 'Unknown',
    sliding_scale_has_ever_qualified = 'Has the patient qualified for slideing scale treatment cost',
    date_submitted_intake = 'Date of intake package submission',
    city = 'City of residence',
    state = 'State of residence',
    zip_code = 'Zip code',
    gender_identity = 'Patient gender identity',
    pronoun_s = 'Pronouns patient identifies',
    sex_assigned_at_birth = 'Biological sex assigned at birth',
    have_you_tried_more_than_two_medications_for_your_depressive_symptoms_in_the_past_five_years = 'Has tried more than two medications in past five years',
    have_you_tried_more_than_two_medications_for_your_depressive_symptoms_in_your_lifetime = 'Has tried more than two medications in lifetime',
    have_you_tried_transcranial_magnetic_stimulation_tms = 'Has tried TMS',
    have_you_tried_electroconvulsive_therapy_ect = 'Has tried ECT',
    use_tobacco_yes = 'Uses tobacco (self-report)',
    use_cannabis_yes = 'Uses cannabis (self-report)',
    drink_alcohol_yes = 'Uses alcohol (self-report)',
    use_other_recreational_substances_yes = 'Uses recreational substances (self-report)',
    heart_disease_stroke_yes = 'Has had heart disease or stroke (self-report)',
    high_blood_pressure_yes = 'Has high blood pressure (self-report)',
    diabetes_yes = 'Has diabetes (self-report)',
    cancer_specify_type_yes = 'Has cancer (self-report)',
    depression_yes = 'Has depression (self-report)',
    anxiety_yes = 'Has anxiety (self-report)',
    other_yes = 'Has other psychiatric condition (self-report)',
    do_you_have_health_insurance = 'Patient has health insurance',
    do_you_have_out_of_network_benefits_with_your_health_insurance = 'Patient has out of network benefits with insurance',
    time_first_infusion_to_phq = 'Time from first infusion to current PHQ submission (days); can be negative when PHQ occurs before first infusion',
    time_last_foundational_infusion_to_phq = 'Time final infusion in foundational series to current PHQ (days); can be negative or positive',
    comorbid_anxiety = 'Comorbid anxiety (1=Yes/0=No)',
    comorbid_ocd = 'Comorbid obsessive-compulsive disorder (1=Yes/0=No)',
    comorbid_ptsd = 'Comorbid post-traumatic stress disorder disorder (1=Yes/0=No)',
    comorbid_sud = 'Comorbid substance use or alcohol use disorder (1=Yes/0=No)',
    primary_diagnosis_bipolar = '1=Bipolar Disorder, 0=Major Depressive Disorder',
    age_category_adolescent = 'Age < 18 years',
    age_category_adult = 'Age >= 25 and < 65 years',
    age_category_senior = 'Age >= 65 years',
    age_bin = 'Binned age category',
    ketamine_dose_mg_kg = 'Current ketamine dose in mg/kg',
    infusions_before_phq = 'Number of infusions before current PHQ submission not including day of current PHQ submission',
    antidepressant_load = 'Count of number of antidepressants currently used',
    antipsychotic_load = 'Count of number of antipsychotics currently used',
    benzodiazepine_load = 'Count of number of benzodiazepines currently used',
    mood_stabilizer_load = 'Count of number of mood stabilizers currently used',
    non_benzodiazepine_anxiolytic_sedative_load = 'Count of number of non-benzodiazepine anxiolytic sedatives currently used',
    stimulant_load = 'Count of number of stimulants currently used',
    other_psychotropic_medication_load = 'Count of number of other psychotropic medications not in other categories',
    total_psychotropic_medication_load = 'Count of number of all psychotropic medications currently used (sum across previous classes)',
    total_non_psychotropic_medication_load = 'Count of number of all non psychotropic medications',
    per_protocol = 'Per-protocol patients (1=PP/0=ITT conditioned on inclusion of "Yes-Full Foundation" argument): Those with foundation_ember_recommended = "Yes - Full Foundation, date(last_foundational_infusion) - date(first_infusion_completed)) <=14 days, and number_foundational_infusions_including_today <= 4'
  )
  
  # check all variables are labeled
  names(ember_data)[!names(ember_data) %in% names(ember_labels)]
  names(ember_labels)[!names(ember_labels) %in% names(ember_data)]
  
  # create data dictionary
  ember_data_for_dictionary <- ember_data[, !names(ember_data) %in% grep('medication_use', names(ember_data), value = T)]
  dic <- create_dictionary(ember_data_for_dictionary, var_labels = ember_labels)
  
  # write dictionary
  write.csv(dic, file = paste0(path_out, 'ember_data_dictionary.csv'), quote = FALSE, row.names = FALSE)

}