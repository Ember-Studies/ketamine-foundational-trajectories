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
    comorbid_sud = 'Comorbid substance use disorder (1=Yes/0=No)',
    comorbid_aud = 'Comorbid alcohol use disorder (1=Yes/0=No)',
    primary_dx_bipolar = 'Bipolar disorder diagnosis (1=Yes/0=No)',
    primary_dx_depression_mdd = 'Major depressive disorder diagnosis (1=Yes/0=No)',
    primary_dx_depression_other = 'Other depression diagnosis (1=Yes/0=No)',
    comorbid_other_condition = 'Diagnosis of another psychiatric condition (1=Yes/0=No)',
    age_category_adolescent = 'Age < 18 years',
    age_category_adult = 'Age >= 25 and < 65 years',
    age_category_senior = 'Age >= 65 years',
    age_bin = 'Binned age category',
    ketamine_dose_mg_kg = 'Current ketamine dose in mg/kg',
    infusions_before_phq = 'Number of infusions before current PHQ submission not including day of current PHQ submission',
    medication_use_amitriptyline_antidepressant = 'Use amitriptyline (Yes=1)',
    medication_use_bupropion_antidepressant = 'Use bupropion (Yes=1)',
    medication_use_bupropion_dextromethorphan_antidepressant = 'Use bupropion dextromethorphan (Yes=1)',
    medication_use_citalopram_antidepressant = 'Use citalopram (Yes=1)',
    medication_use_clomipramine_antidepressant = 'Use clomipramine (Yes=1)',
    medication_use_desvenlafaxine_antidepressant = 'Use desvenlafaxine (Yes=1)',
    medication_use_doxepin_antidepressant = 'Use doxepin (Yes=1)',
    medication_use_duloxetine_antidepressant = 'Use duloxetine (Yes=1)',
    medication_use_escitalopram_antidepressant = 'Use escitalopram (Yes=1)',
    medication_use_fluoxetine_antidepressant = 'Use fluoxetine (Yes=1)',
    medication_use_fluvoxamine_antidepressant = 'Use fluvoxamine (Yes=1)',
    medication_use_isocarboxazid_antidepressant = 'Use isocarboxazid (Yes=1)',
    medication_use_levomilnacipram_antidepressant = 'Use levomilnacipram (Yes=1)',
    medication_use_mirtazapine_antidepressant = 'Use mirtazapine (Yes=1)',
    medication_use_nortriptyline_antidepressant = 'Use nortriptyline (Yes=1)',
    medication_use_paroxetine_antidepressant = 'Use paroxetine (Yes=1)',
    medication_use_phenelzine_antidepressant = 'Use phenelzine (Yes=1)',
    medication_use_selegiline_antidepressant = 'Use selegiline (Yes=1)',
    medication_use_sertraline_antidepressant = 'Use sertraline (Yes=1)',
    medication_use_tranylcypromine_antidepressant = 'Use tranylcypromine (Yes=1)',
    medication_use_trazodone_antidepressant = 'Use trazodone (Yes=1)',
    medication_use_venlafaxine_antidepressant = 'Use venlafaxine (Yes=1)',
    medication_use_vilazodone_antidepressant = 'Use vilazodone (Yes=1)',
    medication_use_vortioxetine_antidepressant = 'Use vortioxetine (Yes=1)',
    medication_use_aripiprazole_antipsychotic = 'Use aripiprazole (Yes=1)',
    medication_use_asenapine_antipsychotic = 'Use asenapine (Yes=1)',
    medication_use_brexpiprazole_antipsychotic = 'Use brexpiprazole (Yes=1)',
    medication_use_cariprazine_antipsychotic = 'Use cariprazine (Yes=1)',
    medication_use_chlorpromazine_antipsychotic = 'Use chlorpromazine (Yes=1)',
    medication_use_clozapine_antipsychotic = 'Use clozapine (Yes=1)',
    medication_use_fluphenazine_antipsychotic = 'Use fluphenazine (Yes=1)',
    medication_use_haloperidol_antipsychotic = 'Use haloperidol (Yes=1)',
    medication_use_loxapine_antipsychotic = 'Use loxapine (Yes=1)',
    medication_use_lurasidone_antipsychotic = 'Use lurasidone (Yes=1)',
    medication_use_olanzapine_antipsychotic = 'Use olanzapine (Yes=1)',
    medication_use_olanzapine_fluoxetine_antipsychotic = 'Use olanzapine fluoxetine (Yes=1)',
    medication_use_olanzapine_samidorphan_antipsychotic = 'Use olanzapine samidorphan (Yes=1)',
    medication_use_paliperidone_antipsychotic = 'Use paliperidone (Yes=1)',
    medication_use_perphenazine_antipsychotic = 'Use perphenazine (Yes=1)',
    medication_use_quetiapine_antipsychotic = 'Use quetiapine (Yes=1)',
    medication_use_risperidone_antipsychotic = 'Use risperidone (Yes=1)',
    medication_use_thioridazine_antipsychotic = 'Use thioridazine (Yes=1)',
    medication_use_ziprasidone_antipsychotic = 'Use ziprasidone (Yes=1)',
    medication_use_alprazolam_benzodiazepine = 'Use alprazolam (Yes=1)',
    medication_use_chlordiazepoxide_benzodiazepine = 'Use chlordiazepoxide (Yes=1)',
    medication_use_clonazepam_benzodiazepine = 'Use clonazepam (Yes=1)',
    medication_use_diazepam_benzodiazepine = 'Use diazepam benzodiazepine (Yes=1)',
    medication_use_halazepam_benzodiazepine = 'Use halazepam (Yes=1)',
    medication_use_lorazepam_benzodiazepine = 'Use lorazepam benzodiazepine (Yes=1)',
    medication_use_oxazepam_benzodiazepine = 'Use oxazepam (Yes=1)',
    medication_use_prazepam_benzodiazepine = 'Use prazepam (Yes=1)',
    medication_use_temazepam_benzodiazepine = 'Use temazepam (Yes=1)',
    medication_use_triazolam_benzodiazepine = 'Use triazolam (Yes=1)',
    medication_use_carbamazepine_mood_stabilizer = 'Use carbamazepine (Yes=1)',
    medication_use_divalproex_sodium_mood_stabilizer = 'Use divalproex sodium (Yes=1)',
    medication_use_lamotrigine_mood_stabilizer = 'Use lamotrigine (Yes=1)',
    medication_use_lithium_carbonate_mood_stabilizer = 'Use lithium carbonate (Yes=1)',
    medication_use_oxcarbazepine_mood_stabilizer = 'Use oxcarbazepine (Yes=1)',
    medication_use_topiramate_mood_stabilizer = 'Use topiramate (Yes=1)',
    medication_use_buspirone_non_benzodiazepine_anxiolytic_sedative = 'Use buspirone (Yes=1)',
    medication_use_eszoplicone_non_benzodiazepine_anxiolytic_sedative = 'Use eszoplicone (Yes=1)',
    medication_use_gabapentin_non_benzodiazepine_anxiolytic_sedative = 'Use gabapentin (Yes=1)',
    medication_use_suvorexant_non_benzodiazepine_anxiolytic_sedative = 'Use suvorexant (Yes=1)',
    medication_use_zolpidem_non_benzodiazepine_anxiolytic_sedative = 'Use zolpidem (Yes=1)',
    medication_use_acamprosate_other_psychotropic_medication = 'Use acamprosate (Yes=1)',
    medication_use_buprenorphine_other_psychotropic_medication = 'Use buprenorphine (Yes=1)',
    medication_use_methadone_other_psychotropic_medication = 'Use methadone (Yes=1)',
    medication_use_naloxone_other_psychotropic_medication = 'Use naloxone (Yes=1)',
    medication_use_naltrexone_other_psychotropic_medication = 'Use naltrexone (Yes=1)',
    medication_use_varenicline_other_psychotropic_medication = 'Use varenicline (Yes=1)',
    medication_use_amphetamine_dextroamphetamine_stimulant = 'Use amphetamine dextroamphetamine (Yes=1)',
    medication_use_atomoxetine_stimulant = 'Use atomoxetine (Yes=1)',
    medication_use_dexmethylphenidate_stimulant = 'Use dexmethylphenidate (Yes=1)',
    medication_use_lisdexamfetamine_stimulant = 'Use lisdexamfetamine (Yes=1)',
    medication_use_methylphenidate_stimulant = 'Use methylphenidate (Yes=1)',
    medication_use_methylphenidate_extended_release_stimulant = 'Use methylphenidate extended release (Yes=1)',
    medication_use_modafinil_stimulant = 'Use modafinil (Yes=1)',
    antidepressant_load = 'Count of number of antidepressants currently used',
    antipsychotic_load = 'Count of number of antipsychotics currently used',
    benzodiazepine_load = 'Count of number of benzodiazepines currently used',
    mood_stabilizer_load = 'Count of number of mood stabilizers currently used',
    non_benzodiazepine_anxiolytic_sedative_load = 'Count of number of non-benzodiazepine anxiolytic sedatives currently used',
    stimulant_load = 'Count of number of stimulants currently used',
    total_medication_load = 'Count of number of all medications currently used (sum across previous classes)'
  )
  
  # check all variables are labeled
  names(ember_data)[!names(ember_data) %in% names(ember_labels)]
  names(ember_labels)[!names(ember_labels) %in% names(ember_data)]
  
  # create data dictionary
  dic <- create_dictionary(ember_data, var_labels = ember_labels)
  
  # write dictionary
  write.csv(dic, file = paste0(path_out, 'ember_data_dictionary.csv'), quote = FALSE, row.names = FALSE)

}