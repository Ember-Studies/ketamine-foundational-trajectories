format_patient_airtable <- function(path_data, accstoken){
  
  # load the patient table
  tablename <- 'Patients'  # available tables are Patients, Providers, Visits and Events
  
  # establish connection and read table
  testtable <- airtable(tablename, 'appRioSAcdOonQ8RX')
  airtable_data <- read_airtable(testtable, id_to_col = TRUE)
  
  # clean names
  airtable_data <- airtable_data %>%
    janitor::clean_names()
  
  # select relevant variables
  airtable_data_filtered <- airtable_data %>%
    dplyr::select(intake_q_id, first_infusion_completed, foundation_ember_recommended, 
           foundation_completion_status, last_foundational_infusion, 
           number_foundational_infusions_including_today, foundation_outcome,
           fx_therapistbinary, fx_historical_psychiatrist, primary_office,
           phase_of_care, intake_client_source, sliding_scale_has_ever_qualified)
  
  # Field “First Infusion (Completed)” is NOT EMPTY 
  airtable_data_filtered <- airtable_data_filtered[airtable_data_filtered$first_infusion_completed != "", ]
  
  # Field “Foundation: Ember Recommended” = “Yes - Full Foundation”
  airtable_data_filtered <- airtable_data_filtered[airtable_data_filtered$foundation_ember_recommended == "Yes - Full Foundation", ]
  
  return(airtable_data_filtered)
  
}