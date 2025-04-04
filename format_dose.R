format_dose <- function(path_data){

  require(readxl)
  
  # load dose data
  dose_data <- read.csv(paste0(path_data, 'Visits-All Data.csv'))
  
  # clean names
  dose_data <- dose_data %>%
    janitor::clean_names() %>%
    rename('client_id'='intake_q_id') %>%
    as.data.frame()
  
  # note this airtable export is missing most info needed for mg/kg so dose will be computed with intake data
  summary(dose_data$weight_kg)
  
  # find recorded dose data: this should be valid through mid-2021
  dose_data_through_2021 <- dose_data[!is.na(dose_data$ketamine_dose_mg_kg), ]
  
  # for dose data post mid-2021, need to merge "_MGH - Infusion Visit Notes" and "Airtable Visit table"
  # "_MGH - Infusion Visit Notes" Is an excel file, has 2 tables
  # - "Current" is the current note system, which started on 10/19/2023
  # - "Prior Version" is one version old note system, started on 1/25/2021 and ran to 10/19/2023
  # - Different naming conventions
  # current: final_ketamine_dose_current_visit_mg, date_of_ketamine_administration
  # prior: final_ketamine_order_for_current_infusion_mg, date_of_current_infusion
  
  # load current and prior infusion notes
  infusion_notes_current <- read_xlsx(paste0(path_data, 'MGH - Infusion Visit Notes - 2025data.xlsx'), sheet = 'Current', col_types = c('text'))
  infusion_notes_prior <- read_xlsx(paste0(path_data, 'MGH - Infusion Visit Notes - 2025data.xlsx'), sheet = 'Prior Version', col_types = c('text'))
  
  # clean names
  infusion_notes_current <- infusion_notes_current %>%
    janitor::clean_names() %>%
    rename('ketamine_dose_mg'='final_ketamine_dose_current_visit_mg', 
           'date_ketamine'='date_of_ketamine_administration',
           'date_weight'='date_of_weight_check_most_recent')
  
  infusion_notes_prior <- infusion_notes_prior %>%
    janitor::clean_names() %>%
    rename('ketamine_dose_mg'='final_ketamine_order_for_current_infusion_mg',
           'date_ketamine'='date_of_current_infusion',
           'date_weight'='date_of_weight_check',
           'date'='date_3') # date_3 vs date_24??
  
  # drop rows with missing client ids
  infusion_notes_current <- infusion_notes_current[!is.na(infusion_notes_current$client_id), ]
  infusion_notes_prior <- infusion_notes_prior[!is.na(infusion_notes_prior$client_id), ]
  
  # client_id to numeric
  infusion_notes_current$client_id <- as.numeric(infusion_notes_current$client_id)
  infusion_notes_prior$client_id <- as.numeric(infusion_notes_prior$client_id)
  
  # fix date formats in current version: has mix of string types
  inc_dates_iso <- infusion_notes_current[grep('Z', infusion_notes_current$date_weight, value = F), ]
  inc_dates_ymd <- infusion_notes_current[grep('Z', infusion_notes_current$date_weight, value = F, invert = T), ]
  
  inc_dates_iso$date_weight <- as.Date(inc_dates_iso$date_weight) # parses as ymd
  inc_dates_ymd$date_weight <- convert_to_date(inc_dates_ymd$date_weight) # parses as ymd
  
  # combine 
  infusion_notes_current_fixed_dates <- rbind(inc_dates_iso, inc_dates_ymd)
  
  # fix date formats in prior version: has mix of string types
  inp_dates_iso <- infusion_notes_prior[grep('Z', infusion_notes_prior$date_weight, value = F), ]
  inp_dates_ymd <- infusion_notes_prior[grep('Z', infusion_notes_prior$date_weight, value = F, invert = T), ]
  
  inp_dates_iso$date_weight <- as.Date(inp_dates_iso$date_weight) # parses as ymd
  inp_dates_ymd$date_weight <- convert_to_date(inp_dates_ymd$date_weight) # parses as ymd
  
  # combine 
  infusion_notes_prior_fixed_dates <- rbind(inp_dates_iso, inp_dates_ymd)
  
  # pull relevant variables
  infusion_notes_current_fixed_dates <- infusion_notes_current_fixed_dates %>% 
    dplyr::select('client_id', 'date', 'date_ketamine', 'date_weight', 
           'patient_weight_kg', 'ketamine_dose_mg') %>%
    as.data.frame()
  
  infusion_notes_prior_fixed_dates <- infusion_notes_prior_fixed_dates %>% 
    dplyr::select('client_id', 'date', 'date_ketamine', 'date_weight', 
           'patient_weight_kg', 'ketamine_dose_mg') %>%
    as.data.frame()
  
  # merge infusion notes
  infusion_notes_merged <- rbind(infusion_notes_prior_fixed_dates, infusion_notes_current_fixed_dates)
  
  # fix dates: dates show as floats again after merge
  infusion_notes_merged$date <- convert_to_date(infusion_notes_merged$date)
  infusion_notes_merged$date_ketamine <- convert_to_date(infusion_notes_merged$date_ketamine)
  infusion_notes_merged$date_weight <- convert_to_date(infusion_notes_merged$date_weight)
  
  # fix client id: float to integer
  #infusion_notes_merged$client_id <- as.integer(infusion_notes_merged$client_id)
  
  # parse patient_weight_kg string
  # - note: some weights reported as mg but are in expected kg range (typo?)
  # - some reported as range a - b
  # - some extreneous text
  # - some with "reweight"; should these be dropped?
  
  infusion_notes_merged$patient_weight_kg <- as.numeric(gsub("[^0-9.]", "", infusion_notes_merged$patient_weight_kg))
  infusion_notes_merged$patient_weight_kg[infusion_notes_merged$patient_weight_kg>300] <- NA
  infusion_notes_merged$patient_weight_kg[infusion_notes_merged$patient_weight_kg==0] <- NA
  
  # parse ketamine_dose_mg
  ss <- gsub(" = [0-9].[0-9]+ mg/kg", "", infusion_notes_merged$ketamine_dose_mg)
  ss <- gsub("= [0-9].[0-9]+mg/kg", "", ss)
  ss <- gsub("\\s*\\([^\\)]+\\)", "", ss)
  ss <- gsub(".*-->", "", ss)
  ss <- gsub("(.*?)mg.*", "\\1", ss)
  ss <- gsub("(.*?)=*", "\\1", ss)
  ss <- gsub(" .*", "", ss)
  ss <- gsub("[^0-9.]", "", ss)
  ss <- gsub("\\.\\.", ".", ss)
  
  dose <- as.numeric(ss)
  
  hist(dose)
  
  # reassign formatted dose
  infusion_notes_merged$ketamine_dose_mg <- dose
  
  hist(infusion_notes_merged$ketamine_dose_mg / infusion_notes_merged$patient_weight_kg)
  summary(infusion_notes_merged$ketamine_dose_mg / infusion_notes_merged$patient_weight_kg)
  
  # format dose data prior to mid-2021
  dose_data_through_2021 <- dose_data_through_2021 %>%
    rename('date'='date_time_created',
           'date_ketamine'='visit_date_and_time',
           'date_weight'='weight_date_taken',
           'patient_weight_kg'='weight_kg') %>%
    dplyr::select(client_id, date, date_ketamine, date_weight, 
           patient_weight_kg, ketamine_dose_mg, ketamine_dose_mg_kg)
  
  # format dates for pre 2021 data to ymd
  dose_data_through_2021$date_ketamine <- gsub(" .*", "", dose_data_through_2021$date_ketamine)
  dose_data_through_2021$date_ketamine <- format(as.Date(dose_data_through_2021$date_ketamine, '%m/%d/%Y'), '%Y-%m-%d')
  dose_data_through_2021$date_ketamine <- ymd(dose_data_through_2021$date_ketamine)
  
  dose_data_through_2021$date_weight <- gsub(" .*", "", dose_data_through_2021$date_weight)
  dose_data_through_2021$date_weight <- format(as.Date(dose_data_through_2021$date_weight, '%m/%d/%Y'), '%Y-%m-%d')
  dose_data_through_2021$date_weight <- ymd(dose_data_through_2021$date_weight)
  
  dose_data_through_2021$date <- gsub(" .*", "", dose_data_through_2021$date)
  dose_data_through_2021$date <- format(as.Date(dose_data_through_2021$date, '%m/%d/%Y'), '%Y-%m-%d')
  dose_data_through_2021$date <- ymd(dose_data_through_2021$date)
  
  # # fill missing patient weights with forward imputation grouped by subject and sorted by date (~1200 records fixed)
  # infusion_notes_merged <- infusion_notes_merged %>%
  #   group_by(client_id) %>%
  #   arrange(client_id, date) %>%
  #   fill(patient_weight_kg) %>%
  #   mutate(ketamine_dose_mg_kg = ketamine_dose_mg / patient_weight_kg) %>%
  #   as.data.frame()
  
  # calculate ketamine dose mg/kg for newer data
  # infusion_notes_merged$ketamine_dose_mg_kg <- infusion_notes_merged$ketamine_dose_mg / infusion_notes_merged$patient_weight_kg
  
  infusion_notes_merged <- infusion_notes_merged %>%
    group_by(client_id) %>%
    arrange(client_id, date) %>%
    fill(patient_weight_kg, .direction = 'downup') %>%
    as.data.frame()
  
  # for remaining missing weight, see if its in old dose data
  missing_weights <- which(is.na(infusion_notes_merged$patient_weight_kg))
  
  for(w in missing_weights){
    
    # find weights from earlier dataset 
    weights_recovered <- dose_data_through_2021[dose_data_through_2021$client_id==infusion_notes_merged$client_id[w], 'patient_weight_kg']
    
    if(length(weights_recovered)==0){
      next
    }
    
    # replace missing weight for current subject if is.na with last instance of weight recorded from earlier version 
    infusion_notes_merged[infusion_notes_merged$client_id == infusion_notes_merged$client_id[w], 'patient_weight_kg'][is.na(infusion_notes_merged[infusion_notes_merged$client_id == infusion_notes_merged$client_id[w], 'patient_weight_kg'])] <- weights_recovered[length(weights_recovered)]
    
  }
  
  # calculate dose (mg/kg) for infusion_notes_merged
  infusion_notes_merged$ketamine_dose_mg_kg <- infusion_notes_merged$ketamine_dose_mg / infusion_notes_merged$patient_weight_kg
  
  # merge old and new data
  infusion_notes_all <- rbind(dose_data_through_2021, infusion_notes_merged)

  return(infusion_notes_all)
  
}











