require(tidyverse)
require(dplyr)
require(janitor)
require(lubridate)
require(rairtable)
require(readxl)

path_code <- '/path/to/code/'
path_data <- '/path/to/data/'
path_out <- '/path/to/output/'

# get formatted data from 'MGH - Infusion Visit Notes - 2025data.xlsx' and airtable (Visits) pull
source(paste0(path_code, "format_dose.R"))
infusion_notes_all <- format_dose(path_data = path_data, plot_hist = FALSE)

## read visit notes from prior version
visits_dose <- read.csv(paste0(path_data, 'Visits-Dose View prior 2025.csv'))

visits_dose <- visits_dose %>%
  clean_names() %>%
  rename("client_id"="intake_q_id")

# format dates
visits_dose$visit_date_and_time <- lubridate::mdy_hm(visits_dose$visit_date_and_time)
visits_dose$visit_date_and_time <- lubridate::mdy(format(visits_dose$visit_date_and_time, "%m/%d/%Y")) # drops HM
visits_dose$weight_date_taken <- lubridate::mdy(visits_dose$weight_date_taken)

# find missing dose and weight indices
idx_missing_dose <- which(is.na(visits_dose$ketamine_dose_mg))
idx_missing_weight <- which(is.na(visits_dose$weight_kg))

# are missings the same? Yes
length(setdiff(idx_missing_dose, idx_missing_weight)) == 0

# loop over indices of missing data and replace 
for(index_visit_dose in idx_missing_dose){
  
  print(index_visit_dose)
  
  cid <- visits_dose[index_visit_dose, 'client_id']
  
  # draw from merged data
  ss <- infusion_notes_all[infusion_notes_all$client_id == cid, ]
  
  # calculate difference between dates (days) to allow for tolorance/fuzzy matching
  #visits_dose[index_visit_dose, 'visit_date_and_time'] - ss$date_ketamine
  
  match_index <- which(visits_dose[index_visit_dose, 'visit_date_and_time'] == ss$date_ketamine)
  
  if(length(match_index)==0){
    print('No exact matching dates. Skipping.')
    next
  }
  
  if(length(match_index) > 1){
    print('More than one match. Using first record.')
    match_index <- match_index[1]
  }
  
  # replace ketamine dose (mg)
  if( (is.na(visits_dose[index_visit_dose, 'ketamine_dose_mg']) ) & (!is.na(ss[match_index, 'ketamine_dose_mg']) )){
    
    visits_dose[index_visit_dose, 'ketamine_dose_mg'] <- ss[match_index, 'ketamine_dose_mg']
    
  }
  
  # replace patient weight (kg)
  if( (is.na(visits_dose[index_visit_dose, 'weight_kg']) ) & (!is.na(ss[match_index, 'patient_weight_kg']) )){
    
    visits_dose[index_visit_dose, 'weight_kg'] <- ss[match_index, 'patient_weight_kg']
    
  }
  
}

# save updated
write.csv(visits_dose, file = paste0(path_out, 'Visits-Dose_View_prior_2025_Updated.csv'), row.names = FALSE, quote = FALSE)




