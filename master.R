## Script to prepare data from Ember Health for Study 1 using linear mixed effects models to characterize symptom changes over foundational ketamine series

# load packages
require(tidyverse)
require(dplyr)
require(janitor)
require(lubridate)
require(survival)
require(datadictionary)
require(tidystringdist)
require(rairtable)
require(writexl)

# set paths
path_code <- '/path/to/code/' # path to formatting scripts
path_data <- '/path/to/data/' # path to csv files
path_out <- '/path/for/output/' # path to save output

# set airtable access token
token <- readLines('/path/to/token.txt') # read txt file that contains token
set_airtable_api_key(token, install = FALSE)

# source functions
source(paste0(path_code, 'format_patient_airtable.R'))
source(paste0(path_code, 'format_intake_tables.R'))
source(paste0(path_code, 'merge_patient_airtable_intake_data.R'))
source(paste0(path_code, 'format_merge_phq_data.R'))
source(paste0(path_code, 'add_transform_variables.R'))
source(paste0(path_code, 'fill_missing.R'))
source(paste0(path_code, 'cleanup.R'))
source(paste0(path_code, 'data_dictionary.R'))

# Format base patient table
airtable_data_filtered <- format_patient_airtable(path_data = path_data, date_ceiling_ymd = "2025-01-01")

# Format intake table
intake_data_filtered <- format_intake_tables(path_data = path_data)

# Merge airtable and intake data
merged_patient_airtable_intake_data <- merge_patient_airtable_intake_data(airtable_data_filtered = airtable_data_filtered,
                                                                         intake_data_filtered = intake_data_filtered)

# Format PHQ data and merge with patient data
merged_phq_patient_data <- format_merge_phq_data(merged_patient_airtable_intake_data = merged_patient_airtable_intake_data,
                                                 path_data = path_data, 
                                                 time_pre = -30, 
                                                 time_post = 30)

# Add and format variables
ember_data <- add_transform_variables(merged_phq_patient_data = merged_phq_patient_data, 
                                      path_data = path_data, 
                                      path_code = path_code)

# Find replacements for missing variables
ember_data <- fill_missing(ember_data = ember_data, 
                           variable = 'sex_assigned_at_birth', 
                           path_data = path_data)

# Clean variables
ember_data <- cleanup(ember_data = ember_data)

# Write data dictionary
data_dictionary(ember_data = ember_data, 
                path_out = path_out)

# Save
save(ember_data, file = sprintf('%s/ember_data_formatted_%s.Rdata', path_out, gsub(" ", "_", date())))
write_xlsx(ember_data, sprintf('%s/ember_data_formatted_%s.xlsx', path_out, gsub(" ", "_", date())))




