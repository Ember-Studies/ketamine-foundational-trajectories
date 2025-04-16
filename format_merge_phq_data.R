# Time pre: days before/including first infusion (should be passed as negative value)
# Time post: days after last infusion

format_merge_phq_data <- function(merged_patient_airtable_intake_data, path_data, time_pre, time_post, plot_hist = FALSE){
  
  # read PHQ data 
  phq <- read.csv(paste0(path_data, 'MGH - All PHQ9 Data - 2025Data.csv'))
  
  # clean names
  phq <- phq %>%
    janitor::clean_names()
  
  # rename
  phq <- phq %>%
    rename('phq1'='little_interest_or_pleasure_in_doing_things',
           'phq2'='feeling_down_depressed_or_hopeless',
           'phq3'='trouble_falling_or_staying_asleep_or_sleeping_too_much',
           'phq4'='feeling_tired_or_having_little_energy',
           'phq5'='poor_appetite_or_overeating',
           'phq6'='feeling_bad_about_yourself_or_that_you_are_a_failure_or_have_let_yourself_or_your_family_down',
           'phq7'='trouble_concentrating_on_things_such_as_reading_the_newspaper_or_watching_television',
           'phq8'='moving_or_speaking_so_slowly_that_other_people_could_have_noticed_or_so_fidgety_or_restless_that_you_have_been_moving_a_lot_more_than_usual',
           'phq9'='thoughts_that_you_would_be_better_off_dead_or_thoughts_of_hurting_yourself_in_some_way',
           'phq_tot'='phq9total')
  
  # transform date of birth
  phq$date_of_birth_tx <- lubridate::mdy(phq$date_of_birth)
  
  # transform date submitted
  phq$date_submited_tx <- lubridate::ymd(phq$date_submited)
  
  # transform first opened date
  phq$first_opened_tx <- as.Date(lubridate::mdy_hms(phq$first_opened))
  
  # make phq_9_date variable from first opened and secondarily date submitted
  phq$phq9_date <- phq$first_opened_tx
  index_phq9_date_missing <- which(is.na(phq$phq9_date))
  phq$phq9_date[index_phq9_date_missing] <- phq$date_submited_tx[index_phq9_date_missing]
  
  # read in all clients csv for corrected dobs
  data_all_clients <- read.csv(paste0(path_data, 'MGH - List - All Clients - 2025Data.csv'))
  
  # clean names
  data_all_clients <- data_all_clients %>%
    janitor::clean_names()
  
  # transform all clients dob
  data_all_clients$date_of_birt_tx <- rep(NA, nrow(data_all_clients))
  for(r in 1:nrow(data_all_clients)){
    data_all_clients$date_of_birt_tx[r] <- format(as.Date(data_all_clients$date_of_birth[r], '%Y-%m-%d'), "%m/%d/%Y")
  }
  
  data_all_clients$date_of_birt_tx <- lubridate::mdy(data_all_clients$date_of_birt_tx)
  
  # replace phq dob with all clients dob
  for(s in 1:nrow(data_all_clients)){
    
    # get current client_id
    sid <- data_all_clients$client_id[s]
    
    # set dob replacement from all clients sheet
    dob_replace <- data_all_clients$date_of_birt_tx[which(data_all_clients$client_id==sid)]
    
    # leave as is if dob not recorded in all clients
    if(is.na(dob_replace)){
      next
    }else{
      phq[which(phq$client_id==sid), 'date_of_birth_tx'] <- dob_replace
    }
  }
  
  # format PHQ data
  phq_formatted <- phq %>%
    mutate(date_submitted_transformed=phq9_date,
           date_of_birth_transformed=ymd(date_of_birth_tx)) %>%
    dplyr::select(-archived, -source, -first_opened, -date_submited, -phq9_date, -date_of_birth, -date_of_birth_tx, -x_optional_in_your_own_words_how_would_you_describe_how_you_are_feeling_and_what_is_contributing_to_your_current_mood) %>%
    rename('phq9_functionality'='have_these_problems_made_it_difficult_for_you_to_do_your_work_take_care_of_things_at_home_or_get_along_with_other_people') %>%
    arrange(client_id, date_submitted_transformed) %>%
    group_by(client_id) %>%
    mutate(time_point=row_number()) %>% 
    mutate(time_interval = date_submitted_transformed - date_submitted_transformed[1]) %>%
    mutate(time_since_previous = date_submitted_transformed - lag(date_submitted_transformed, n = 1, default = first (date_submitted_transformed))) %>%
    mutate(age_years = lubridate::interval(date_of_birth_transformed, date_submitted_transformed) / years(x=1)) %>%
    as.data.frame()
  
  # sanity check phq range
  # phq_formatted[which(phq_formatted$phq_tot>27), ]
  
  # rename variables in phq dataframe
  phq_formatted <- phq_formatted %>%
    rename('date_submitted_phq'='date_submitted_transformed')
  
  # rename variables in merged dataframe and drop date of birth
  merged_patient_airtable_intake_data <- merged_patient_airtable_intake_data %>%
    rename('date_submitted_intake'='date_submitted')
  
  # merge with left join
  merged_phq_patient_data <- phq_formatted %>%
    left_join(merged_patient_airtable_intake_data, by = 'client_id')
  
  # create Field 1: Date PHQ-9 submitted minus Date Of First infusion: "time_first_infusion_to_phq"
  merged_phq_patient_data$time_first_infusion_to_phq <- merged_phq_patient_data$date_submitted_phq - merged_phq_patient_data$first_infusion_completed # coded so negative is PHQ before infusion
  if (plot_hist) {
    hist(as.numeric(merged_phq_patient_data$time_first_infusion_to_phq))
  }
  
  # drop/remove all records where Field1 < -30 days 
  merged_phq_patient_data <- merged_phq_patient_data[!is.na(merged_phq_patient_data$time_first_infusion_to_phq), ]
  merged_phq_patient_data <- merged_phq_patient_data[!merged_phq_patient_data$time_first_infusion_to_phq < time_pre, ]
  
  # create Field 2; Date PHQ-9 submitted minus Date Last Foundational Infusion: "time_last_foundational_infusion_to_phq"
  merged_phq_patient_data$time_last_foundational_infusion_to_phq <- merged_phq_patient_data$date_submitted_phq - merged_phq_patient_data$last_foundational_infusion
  
  # drop/remove all records where absolute time > 30 days for time_last_foundational_infusion_to_phq
  merged_phq_patient_data <- merged_phq_patient_data[!merged_phq_patient_data$time_last_foundational_infusion_to_phq > time_post,]
  #hist(as.numeric(merged_phq_patient_data$time_last_foundational_infusion_to_phq))
  
  ## Identify T1 (baseline) PHQ as worst PHQ prior to or on day of first infusion
  tmp <- split(merged_phq_patient_data, merged_phq_patient_data$client_id)
  
  tmp2 <- lapply(tmp, function(x){
    
    #print(x$client_id)
    x_ss <- x[x$time_first_infusion_to_phq <= 0, ] # ensure index is before infusion 1
    
    if(nrow(x_ss)<1){
      return(NULL)
    }else{
      
      index_row <- which.max(x_ss$phq_tot)
      ss <- rbind(x[index_row,], x[x$time_first_infusion_to_phq >0, ]) # combine defined baseline with all time points past first infusion
      ss$time_point_uncorrected <- c(x[index_row, 'time_point'], x[x$time_first_infusion_to_phq >0, 'time_point']) # record uncorrected time point
      ss$time_point <- 1:nrow(ss)
      return(ss)
      
    }
  })
  merged_phq_patient_data <- do.call(rbind, tmp2)
  
  # sanity checks
  # sum(as.numeric(merged_phq_patient_data[merged_phq_patient_data$time_point==1, 'time_since_first_infusion'])>0)
  
  # confirm merge worked
  # sid <- '3606' # 3555, 4180, 3606, 230
  # merged_phq_patient_data[merged_phq_patient_data$client_id==sid, 'time_point']
  # merged_phq_patient_data[merged_phq_patient_data$client_id==sid, 'phq_tot']
  # merged_phq_patient_data[merged_phq_patient_data$client_id==sid, 'medication_1_medication_name']
  # merged_phq_patient_data[merged_phq_patient_data$client_id==sid, 'date_of_birth_transformed']
  
  return(merged_phq_patient_data)

}

