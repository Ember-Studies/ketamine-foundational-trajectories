# Time pre: days before/including first infusion (should be passed as negative value)
# Time post: days after last infusion

format_merge_phq_data <- function(merged_patient_airtable_intake_data, path_data, time_pre, time_post){
  
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
  
  # get patient age and identify likely typo dobs
  phq$age_est <- lubridate::interval(phq$date_of_birth_tx, phq$date_submited_tx) / years(x=1)
  
  # gather IDs of young patients that may be typos: fixed arbitrarily to <= 18 and >=99
  ages_to_correct_unique <- c(unique(phq$client_id[which(phq$age_est <= 18)]),
                              unique(phq$client_id[which(phq$age_est >= 99)])) 
  
  # gather IDs of patients with missing ages in PHQ
  ages_to_correct_missing <- unique(phq$client_id[which(is.na(phq$age_est))])
  
  # combine
  ages_to_correct <- unique(c(ages_to_correct_unique, ages_to_correct_missing))
  ages_to_correct <- ages_to_correct[!is.na(ages_to_correct)]
  
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
  for(s in ages_to_correct){
    phq[which(phq$client_id==s), 'date_of_birth_tx'] <- data_all_clients$date_of_birt_tx[which(data_all_clients$client_id==s)]
  }
  
  # format PHQ data
  phq_formatted <- phq %>%
    mutate(date_submitted_transformed=ymd(date_submited),
           date_of_birth_transformed=ymd(date_of_birth_tx)) %>%
    dplyr::select(-archived, -source, -first_opened, -date_submited, -date_of_birth, -have_these_problems_made_it_difficult_for_you_to_do_your_work_take_care_of_things_at_home_or_get_along_with_other_people, -age_est, -date_of_birth_tx, -x_optional_in_your_own_words_how_would_you_describe_how_you_are_feeling_and_what_is_contributing_to_your_current_mood) %>%
    arrange(client_id, date_submitted_transformed) %>%
    group_by(client_id) %>%
    mutate(time_point=row_number()) %>% 
    mutate(time_interval = date_submitted_transformed - date_submitted_transformed[1]) %>%
    mutate(time_since_previous = date_submitted_transformed - lag(date_submitted_transformed, n = 1, default = first (date_submitted_transformed))) %>%
    mutate(age_years = lubridate::interval(date_of_birth_transformed, date_submitted_transformed) / years(x=1))
  
  phq_formatted <- data.frame(phq_formatted)
  
  # sanity check phq range
  # phq_formatted[which(phq_formatted$phq_tot>27), ]
  
  # rename variables in phq dataframe
  phq_formatted <- phq_formatted %>%
    rename('date_submitted_phq'='date_submitted_transformed')
  
  # rename variables in merged dataframe and drop date of birth
  merged_patient_airtable_intake_data <- merged_patient_airtable_intake_data %>%
    rename('date_submitted_intake'='date_submitted') %>%
    dplyr::select(-date_of_birth)
  
  # merge with left join
  merged_phq_patient_data <- phq_formatted %>%
    left_join(merged_patient_airtable_intake_data, by = 'client_id')
  
  # create Field 1: Date PHQ-9 submitted minus Date Of First infusion: "time_first_infusion_to_phq"
  merged_phq_patient_data$time_first_infusion_to_phq <- merged_phq_patient_data$date_submitted_phq - merged_phq_patient_data$first_infusion_completed # coded so negative is PHQ before infusion
  hist(as.numeric(merged_phq_patient_data$time_first_infusion_to_phq))
  
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

