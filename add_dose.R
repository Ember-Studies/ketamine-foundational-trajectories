add_dose <- function(merged_phq_patient_data, dose_data){

  # split data by subject id
  data_split <- split(merged_phq_patient_data, f = merged_phq_patient_data$client_id)
  
  # loop over subjects
  for(sid in 1:length(data_split)){
    
    print(sid)
    
    # confirm current subject has dose records
    matching_ids <- data_split[[sid]]$client_id %in% dose_data$client_id
    
    # # skip to next loop iteration if only one time point record matched; these patients shouldn't be for longitudinal analysis 
    # if(length(matching_ids)<2){
    #   data_split[[sid]]$ketamine_dose_mg_kg <- rep(NA, nrow(data_split[[sid]]))
    #   next
    # }
    
    # subset dose data on current subject
    dose_data_ss <- dose_data[dose_data$client_id %in% data_split[[sid]]$client_id, ]
    
    # get current subject's dose dates
    dates_ket <- unique(dose_data_ss$date_ketamine)
    
    # some patients have no ketamine administrations and will be excluded later but throws errors here
    if(length(dates_ket)<1){
      data_split[[sid]]$ketamine_dose_mg_kg <- rep(NA, nrow(data_split[[sid]]))
      #data_split[[sid]]$date_ketamine <- rep(NA, nrow(data_split[[sid]]))
      data_split[[sid]]$infusions_before_phq <- rep(0, nrow(data_split[[sid]]))
      next
    }
    
    # get current subject's phq dates
    dates_phq <- data_split[[sid]]$date_submitted_phq 
    
    # identify nearest preceding infusion dates for each phq date; note first PHQ will usually not have preceding infusion and will be NA
    
    # note: near date "prior" will include current date so this isn't ideal
    #nd <- neardate(rep(1, length(dates_phq)), rep(1, length(dates_ket)), dates_phq, dates_ket, best = 'prior')
    #nd
    
    # updated function to exclude current date matches
    find_nearest_prior_date <- function(target_date, date_vector) {
      prior_dates <- date_vector[date_vector < target_date]
      if (length(prior_dates) == 0) {
        return(NA)
      } else {
        return(max(prior_dates))
      }
    }
    
    nd <- sapply(dates_phq, function(d){
      nd_tmp <- find_nearest_prior_date(d, dates_ket)
      nd_idx <- which(dates_ket==as.Date(nd_tmp))
      if(length(nd_idx)==0){
        return(NA)
      }else{
        return(nd_idx) 
      }
    })
    nd <- unlist(nd)
    
    # format nearest date variable for mismatched cases
    if ( (length(nd) > 1) & (length(nd) == sum(is.na(nd))) ){
      nd <- nd[1:nrow(data_split[[sid]])]
    }
    
    # initialize dose variable in full data frame
    data_split[[sid]]$ketamine_dose_mg_kg <- rep(NA, length(nd))
    #data_split[[sid]]$date_ketamine <- rep(NA, length(nd))
    
    # loop over subjects using nearest date length
    for(d in 1:length(nd)){
      
      # if time point is 1 (should be pre-infusion) and confirmed that time point 1 is before or same date as first infusion, set dose to 0
      # added criterion that if current nearest date is NA (i.e., TP 1 before first infusion) dose is 0
      #if( (data_split[[sid]]$time_point[d]==1 & (data_split[[sid]]$date_submitted_phq[d] - dose_data_ss$date_ketamine[nd[d]])<=0) | is.na(nd[d])) {
      
      if( is.na(nd[d]) ){
        
        data_split[[sid]]$ketamine_dose_mg_kg[d] <- 0
        
      } else if( (data_split[[sid]]$time_point[d]==1 & (data_split[[sid]]$date_submitted_phq[d] - dose_data_ss$date_ketamine[nd[d]])<=0)) {
        
        data_split[[sid]]$ketamine_dose_mg_kg[d] <- 0
        
        # if not, baseline and pre-infusion use nearest preceding dose
      }else{
        
        data_split[[sid]]$ketamine_dose_mg_kg[d] <- dose_data_ss$ketamine_dose_mg_kg[nd[d]]
        
      }
      
    }
    
    # calculate cumulative number of ketamine doses for successive phq submissions
    # infusions_before_phq <- sapply(dates_phq, function(d){
    #   
    #   # don't count ketamine infusions from thirty days before first phq submission in case prior course is recorded (arbitrary threshold)
    #   dates_ket_ss <- dates_ket[dates_ket > min(dates_phq, na.rm=T) - 30]
    #   
    #   return(length( which(dates_ket_ss < d) ))
    #   
    # })
    
    # function to get cumulative ketamine administrations up to current PHQ date excluding current PHQ date
    prior_infusion_count <- function(target_date, date_vector) {
      
      prior_dates <- date_vector[date_vector < target_date]
      
      # don't allow prior ketamine dates less than some threshold before target: 60 (arbitrary)
      prior_dates <- prior_dates[(prior_dates - target_date) > -60]
      
      return(length(prior_dates))
      
    }
    
    infusions_before_phq <- sapply(dates_phq, function(d){
      prior_infusion_count(d, dates_ket)
    })
    
    # add number of prior infusions
    data_split[[sid]]$infusions_before_phq <- infusions_before_phq
    
    # # add ketamine dates doesn't work because often of different dimension
    # data_split[[sid]]$date_ketamine <- dates_ket[nd]
    
  }
  
  data_out <- do.call(rbind, data_split)
  
  # check
  #data_out[data_out$client_id==sample(unique(data_out$client_id), 1),]
  
  return(data_out)

}







