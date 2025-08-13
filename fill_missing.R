fill_missing <- function(ember_data, variable, path_data){
  
  ## searches the following for replacements
  # - 'MGH - All Intake Data - Combined.csv'
  # - 'MGH - Form - Intake Package - 2025update.csv'
  # - 'MGH - List - All Appointments - 2025Data.csv'
  # - 'MGH - List - All Clients - 2025Data.csv'
  # - 'MGH - Form - Brief Medical Intake Form - 2025Data.csv'
  
  require(stringr)
  
  # set sex variables != Male | Female to "" so they are replaced 
  if(variable=="sex_assigned_at_birth"){
    
    print("setting levels to missing")
    ember_data$sex_assigned_at_birth[ember_data$sex_assigned_at_birth != "Male" & ember_data$sex_assigned_at_birth != "Female"] <- ""
    
  }
  
  # get client_ids with missing variables
  missing <- c(ember_data[which(is.na(ember_data[[variable]])), 'client_id'], 
               ember_data[which(ember_data[[variable]]==""), 'client_id'])
  missing_unique <- unique(missing)
  
  # initialize dataframe for mapping
  df_mapping <- data.frame(client_id = missing_unique,
                           value = rep("", length(missing_unique)))
  
  # first: check whether variable is present in existing dataframe in any records
  for(s in 1:length(missing_unique)){
    
    # get values across current subject time points
    values <- ember_data[ember_data$client_id==missing_unique[s], variable]
    
    # get unique strings
    values_unique <- unique(values)
    
    # if only one unique value exists it should be NA so skip
    if(length(values_unique)<2){
      next
    }else{
      
      # get length of unique strings 
      sl <- str_length(values_unique)
      
      # set any NAs to length 0
      sl[is.na(sl)] <- 0
      
      # assign any non-na value to mapping list
      df_mapping[df_mapping$client_id==missing_unique[s], 'value'] <- values_unique[which(sl>0)]
      
    }
    
  }
  
  print(sprintf("Found %s missing values", nrow(df_mapping[df_mapping$value!="", ])))
  
  # define function to searach EHR forms for replacements
  search_missings <- function(data, df_mapping, variable){
    
    # update missing unique
    missing_unique <- df_mapping$client_id[which(df_mapping$value=="")]
    
    for(s in 1:length(missing_unique)){
      
      print(s)
      
      # get values across current subject time points
      values <- data[data$client_id==missing_unique[s], variable]
      
      # get unique strings
      values_unique <- unique(values)
      
      if(is.null(values_unique)){
        next
      }
      
      # if only one unique value exists it should be NA so skip
      if(length(values_unique)<2 & (is.na(values_unique[1]) | values_unique[1] == "")){
        next
      }else{
        
        # get length of unique strings 
        sl <- str_length(values_unique)
        
        # set any NAs to length 0
        sl[is.na(sl)] <- 0
        
        # assign any non-na value to mapping list
        df_mapping[df_mapping$client_id==missing_unique[s], 'value'] <- values_unique[which(sl>0)]
        
      }
      
    }
    
    print(sprintf("Found %s missing values", nrow(df_mapping[df_mapping$value!="", ])))
    
    return(df_mapping)
    
  }
  
  # all clients form
  #data_all_clients <- read.csv(paste0(path_data, 'MGH - List - All Clients - 2025Data.csv'))
  data_all_clients <- readxl::read_xlsx(paste0(path_data, 'MGH - List - All Clients.xlsx'), sheet = '2025Data')
  data_all_clients <- data_all_clients %>%
    clean_names()
  
  df_mapping <- search_missings(data = data_all_clients, df_mapping = df_mapping, variable = variable)
  
  # all intake data form
  data_all_intake <- read.csv(paste0(path_data, 'MGH - All Intake Data - Combined.csv'))
  data_all_intake <- data_all_intake %>%
    clean_names()
  
  df_mapping <- search_missings(data = data_all_intake, df_mapping = df_mapping, variable = variable)
  
  # intake package data form
  data_intake_package <- read.csv(paste0(path_data, 'MGH - Form - Intake Package - 2025update.csv'))
  data_intake_package <- data_intake_package %>%
    clean_names()
  
  df_mapping <- search_missings(data = data_intake_package, df_mapping = df_mapping, variable = variable)
  
  # all appointments form
  data_all_appointments <- read.csv(paste0(path_data, 'MGH - List - All Appointments - 2025Data.csv'))
  data_all_appointments <- data_all_appointments %>%
    clean_names()
  
  df_mapping <- search_missings(data = data_all_appointments, df_mapping = df_mapping, variable = variable)
  
  # brief intake form
  data_brief_intake <- read.csv(paste0(path_data, 'MGH - Form - Brief Medical Intake Form - 2025Data.csv'))
  data_brief_intake <- data_brief_intake %>%
    clean_names()
  
  df_mapping <- search_missings(data = data_brief_intake, df_mapping = df_mapping, variable = variable)
  
  ## Merge with original data
  df_mapping_ss <- df_mapping[df_mapping$value!="", ]
  
  for(i in 1:length(df_mapping_ss$client_id)){
    
    cid <- df_mapping_ss$client_id[i]
    
    ember_data[ember_data$client_id==cid, variable] <- df_mapping_ss$value[i]
    
  }
  
  return(ember_data)

}













