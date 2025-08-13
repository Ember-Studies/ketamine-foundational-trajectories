# Helper script to format intermediate drug matching table. 
# These changes should be integrated into the main pipeline later. 

data <- matched_results %>%
  as.data.frame()

# unpack categories
category_1 <- sapply(data$categories, function(x){ 
  return(x[[1]])
})

category_2 <- sapply(data$categories, function(x){ 
  len <- length(x)
  if(len==1){
    return("No second category")
  }else{
    return(x[[2]])
  }
})

data <- data %>%
  dplyr::select(-categories) %>%
  mutate(category_1 = category_1) %>%
  mutate(category_2 = category_2)

data$degree_matched <- unlist(data$degree_matched)

write.csv(data, file = '~/Desktop/ember_medication_matches_vincent.csv', row.names = F, quote = F)
