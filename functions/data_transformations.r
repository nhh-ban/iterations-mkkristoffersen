
# Creating a function for transforming metadata to a dataframe


transform_metadata_to_df <- function(metadata){
  testing <- stations_metadata[[1]] %>%  
    map(as_tibble) %>%  
    list_rbind() %>%  
    mutate(latestData = map_chr(latestData, 1, .default = "")) %>%  
    mutate(latestData = as_datetime(latestData, tz = "UTC")) %>% 
    mutate(location = map(location, unlist))%>%  
    mutate(
      lat = map_dbl(location, "latLon.lat"),
      lon = map_dbl(location, "latLon.lon")
    ) %>% 
    select(-location)
}




# Creating a function to_iso8610

to_iso8601 <- function(datetime, offset){
  date <- datetime + days(offset)
  timeformat <- iso8601(date)
  timeformat_with_z <- paste(timeformat, "Z", sep = "")
  return(timeformat_with_z)
}



# transform_volumes()

transform_volumes <- function(df){
  create_df <- as.data.frame(matrix(unlist(df$trafficData$volume$byHour$edges), ncol = 3, byrow = TRUE))
  colnames(create_df) <- c("from","to","volume")
  correct_specification <- create_df %>% 
    mutate(from = as_datetime(from, tz = "UTC"),
           to = as_datetime(to, tz = "UTC"),
           volume = as.numeric(volume))
  return(correct_specification)
}

