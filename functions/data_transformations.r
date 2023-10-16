
# Creating a function


transform_metadata_to_df <- function(metadata){
  testing <- stations_metadata[[1]] %>%  
    map(as_tibble) %>%  
    list_rbind() %>%  
    mutate(latestData = map_chr(latestData, 1, .default = "")) %>%  
    mutate(latestData = as_datetime(latestData, tz = "Europe/Berlin")) %>% 
    mutate(location = map(location, unlist))%>%  
    mutate(
      lat = map_dbl(location, "latLon.lat"),
      lon = map_dbl(location, "latLon.lon")
    ) %>% 
    select(-location)
}



