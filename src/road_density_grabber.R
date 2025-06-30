road_density_grabber <- function(watersheds){
  
  all_roads <- readRDS("data/TIGER_roads.RDS") 
  if(crs(all_roads) != crs(watersheds)){
    all_roads <- all_roads %>% st_transform(crs(watersheds))
  }
  
  all_roads <- all_roads %>%
    as.data.table() %>%
    distinct() %>%
    st_as_sf() %>%
    .[watersheds,]
  
  road_lengths <- vector("list", length = nrow(watersheds))
  
  for(i in 1:nrow(watersheds)){
    
    ws <- watersheds[i,]
    
    # Intersect all roads with all watersheds
    intersection <- all_roads %>%
      st_intersection(., ws)
    
    # Calculate lengths of all road segments
    intersection$length_m <- st_length(intersection)
    
    # Summarize road lengths by watershed
    road_lengths[[i]] <- intersection %>%
      st_drop_geometry() %>%
      group_by(index) %>%
      summarize(road_length_m = sum(as.numeric(length_m), na.rm = TRUE))
    print(ws$index)
  }
  
  road_lengths <- road_lengths %>% bind_rows()
  
  # Join with watershed data
  result <- watersheds %>%
    st_drop_geometry() %>%
    select(index) %>%
    left_join(road_lengths, by = "index") %>%
    mutate(area_m2 = st_area(watersheds),
           road_length_m = ifelse(is.na(road_length_m), 0, road_length_m),
           watershed_area_m2 = as.numeric(area_m2),
           road_density_km_per_km2 = (road_length_m / watershed_area_m2)*100)
  
  # Join back with the original watersheds to create a spatial dataframe
  watersheds_with_density <- left_join(watersheds, result, by = "index")
  
  return(watersheds_with_density)
  
}