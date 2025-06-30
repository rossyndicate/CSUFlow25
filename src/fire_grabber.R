
fire_grabber <- function(watersheds){

  sf_use_s2(FALSE)
  
  # Read and transform fire perimeter data
  fire_perimeters <- readRDS("data/fire.RDS") %>%
    st_transform(crs = st_crs(watersheds))
  
  # Initialize output
  results <- watersheds %>%
    mutate(
      ws_area_m2 = st_area(.),        # Watershed area
      burned_area_m2 = 0,             # Placeholder for burned area
      percent_burned = 0              # Placeholder for % burned
    )
  
  # Loop over each watershed and calculate overlap
  for (i in 1:nrow(watersheds)) {
    
    ws <- watersheds[i, ]
    
    # Find fire perimeters that intersect this watershed
    fires_in_ws <- fire_perimeters[st_intersects(ws, fire_perimeters, sparse = FALSE), ]
    
    if (nrow(fires_in_ws) > 0) {
      burned_geom <- st_intersection(ws, st_union(fires_in_ws))
      burned_area <- st_area(burned_geom)
      results$burned_area_m2[i] <- burned_area
      results$percent_burned[i] <- as.numeric(burned_area / results$ws_area_m2[i]) * 100
    }
  }
  sf_use_s2(TRUE)
  return(results %>% select(-burned_area_m2, -ws_area_m2))
}
