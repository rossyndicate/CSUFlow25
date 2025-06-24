road_density_grabber <- function(watersheds){
  
  # Get states data to determine which states to fetch counties from
  states <- tigris::states()

  if(crs(watersheds) != crs(states)){
    states <- st_transform(states, crs(watersheds))
  }

  # Find overlapping states (using st_intersects for speed)
  state_intersects <- st_intersects(watersheds, states, sparse = TRUE)
  overlapping_states <- states[unlist(state_intersects), ]

  # Only fetch counties for states that overlap with our watersheds
  overlapping_counties <- data.frame()

  state_fips <- overlapping_states$STATEFP

  # Get counties only for the states that intersect
  counties <- tigris::counties(state = state_fips, cb = TRUE, resolution = "20m")
  counties <- st_transform(counties, st_crs(watersheds))

  # Find overlapping counties
  county_intersects <- st_join(watersheds, counties, left = TRUE) %>%
    st_drop_geometry() %>%
    select(site_no, STATEFP, COUNTYFP) %>%
    distinct()

  table <- vector("list", length = nrow(watersheds))

  for(i in 1:nrow(watersheds)){

    sub <- county_intersects %>%
      filter(site_no == watersheds[i,]$site_no)

    roads <- vector("list", length = nrow(sub))

    for(j in 1:nrow(sub)){
      roads[[i]] <- tigris::roads(state = sub[j,]$STATEFP, county = sub[j,]$COUNTYFP)
    }

    table[[i]] <- bind_rows(roads)

  }
  
  all_roads <- bind_rows(table)
  
  if(crs(all_roads) != crs(watersheds)){
    all_roads <- all_roads %>% st_transform(crs(watersheds))
  }
  
  
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
      group_by(site_no) %>%
      summarize(road_length_m = sum(as.numeric(length_m), na.rm = TRUE))
    print(ws$site_no)
  }
  
  road_lengths <- road_lengths %>% bind_rows()
  
  # Join with watershed data
  result <- watersheds %>%
    st_drop_geometry() %>%
    select(site_no) %>%
    left_join(road_lengths, by = "site_no") %>%
    mutate(area_m2 = st_area(watersheds),
           road_length_m = ifelse(is.na(road_length_m), 0, road_length_m),
           watershed_area_m2 = as.numeric(area_m2),
           road_density_km_per_km2 = (road_length_m / watershed_area_m2)*100)
  
  # Join back with the original watersheds to create a spatial dataframe
  watersheds_with_density <- left_join(watersheds, result, by = "site_no")
  
  return(watersheds_with_density)
  
}