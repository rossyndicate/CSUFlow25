find_dominant_hydroregion <- function(watersheds, region_col = "Region") {
  
  hydro_data <- sf::st_read("data/hyd_region_2025/Hyd_Region_2025.shp") %>%
    sf::st_set_crs(4326)
  
  # Ensure same CRS
  if(st_crs(watersheds) != st_crs(hydro_data)){
    watersheds_proj <- st_transform(watersheds, crs = st_crs(hydro_data))
  } else {
    watersheds_proj <- watersheds
  }
  
  dominant_region <- vector("list", length = nrow(watersheds_proj))
  
  for(i in 1:nrow(watersheds_proj)){
    
    # Perform spatial intersection
    intersect_hydro <- st_intersection(hydro_data, watersheds_proj[i,])
    
    # If no intersection, return nearest
    if(nrow(intersect_hydro) == 0) {
      
      nearest_region <- hydro_data[st_nearest_feature(watersheds_proj[i,], hydro_data),] %>%
        pull(!!sym(region_col))
      
      dominant_region[[i]] <- watersheds_proj[i,] %>%
        mutate(hyd_region = nearest_region) %>%
        st_drop_geometry()
      
    } else {
    
    # Calculate intersection areas
    intersect_hydro$intersection_area <- st_area(intersect_hydro)
    
    # Find the region with the largest intersection area
    region <- intersect_hydro %>%
      st_drop_geometry() %>%
      group_by(!!sym(region_col)) %>%
      summarize(total_area = sum(as.numeric(intersection_area)), .groups = "drop") %>%
      arrange(desc(total_area)) %>%
      slice(1) %>%
      pull(!!sym(region_col))
    
    dominant_region[[i]] <- watersheds_proj[i,] %>%
      mutate(hyd_region = region) %>%
      st_drop_geometry()
    
    }
    
  }
  
  dominant_regions <- bind_rows(dominant_region)  %>%
    select(index, hyd_region)
  
  return(dominant_regions)
  
}