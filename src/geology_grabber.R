geology_grabber <- function(watersheds){
  
  # # Read in geological data
  # geo <- st_read("data/geo/864e9173-8ce1-40a9-9b6d-a22727e86446.gdb", 
  #                layer = "SGMC_Geology_A") %>%
  #   mutate(CLASS = case_when(GENERALIZE == "Igneous, intrusive" ~ "perc_intrusive",
  #                            grepl("Metamorphic|Melange|Tectonite", GENERALIZE) ~ "perc_metamorphic",
  #                            GENERALIZE == "Igneous, volcanic" ~ "perc_volcanic",
  #                            GENERALIZE %in% c("Sedimentary, clastic", "Sedimentary, iron formation, undifferentiated",
  #                                              "Sedimentary, undifferentiated") ~ "perc_sedimentary_clastic",
  #                            GENERALIZE %in% c("Sedimentary, chemical", "perc_sedimentary_carbonate",
  #                                              "Sedimentary, evaporite") ~ "perc_sedimentary_chemical",
  #                            grepl("Unconsolidated", GENERALIZE) ~ "perc_unconsolidated",
  #                            GENERALIZE %in% c("Igneous and Metamorphic, undifferentiated", "Igneous and Metamorphic, undifferentiated",
  #                                              "Igneous, undifferentiated") ~ "perc_mix")) %>%
  #   filter(!is.na(CLASS))
  # 
  # # Create a vector of all possible CLASS values to ensure they're included in the output
  # all_class_values <- c("perc_intrusive", "perc_metamorphic", "perc_volcanic", 
  #                       "perc_sedimentary_clastic", "perc_sedimentary_chemical", 
  #                       "perc_sedimentary_carbonate", "perc_unconsolidated", "perc_mix")
  # 
  # # Ensure both datasets have the same CRS
  # if(st_crs(watersheds) != st_crs(geo)){
  #   sf <- st_transform(watersheds, crs = st_crs(geo))
  # } else {
  #   sf <- watersheds
  # }
  # 
  # intersect_geo_sf <- geo %>%
  #   .[sf,]
  # 
  # saveRDS(intersect_geo_sf, "data/geology.RDS")
  # Create a vector of all possible CLASS values to ensure they're included in the output
  all_class_values <- c("perc_intrusive", "perc_metamorphic", "perc_volcanic",
                        "perc_sedimentary_clastic", "perc_sedimentary_chemical",
                        "perc_sedimentary_carbonate", "perc_unconsolidated", "perc_mix")
  
  geo <- readRDS("data/geology.RDS")
  
  # Ensure both datasets have the same CRS
  if(st_crs(watersheds) != st_crs(geo)){
    sf <- st_transform(watersheds, crs = st_crs(geo))
  } else {
    sf <- watersheds
  }
  
  # Perform spatial intersection
  intersect_geo_sf <- st_intersection(geo, sf)
  
  # Calculate area of each intersection
  intersect_geo_sf$intersection_area <- st_area(intersect_geo_sf)
  
  # Calculate area of each watershed for reference
  sf$total_area <- st_area(sf)
  
  # Join watershed areas to the intersection data
  intersect_with_total <- intersect_geo_sf %>%
    left_join(sf %>% st_drop_geometry() %>% select(index, total_area), by = "index")
  
  # Get percentage of each geologic class within each watershed
  geo_watershed_pct <- intersect_with_total %>%
    mutate(pct_area = as.numeric(intersection_area / total_area) * 100) %>%
    group_by(index, CLASS) %>%
    summarize(area_sqm = sum(as.numeric(intersection_area)),
              total_watershed_area = first(total_area),
              percent_of_watershed = as.numeric(sum(as.numeric(intersection_area)) / first(total_area) * 100),
              .groups = "drop")
  
  # Create a complete template with all index and CLASS combinations
  # This ensures all CLASS values are represented for each watershed
  template <- expand.grid(
    index = unique(sf$index),
    CLASS = all_class_values,
    stringsAsFactors = FALSE
  )
  
  # Join the actual data to the template
  geo_watershed_complete <- template %>%
    left_join(geo_watershed_pct %>% st_drop_geometry() %>% 
                select(index, CLASS, percent_of_watershed), 
              by = c("index", "CLASS")) %>%
    # Replace NA values with 0
    mutate(percent_of_watershed = ifelse(is.na(percent_of_watershed), 0, percent_of_watershed))
  
  # Reorganize the results 
  geo_watershed_wide <- geo_watershed_complete %>%
    select(index, CLASS, percent_of_watershed) %>%
    pivot_wider(id_cols = index,
                names_from = CLASS,
                values_from = percent_of_watershed)
  
  # Join the results back to the watersheds
  watersheds <- watersheds %>%
    select(index) %>%
    left_join(geo_watershed_wide, by = "index")
  
  # Vector of geology column names
  geology_cols <- c(
    "perc_intrusive", "perc_metamorphic", "perc_volcanic",
    "perc_sedimentary_clastic", "perc_sedimentary_chemical",
    "perc_sedimentary_carbonate", "perc_unconsolidated", "perc_mix"
  )
  
  # create dominant_geology column
  watersheds_geology <- watersheds %>%
    rowwise() %>%
    mutate(dominant_geology = sub("^perc_", "", geology_cols[which.max(c_across(all_of(geology_cols)))])) %>%
    ungroup() %>%
    select(index, dominant_geology)
  
  return(watersheds_geology)
}