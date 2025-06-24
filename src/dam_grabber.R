dam_grabber <- function(watersheds){
   
   # Load NID data
   nid_nation <- data.table::fread('data/nid_nation.csv') %>%
     filter(!is.na(Longitude) & !is.na(Latitude)) %>%
     # Convert to spatial object
     st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) 
   
   # Ensure both layers have the same CRS
   if(st_crs(watersheds) != st_crs(nid_nation)){
     nid_nation <- st_transform(nid_nation, st_crs(watersheds))
   }
   
   # For each watershed, calculate dam statistics
   dam_stats <- vector("list", length = nrow(watersheds))
   
   for(i in 1:nrow(watersheds)){
     
     # Extract single watershed
     watershed <- watersheds[i,]
     
     # Find dams within the watershed
     dams_in_watershed <- st_intersection(nid_nation, watershed)
     
     # Calculate area in square kilometers
     watershed_area_km2 <- as.numeric(st_area(watershed))/1000000
     
     # Calculate dam density (per km²)
     dam_density <- nrow(dams_in_watershed) / watershed_area_km2
     
     # Calculate dam storage values (acre-feet per km²)
     # Replace NAs with 0 for calculations
     nid_storage <- sum(dams_in_watershed$`NID.Storage..Acre.Ft.`, na.rm = TRUE) / watershed_area_km2
     normal_storage <- sum(dams_in_watershed$`Normal.Storage..Acre.Ft.`, na.rm = TRUE) / watershed_area_km2
     
     # Store results for this watershed
     dam_stats[[i]] <- data.frame(
       site_no = watershed$site_no,
       damdensws_nid = dam_density,
       damnidstorws_nid = nid_storage,
       damnrmstorws_nid = normal_storage
     )
     
     # Print progress
     if(i %% 10 == 0) {
       cat("Processed", i, "of", nrow(watersheds), "watersheds\n")
     }
   }
   
   # Combine results
   dam_stats_df <- bind_rows(dam_stats)
   
   # Join with original watersheds data
   watersheds_with_dams <- watersheds %>%
     left_join(dam_stats_df, by = "site_no")
   
   return(watersheds_with_dams)
 }