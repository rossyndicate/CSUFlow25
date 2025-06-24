# natsgo_grabber <- function(watersheds){
#   
#   sf_use_s2(FALSE)
#   
#   ws <- watersheds
#   
#   
#   sf <- sf::st_read("data/katie/wss_gsmsoil_US_[2016-10-13]/spatial/gsmsoilmu_a_us.shp") 
#   
#   if(crs(sf) != crs(ws)){
#     
#     ws <- ws %>% st_transform(crs(sf))
#     
#   }
#   
#   sf <- sf %>%
#     .[ws,] %>%
#     sf::st_make_valid()
#   
#   
#   component_cols <- component_names_vector  # Example
#   component <- read_delim(file.path("data/katie/wss_gsmsoil_US_[2016-10-13]/tabular/comp.txt"),
#                           delim = "|",
#                           col_names = component_cols,
#                           col_types = cols(.default = "c")) %>%
#     select(mukey, cokey) %>%
#     st_drop_geometry()
#   
#   # ksat_r: The amount of water that would move vertically through a unit area of saturated soil in unit time under unit hydraulic gradient. um/s  
#   chorizon_cols <- names(chorizon_names)  # Example
#   chorizon <- read_delim(file.path("data/katie/wss_gsmsoil_US_[2016-10-13]/tabular/chorizon.txt"),
#                           delim = "|",
#                           col_names = chorizon_cols,
#                           col_types = cols(.default = "c")) %>%
#     dplyr::select(cokey, sandtotal_r, claytotal_r, ksat_r) %>%
#     st_drop_geometry()
#   
#   
#   
#   
#   # brockdepmin: The distance from the soil surface to the top of a bedrock layer, expressed as a shallowest depth of components whose composition in the
#   # map unit is equal to or exceeds 15%. cm
#   # wtdepannmin: The shallowest depth to a wet soil layer (water table) at any time during the year expressed as centimeters from the soil surface, for
#   # components whose composition in the map unit is equal to or exceeds 15%. cm
#   # ksat_r: The amount of water that would move vertically through a unit area of saturated soil in unit time under unit hydraulic gradient. um/s  
#   water_table_cols <- names(water_table_names)  # Example
#   water_table <- read_delim(file.path("data/katie/wss_gsmsoil_US_[2016-10-13]/tabular/chorizon.txt"),
#                          delim = "|",
#                          col_names = water_table_cols,
#                          col_types = cols(.default = "c")) %>%
#     select(mukey, wtdepannmin, brockdepmin) %>%
#     st_drop_geometry()
#   
#   full_data[[i]] <- sf %>%
#     left_join(component, by = c("MUKEY" = "mukey")) %>%
#     left_join(chorizon, by = "cokey") %>%
#     left_join(water_table, by = c("MUKEY" = "mukey"))
#   
#   print(states[i])
#   
# }
# 
# full_dat <- bind_rows(full_data)
# 
# sub <- st_intersection(ws, full_dat) %>%
#   as.data.table() %>%
#   distinct() %>%
#   st_as_sf()
# 
# area_weighted_averages <- sub %>%
#   # Calculate area of each intersected polygon
#   mutate(polygon_area = st_area(.)) %>%
#   st_drop_geometry() %>%
#   as.data.table() %>%
#   # Convert to numeric if needed
#   mutate(polygon_area = as.numeric(polygon_area)) %>%
#   group_by(site_no) %>%  
#   # Calculate weighted values
#   summarize(sandws = weighted.mean(sandtotal_r, polygon_area, na.rm = TRUE),
#             clayws = weighted.mean(claytotal_r, polygon_area, na.rm = TRUE),
#             permws = weighted.mean(ksat_r, polygon_area, na.rm = TRUE)*0.36, # us/S to cm/hr
#             wtdepws = weighted.mean(wtdepannmin, polygon_area, na.rm = TRUE)*10, # cm to mm
#             rckdepws = weighted.mean(brockdepmin, polygon_area, na.rm = TRUE)*10, # cm to mm
#             .groups = "drop") 
# 
# sf_use_s2(TRUE)
# 
# return(area_weighted_averages)
# 
# }
# 
# 
# 
# 
# 
# test <- st_layers("data/katie/gNATSGO_02_03_2025_gpkg/gNATSGO_02_03_2025.gpkg")
# 
# sf <- terra::rast("data/katie/gNATSGO_02_03_2025_gpkg/muraster_30m.tif")
# 
# if(crs(sf) != crs(ws)){
#   
#   ws <- ws %>% st_transform(crs(sf))
#   
# }
# 
# cropped <- sf %>%
#   extract(., ws, cells = TRUE, weights = TRUE, ID = TRUE, mask = TRUE )
# 
# 
# 
# 
