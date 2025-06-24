raster_data <- terra::rast("data/for_app/flow_network_3857.tif")

stack = terra::rast(list.files("data/daymet/", pattern = "prcp", full.names = TRUE)) 
  
    stack_crs <- crs(stack[[1]])
    
    # transform AOI to match raster CRS if needed
    if(st_crs(raster_data) != stack_crs) {
      raster_data <- terra::project(raster_data, stack)
    } else {
      raster_data <- raster_data
    }
  
    cropped <- terra::crop(stack, raster_data, mask = TRUE)
    
    library(elevatr); library(tigris); library(terra)
    colorado_elevation <- terra::rast(elevatr::get_elev_raster(raster_data, zoom = 12))
    saveRDS(colorado_elevation, "data/elevation.RDS")
