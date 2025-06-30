snow_persistence_grabber <- function(watersheds){
  
  # Load raster stack
  # raster_stack <- terra::rast("data/masked_sp_stack.tif")
  
  # OPTIMIZATION 1: Calculate mean annual SP first (before extraction)
  # This reduces the number of layers we need to work with
  # mean_annual_sp <- app(raster_stack, fun = mean, na.rm = TRUE)
  # 
  # terra::writeRaster(
  #   mean_annual_sp,
  #   filename = "data/masked_mean_annual_sp.tif", 
  #   overwrite = TRUE
  # )
  
  mean_annual_sp <- terra::rast("data/masked_mean_annual_sp.tif")
  
  # Align CRS if necessary
  if(crs(watersheds) != crs(mean_annual_sp)){
    polygon <- st_transform(watersheds, crs(mean_annual_sp))
  } else {
    polygon <- watersheds
  }
  
  # Convert to terra vector format
  polygon_terra <- vect(polygon)
  
  
  # OPTIMIZATION 2: Extract from the mean layer directly
  # This eliminates the need for pivot/group/summarize operations
  mean_sp_extracted <- extract(mean_annual_sp, polygon_terra, fun = mean, weights = TRUE, na.rm = TRUE)
  
  # OPTIMIZATION 3: Create 60% mask and calculate area fraction in one step
  # sp_60_mask <- mean_annual_sp > 60
  # 
  # terra::writeRaster(
  #   sp_60_mask,
  #   filename = "data/masked_sp_60.tif",
  #   overwrite = TRUE
  # )
 
  sp_60_mask <- terra::rast("data/masked_sp_60.tif")
  
  
  area60_fraction <- extract(sp_60_mask, polygon_terra, fun = mean, weights = TRUE, na.rm = TRUE) %>%
    rename(area60 = mean)
  
  # OPTIMIZATION 4: Simplified data frame construction
  watershed_sp <- watersheds %>%
    select(index) %>%
    st_drop_geometry() %>%
    mutate(
      mean_sp_2001_2020 = mean_sp_extracted$mean,
      area60 = area60_fraction$area60
    )
  
  return(watershed_sp)
}