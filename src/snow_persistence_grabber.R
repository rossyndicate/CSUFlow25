snow_persistence_grabber <- function(watersheds){
  
  # load file paths
  tif_files <- list.files("data/snow_persistence_hammond/", pattern = "\\.tif$", full.names = TRUE)
  
  # get extent of watersheds for initial cropping
  watershed_ext <- terra::ext(watersheds)
  
  # read first raster to check CRS
  first_raster <- terra::rast(tif_files[1])
  
  # align watershed CRS with raster if needed
  if(st_crs(watersheds) != terra::crs(first_raster)){
    polygon <- st_transform(watersheds, terra::crs(first_raster))
  } else {
    polygon <- watersheds
  }
  
  # convert to terra format
  polygon_terra <- terra::vect(polygon)
  watershed_ext_transformed <- terra::ext(polygon_terra)
  
  # process each raster individually - crop and mask early
  masked_rasters <- list()
  for(i in 1:length(tif_files)){
    # read raster
    r <- terra::rast(tif_files[i])
    
    # crop to watershed extent (fast rectangular crop)
    r_cropped <- terra::crop(r, watershed_ext_transformed)
    
    # mask to exact watershed boundaries
    r_masked <- terra::mask(r_cropped, polygon_terra)
    
    masked_rasters[[i]] <- r_masked
  }
  
  # stack the pre-masked rasters
  masked_stack <- terra::rast(masked_rasters)
  
  # extract mean SP across each watershed
  mean_sp <- terra::extract(masked_stack, polygon_terra, fun = mean, weights = TRUE)
  
  # calculate mean annual snow persistence for each cell across all years
  masked_mean_annual_sp <- terra::app(masked_stack, fun = mean, na.rm = TRUE)
  
  # create a binary raster where 1 = SP > 60%, 0 = SP <= 60%
  sp_60_mask <- masked_mean_annual_sp > 60
  
  # calculate the fraction of each watershed with SP > 60%
  area60_fraction <- terra::zonal(sp_60_mask, polygon_terra, fun = "mean", weights = TRUE, na.rm = TRUE) %>%
    dplyr::rename(area60 = mean)
  
  # convert the results to a data frame
  watershed_sp <- tibble::as_tibble(mean_sp) %>%
    pivot_longer(cols = c(contains("MOD"))) %>%
    group_by(ID) %>% 
    summarize(mean_sp_2001_2020 = mean(value)) %>%
    select(-ID) %>%
    bind_cols(watersheds, .) %>%
    bind_cols(area60_fraction)
  
  return(watershed_sp)
}
