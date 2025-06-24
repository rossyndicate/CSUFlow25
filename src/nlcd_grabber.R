nlcd_grabber <- function(watersheds) {

  # Load NLCD raster and crop to watershed extent
  nlcd_raw <- terra::rast("data/nlcd/Annual_NLCD_LndCov_2019_CU_C1V0.tif")
  imper_raw <- terra::rast("data/nlcd/Annual_NLCD_FctImp_2019_CU_C1V0.tif")
  
  # crs(nlcd_raw) <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

  # 'GEOGCS["WGS 84",
  #   DATUM["WGS_1984",
  #       SPHEROID["WGS 84",6378137,298.257223563,
  #           AUTHORITY["EPSG","7030"]],
  #       AUTHORITY["EPSG","6326"]],
  #   PRIMEM["Greenwich",0,
  #       AUTHORITY["EPSG","8901"]],
  #   UNIT["degree",0.0174532925199433,
  #       AUTHORITY["EPSG","9122"]],
  #   AUTHORITY["EPSG","4326"]]'

  original <- watersheds

  # ensure both datasets have the same CRS
  if(crs(watersheds) != crs(nlcd_raw)){
    watersheds <- st_transform(watersheds, crs = crs(nlcd_raw))
  }

  # results <- vector("list", length = nrow(watersheds))

  # for(i in 1:nrow(watersheds)){
  #
  ws <- vect(watersheds)

  nlcd <- nlcd_raw %>% terra::crop(., ws, touches = TRUE)

  # Create masks for each land cover type

  # Forest: classes 41 (deciduous), 42 (evergreen), 43 (mixed)
  forest_mask <- nlcd %in% c(41, 42, 43)

  # Grassland/Herbaceous: class 71
  grassland_mask <- nlcd %in% c(71)

  # Shrub/Scrub: class 52
  shrub_mask <- nlcd %in% c(52)

  # Agriculture: classes 81 (pasture/hay), 82 (cultivated crops)
  agriculture_mask <- nlcd %in% c(81, 82)
  
  # Urban: low, medium, and high intensity
  urban_mask <- nlcd %in% c(22, 23, 24)

  # Calculate percentages for each land cover type
  forest_pct <- terra::zonal(forest_mask, ws, fun = "mean", weights = TRUE, na.rm = TRUE) * 100
  grassland_pct <- terra::zonal(grassland_mask, ws, fun = "mean", weights = TRUE, na.rm = TRUE) * 100
  shrub_pct <- terra::zonal(shrub_mask, ws, fun = "mean", weights = TRUE, na.rm = TRUE) * 100
  agriculture_pct <- terra::zonal(agriculture_mask, ws, fun = "mean", weights = TRUE, na.rm = TRUE) * 100
  urb_pct <- terra::zonal(urban_mask, ws, fun = "mean", weights = TRUE, na.rm = TRUE) * 100
  imp_pct <- terra::zonal(imper_raw, ws, fun = "mean", weights = TRUE, na.rm = TRUE) 
 
  
  # Combine results into a data frame
  results <- data.frame(
    original,
    #Watershed_ID = watersheds[,1],
    Forest_Percent = forest_pct[,1],
    Grassland_Percent = grassland_pct[,1],
    Shrub_Percent = shrub_pct[,1],
    Agriculture_Percent = agriculture_pct[,1],
    Impervious_Percent = imp_pct[,1]
  )

  return(results)

  }