library(tigris)
library(tmap)
library(terra)
library(tidyverse)
library(whitebox)
library(mapview)
library(sf)
library(nhdplusTools)
tmap_mode("view")

# grab CO state boundary
colorado <- tigris::states() %>% 
  filter(STUSPS == "CO")

# grab all HUC8s that intersect CO. We will use this as our 
# boundary for the app.
colorado_hucs <- get_huc(AOI = colorado, type = "huc08") %>%
  summarize()

# grab all NHD flowlines within the CO HUC8s:
nhd_flowlines <- nhdplusTools::get_nhdplus(AOI = colorado_hucs) %>%
  dplyr::select(comid, gnis_name) %>%
  st_transform(3857)
# save for use in the app:
saveRDS(nhd_flowlines, "data/for_app/nhd_flowlines_3857.RDS")

# use NHDPlus elevation dataset to create flow accumulation raster
# at a slightly better resolution than the published NHD flowlines (for baby watershed
# delineation)

# step 1: grab and transform all the raw NHD elevation rasters

raster_projector <- function(zone){
  # load then save the raster for whitebox
  raw_dem <- terra::rast(paste0("data/NHDPlusDEM/Ned", zone, "/elev_cm/")) %>%
    terra::crop(., st_transform(colorado_hucs, crs = st_crs(.)), mask = TRUE) %>%
    # for leaflet map to play nice, transforming all layers to 3857:
    terra::project("epsg:3857") 
  # save (temp backup)
  terra::writeRaster(raw_dem, paste0("data/temp/raw_dem_", zone, ".tif"), overwrite = TRUE)
  
  return(raw_dem) 
}

c("10b", "10c", "10d", "11b", "11d",  "13a" , "14a", "14b") %>%
  walk(~raster_projector(zone = .x))

# mosaic the raw NHD rasters into single raster
mosaic_rasters <- function(raster_list) {
  
  # start with the first raster
  result <- raster_list[[1]]
  
  # add each subsequent raster using mosaic
  if(length(raster_list) > 1) {
    for(i in 2:length(raster_list)) {
      result <- terra::mosaic(result, raster_list[[i]], fun = "first")
    }
  }
  
  return(result)
}

merged_elev <- list.files("data/temp/", 
                          full.names = TRUE) %>%
  map(~terra::rast(.)) %>%
  mosaic_rasters()
writeRaster(merged_elev, "data/temp/raw_dem_3857.tif", overwrite = TRUE)

# With our single NHD elevation raster now developed, use it to make
# flow accumulation grid with {whitebox}:

# fill depressions
wbt_fill_depressions(dem = "data/temp/raw_dem_3857.tif",
                     output = "data/temp/fill_dem_3857.tif")

# compute D8 flow direction
wbt_d8_pointer(dem = "data/temp/fill_dem_3857.tif",
               output = "data/for_app/flow_dir_3857.tif")

# compute flow accumulation
wbt_d8_flow_accumulation(input = "data/for_app/flow_dir_3857.tif",
                         output = "data/for_app/flow_acc_3857.tif",
                         pntr = TRUE,
                         out_type = "cells")

# read the flow accumulation back in...
flow_acc <- terra::rast("data/for_app/flow_acc_3857.tif") 

# ... to select only grid cells that represent "streams"...
streams_raster <- flow_acc > 500
plot(streams_raster)
# ... then save
writeRaster(streams_raster, "data/for_app/flow_network_3857.tif", overwrite = TRUE)

# load back in the flow_dir raster:
flow_dir_raster <- terra::rast("data/for_app/flow_dir_3857.tif")

# create pour point raster (same extent/res as flow_dir, just empty for use 
# in app):
pour_point_raster <- flow_dir_raster
values(pour_point_raster) <- NA
writeRaster(pour_point_raster, "data/for_app/pour_point_raster_3857.tif")

# TESTING

# Use Fort Collins for testing the results
streams_raster <-  terra::rast("data/for_app/flow_network_3857.tif")

town <-  tigris::places(state = "CO") %>%
  filter(NAME == "Fort Collins") %>%
  sf::st_transform(., sf::st_crs(streams_raster))

nhd_town <- get_nhdplus(AOI = town)

town_streams <- streams_raster %>%
  terra::crop(., town, mask = TRUE)

town_flow_dir <- flow_dir_raster %>%
  terra::crop(., town, mask = TRUE)

# does the output look appropriate?
tm_shape(town_streams) +
  tm_raster(style = "cat", 
            palette = "viridis",
            title = "Flow Raster") +
  tm_shape(nhd_town) +
 # tm_lines(col = "red", lwd = 1, legend.show = FALSE) +
  tm_layout(main.title = "",
            main.title.position = "center",
            legend.outside = TRUE,
            legend.outside.position = "right",
            frame = TRUE) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position = c("right", "top"))
#... it does!

# fin - we now have all necessary flowlines and rasters for the app's watershed delineation functionality.