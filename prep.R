library(tigris)
library(tmap)
library(terra)
library(tidyverse)
library(whitebox)
library(mapview)
library(sf)
library(nhdplusTools)

# Set tmap to plot mode
tmap_mode("view")

# Filter for Fort Collins for testing the results later
town <-  tigris::places(state = "CO") %>%
  filter(NAME == "Fort Collins")

nhd_flowlines <- get_nhdplus(AOI = town)

# use NHDPlus elevation data to create flow accumulation raster
# at a slightly better resolution than NHD flowlines (for baby watershed
# delineation):
raster_flowliner <- function(zone){
  
  # load then save the raster for whitebox
  raw_dem <- terra::rast(paste0("data/NHDPlusDEM/Ned", zone, "/elev_cm/")) 
  # terra::crop(., sf::st_transform(town, crs = sf::st_crs(.)))
  terra::writeRaster(raw_dem, "data/temp/raw_dem.tif", overwrite = TRUE)
  
  # fill depressions
  wbt_fill_depressions(dem = "data/temp/raw_dem.tif",
                       output = "data/temp/filled_dem.tif")
  
  # compute D8 flow direction
  wbt_d8_pointer(dem = "data/temp/filled_dem.tif",
                 output = "data/temp/flow_dir.tif")
  
  # compute flow accumulation
  wbt_d8_flow_accumulation(input = "data/temp/flow_dir.tif",
                           output = "data/temp/flow_acc.tif",
                           pntr = TRUE,
                           out_type = "cells")
  
  # read the flow accumulation back in...
  flow_acc <- terra::rast("data/temp/flow_acc.tif") 
  
  # ... to select only grid cells that represent "streams"...
  streams_raster <- flow_acc > 500
  
  # delete all temp files
  file.remove(list.files("data/temp/", full.names = TRUE))
  
  # ... then save
  writeRaster(streams_raster, paste0("data/flowline_rasters/flowline_raster_", zone, ".tif"), overwrite = TRUE)
  
  print(paste0(zone, " done!"))
  
}

start = Sys.time()

c("10b", "10c", "10d", "11b", "11d", "14a", "14b") %>%
  walk(~raster_flowliner(zone = .x))

start - Sys.time()

# mosaic rasters into single raster
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


final_raster <- list.files("data/flowline_rasters/", 
                           full.names = TRUE) %>%
  map(~terra::rast(.)) %>%
  mosaic_rasters()

writeRaster(final_raster, "data/flowline_rasters/merged_flowline.tif", overwrite = TRUE)

test <- final_raster %>%
  terra::crop(., sf::st_transform(town, crs = sf::st_crs(.)), mask = TRUE)

# does the output look appropriate?
tm_shape(test) +
  tm_raster(style = "cat", 
            palette = "viridis",
            title = "Flow DEM") +
  tm_shape(nhd_flowlines) +
  tm_lines(col = "red", lwd = 1, legend.show = FALSE) +
  tm_layout(main.title = "",
            main.title.position = "center",
            legend.outside = TRUE,
            legend.outside.position = "right",
            frame = TRUE) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position = c("right", "top"))



