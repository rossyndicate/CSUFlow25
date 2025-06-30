aspect_grabber <- function(watersheds){
  
  # create numerical representation for each cardinal and intercardinal aspect:
  aspect_lookup <- tibble(val = c(1, 2, 3, 4, 5, 6, 7, 8),
                          aspect = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"))
  
  table <- vector("list", length = nrow(watersheds))
  
  for(i in 1:nrow(watersheds)){
    
  # filter our master list to just the gage's watershed we are iterating over
  site <- watersheds[i,]
  
  # grab elevation data
  elev <- elevatr::get_elev_raster(summarize(site), z = 12, clip = "locations") %>% # zoom of 12 is 30 meters
    terra::rast() %>% 
    # clip to extent of the watershed
    terra::mask(., site, touches = TRUE)
  
  mean_elevation <- terra::zonal(elev, vect(site), fun = "mean", weights = TRUE, na.rm = TRUE)

  # calculate aspect from the masked elevation
  aspect_raw <- terra::terrain(elev, v = 'aspect', unit = 'radians')  # Store aspect in radians for calculations
  aspect_deg <- terra::terrain(elev, v = 'aspect')  # Store aspect in degrees for cardinal directions
  
  # calculate slope from the masked elevation (in degrees and radians)
  slope_deg <- terra::terrain(elev, v = 'slope', unit = 'degrees')
  slope_rad <- terra::terrain(elev, v = 'slope', unit = 'radians')
  
  # Convert slope from degrees to m/m
  slope_mm <- terra::app(slope_deg, function(x) tan(x * pi/180))
  
  # convert aspect values to 8 directional categories
  convert_to_direction <- function(aspect) {
    direction <- rep(NA, length(aspect))
    direction[aspect >= 337.5 | aspect < 22.5] <- 1    # N (337.5-22.5)
    direction[aspect >= 22.5 & aspect < 67.5] <- 2     # NE (22.5-67.5)
    direction[aspect >= 67.5 & aspect < 112.5] <- 3    # E (67.5-112.5)
    direction[aspect >= 112.5 & aspect < 157.5] <- 4   # SE (112.5-157.5)
    direction[aspect >= 157.5 & aspect < 202.5] <- 5   # S (157.5-202.5)
    direction[aspect >= 202.5 & aspect < 247.5] <- 6   # SW (202.5-247.5)
    direction[aspect >= 247.5 & aspect < 292.5] <- 7   # W (247.5-292.5)
    direction[aspect >= 292.5 & aspect < 337.5] <- 8   # NW (292.5-337.5)
    return(direction)
  }
  
  # apply the conversion directly to the raster values
  aspect_cardinal_raster <- terra::app(aspect_deg, fun = convert_to_direction) 
  
  # Create a north-facing binary raster (North, Northwest, Northeast)
  north_facing <- terra::app(aspect_deg, function(x) {
    ifelse((x >= 337.5 | x < 22.5), 1, 0)
  })
  
  # Create a steep slope binary raster (slopes > 0.3 m/m)
  steep_slopes <- terra::app(slope_mm, function(x) {
    ifelse(x > 0.3, 1, 0)
  })
  
  # Calculate northness (sin(slope) * cos(aspect))
  northness <- terra::app(c(slope_rad, aspect_raw), function(x) {
    sin(x[1]) * cos(x[2])
  })
  
  # Map showing what this aspect layer looks like geospatially (if needed for debugging):
   # plot(aspect_cardinal_raster)
  
  # Calculate the mode (dom aspect) in each watershed
  dominant_aspect <- as.data.table(aspect_cardinal_raster) %>%
    rename(val = lyr.1) %>%
    group_by(val) %>%
    summarize(count = n()) %>%
    filter(count == max(count)) %>%
    left_join(aspect_lookup, by = "val") %>%
    mutate(index = site$index)
  
  # Calculate the requested statistics
  
  # 1. Mean slope in m/m
  mean_slope <- terra::zonal(slope_mm, vect(site), fun = "mean", weights = TRUE, na.rm = TRUE)
  
  # 2. Fraction of drainage area with slopes > 0.3
  total_cells <- terra::global(slope_mm, fun = "notNA")
  steep_cells <- terra::global(steep_slopes, fun = "sum", na.rm = TRUE)
  slope30 <- steep_cells / total_cells
  
  # 3. Mean northness
  northness_mean <- terra::zonal(northness, vect(site), fun = "mean", weights = TRUE, na.rm = TRUE)
  
  # 4. Fraction of drainage area with north-facing slopes
  north_cells <- terra::global(north_facing, fun = "sum", na.rm = TRUE)
  north_fraction <- north_cells / total_cells
  
  # Compile results
  table[[i]] <- tibble(
    index = site$index,
    dominant_aspect = dominant_aspect$aspect[1],
    mean_slope_mm = as.numeric(mean_slope),
    slope30 = as.numeric(slope30),
    northness_mean = as.numeric(northness_mean),
    north_fraction = as.numeric(north_fraction),
    mean_elevation = as.numeric(mean_elevation)
  )
  
  print(i)
  
  }
  
 watersheds <- bind_rows(table)

  return(watersheds)
 
}