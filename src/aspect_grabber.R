aspect_grabber <- function(watersheds){
  
  # create numerical representation for each cardinal aspect:
  aspect_lookup <- tibble::tibble(val = c(1, 2, 3, 4),
                          aspect = c("North", "East", "South", "West"))
  
  table <- vector("list", length = nrow(watersheds))
  
  for(i in 1:nrow(watersheds)){
    
    # filter our master list to just the gage's watershed we are iterating over
    site <- watersheds[i,]
    
    # grab elevation data
    elev <- elevatr::get_elev_raster(summarize(site), z = 12, clip = "locations") %>% # zoom of 12 is close-ish to 30 meters
      terra::rast() %>% 
      # clip to extent of the watershed
      terra::mask(., site, touches = FALSE)
    
    # calculate aspect from the masked elevation
    aspect_raw <- terra::terrain(elev, v = 'aspect', unit = 'radians')  # Store aspect in radians for calculations
    aspect_deg <- terra::terrain(elev, v = 'aspect')  # Store aspect in degrees for cardinal directions
    
    # calculate slope from the masked elevation (in degrees and radians)
    slope_deg <- terra::terrain(elev, v = 'slope', unit = 'degrees')
    slope_rad <- terra::terrain(elev, v = 'slope', unit = 'radians')
    
    # Convert slope from degrees to m/m
    slope_mm <- terra::app(slope_deg, function(x) tan(x * pi/180))
    
    # convert aspect values to cardinal directions
    convert_to_direction <- function(aspect) {
      direction <- rep(NA, length(aspect))
      direction[aspect >= 0 & aspect <= 45 | aspect > 315 & aspect <= 360] <- 1  # North
      direction[aspect > 45 & aspect <= 135] <- 2  # East
      direction[aspect > 135 & aspect <= 225] <- 3  # South
      direction[aspect > 225 & aspect <= 315] <- 4  # West
      return(direction)
    }
    
    # apply the conversion directly to the raster values
    aspect_cardinal_raster <- terra::app(aspect_deg, fun = convert_to_direction) 
    
    # Create a north-facing binary raster (North, Northwest, Northeast)
    north_facing <- terra::app(aspect_deg, function(x) {
      ifelse((x >= 0 & x <= 45) | (x >= 315 & x <= 360), 1, 0)
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
    dominant_aspect <- data.table::as.data.table(aspect_cardinal_raster) %>%
      dplyr::rename(val = lyr.1) %>%
      dplyr::group_by(val) %>%
      dplyr::summarize(count = n()) %>%
      dplyr::filter(count == max(count)) %>%
      dplyr::left_join(aspect_lookup, by = "val") %>%
      dplyr::mutate(site_no = site$site_no)
    
    # Calculate the requested statistics
    
    # 1. Mean slope in m/m
    mean_slope <- terra::global(slope_mm, fun = "mean", na.rm = TRUE)
    
    # 2. Fraction of drainage area with slopes > 0.3
    total_cells <- terra::global(slope_mm, fun = "notNA")
    steep_cells <- terra::global(steep_slopes, fun = "sum", na.rm = TRUE)
    slope30 <- steep_cells / total_cells
    
    # 3. Mean northness
    northness_mean <- terra::global(northness, fun = "mean", na.rm = TRUE)
    
    # 4. Fraction of drainage area with north-facing slopes
    north_cells <- terra::global(north_facing, fun = "sum", na.rm = TRUE)
    north_fraction <- north_cells / total_cells
    
    # Compile results
    table[[i]] <- tibble::tibble(
      site_no = site$site_no,
      dominant_aspect = dominant_aspect$aspect[1],
      mean_slope_mm = as.numeric(mean_slope),
      slope30 = as.numeric(slope30),
      northness_mean = as.numeric(northness_mean),
      north_fraction = as.numeric(north_fraction)
    )
    
    print(i)
    
  }
  
  watersheds <- watersheds %>%
    dplyr::left_join(., dplyr::bind_rows(table), by = "site_no")
  
  return(watersheds)
  
}