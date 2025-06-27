library(shiny)
library(leaflet)
library(sf)
library(whitebox)
library(bslib)
library(shinyjs)
library(nhdplusTools)
library(purrr)
library(data.table)
library(tidyverse)
library(terra)
library(future)
library(promises)

# Load all grabber functions
list.files("src/", full.names = TRUE) %>% walk(~source(.))

# CRITICAL PERFORMANCE SETTINGS
wbt_options(max_procs = 1, compress_rasters = TRUE, verbose = FALSE)

# Aggressive memory management for terra
terraOptions(
  memfrac = 0.3,        # Reduced from 0.6 - use only 30% of available memory
  todisk = TRUE,        # Always write to disk
  tempdir = "temp_data", # Use local temp directory
  progress = 0          # Disable progress bars for speed
)

# Set up future for async processing (optional)
plan(multisession, workers = 2)

# Pre-load and optimize raster data
raster_data <- rast("data/flow_network_3857.tif")

if (!dir.exists("temp_data")) {
  dir.create("temp_data", recursive = TRUE)
}

# Clean temp directory on startup
temp_files <- list.files("temp_data", full.names = TRUE)
if(length(temp_files) > 0) {
  unlink(temp_files, recursive = TRUE)
}

ui <- page_fluid(
  useShinyjs(),
  
  # Add loading indicators
  tags$head(
    tags$style(HTML("
      .loading {
        position: fixed;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        background: rgba(0,0,0,0.8);
        color: white;
        padding: 20px;
        border-radius: 5px;
        z-index: 9999;
        display: none;
      }
    ")),
    tags$script(HTML("
      $(document).on('shiny:busy', function(event) {
        $('.loading').show();
      });
      $(document).on('shiny:idle', function(event) {
        $('.loading').hide();
      });
    "))
  ),
  
  div(class = "loading", "Processing watershed... This may take several minutes."),
  
  h2("CSUFlow25 Streamflow Prediction"),
  p("Click a point on the map to select a pour point. Confirm before delineation."),
  
  # Add memory usage warning
  div(class = "alert alert-warning", 
      "Note: Large watersheds may take 5-10 minutes to process. Please be patient."),
  
  fluidRow(
    column(3, actionButton("clear_btn", "Clear Watershed", icon = icon("trash"))),
    column(3, actionButton("help_btn", "Help / Directions", icon = icon("info-circle"))),
    column(3, downloadButton("download_watershed_data", "Download Flow Stats")),
    column(3, textOutput("zoom_level_text"))
  ),
  card(
    full_screen = TRUE,
    card_header("Interactive Map"),
    leafletOutput("map", height = "600px")
  )
)

server <- function(input, output, session) {
  
  # Increase session timeout (if running on shinyapps.io)
  session$onSessionEnded(function() {
    # Clean up temp files when session ends
    temp_files <- list.files("temp_data", full.names = TRUE)
    if(length(temp_files) > 0) {
      unlink(temp_files, recursive = TRUE)
    }
    gc()
  })
  
  showModal(modalDialog(
    title = "Directions",
    easyClose = TRUE,
    footer = modalButton("Close"),
    HTML("
      <ul>
        <li>Click a location on the map to select a pour point.</li>
        <li>Confirm the point to begin watershed delineation and flow statistics generation.</li>
        <li>The watershed will be displayed in red.</li>
        <li>Zoom in to view raster stream network at scale 15+.</li>
        <li>Use the 'Clear Watershed' button to reset.</li>
        <li>Once the statistics have been calculated, use the 'Download Flow Stats' button to download.</li>
        <li><strong>WARNING: Large watersheds may take 5-10 minutes to process!</strong></li>
      </ul>
    ")
  ))
  
  zoom_level <- reactiveVal(7)
  click_point <- reactiveVal(NULL)
  watershed_data <- reactiveVal(NULL)
  processing <- reactiveVal(FALSE)  # Add processing flag
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      setView(lng = -105.5, lat = 39, zoom = 7)
  })
  
  observeEvent(input$help_btn, {
    showModal(modalDialog(
      title = "Directions",
      easyClose = TRUE,
      footer = modalButton("Close"),
      HTML("
        <ul>
          <li>Click a location on the map to select a pour point.</li>
          <li>Confirm the point to begin watershed delineation and flow statistics generation..</li>
          <li>The watershed will be displayed in red.</li>
          <li>Zoom in to view raster stream network at scale 15+.</li>
          <li>Use the 'Clear Watershed' button to reset.</li>
          <li>Once the statistics have been calculated, use the 'Download Flow Stats' button to download.</li>
          <li><strong>WARNING: Large watersheds may take 5-10 minutes to process!</strong></li>
        </ul>
      ")
    ))
  })
  
  # Optimized raster overlay with error handling and cleanup
  observe({
    map_zoom <- input$map_zoom
    map_bounds <- input$map_bounds
    
    if (!is.null(map_zoom) && !is.null(map_bounds)) {
      zoom_level(map_zoom)
      
      if (map_zoom >= 15) {
        tryCatch({
          # More conservative bounds calculation
          sw <- c(map_bounds$west, map_bounds$south)
          ne <- c(map_bounds$east, map_bounds$north)
          
          wgs84_to_web_mercator <- function(lng, lat) {
            x <- lng * 20037508.34 / 180
            y <- log(tan((90 + lat) * pi / 360)) / (pi / 180)
            y <- y * 20037508.34 / 180
            return(c(x, y))
          }
          
          sw <- wgs84_to_web_mercator(sw[1], sw[2])
          ne <- wgs84_to_web_mercator(ne[1], ne[2])
          
          # Smaller buffer to reduce memory usage
          buffer_factor <- 0.05  # Reduced from 0.1
          ext_zoom <- ext(sw[1], ne[1], sw[2], ne[2]) + 
            buffer_factor * c(-1, 1, -1, 1) * c(ne[1] - sw[1], ne[1] - sw[1], ne[2] - sw[2], ne[2] - sw[2])
          
          # Check if extent is reasonable size
          extent_area <- (ne[1] - sw[1]) * (ne[2] - sw[2])
          if(extent_area < 1e12) {  # Reasonable size threshold
            cropped_raster <- crop(raster_data, ext_zoom)
            
            leafletProxy("map") %>%
              clearImages() %>%
              addRasterImage(cropped_raster, opacity = 0.7, project = FALSE,
                             colors = colorFactor(c("transparent", "#0000FF"), c(0, 1), na.color = "transparent"))
            
            # Clean up immediately
            rm(cropped_raster)
            gc()
          }
        }, error = function(e) {
          message("Error cropping raster: ", e$message)
          # Clear images on error
          leafletProxy("map") %>% clearImages()
        })
      } else {
        leafletProxy("map") %>% clearImages()
      }
    }
  })
  
  output$zoom_level_text <- renderText({
    current_zoom <- zoom_level()
    if (current_zoom < 15) {
      paste("Current zoom level:", current_zoom, "- Zoom in closer to see raster data")
    } else {
      paste("Current zoom level:", current_zoom, "- Raster data visible")
    }
  })
  
  observeEvent(input$map_click, {
    # Prevent new clicks while processing
    if(processing()) {
      showNotification("Please wait, currently processing another watershed.", type = "warning")
      return()
    }
    
    click <- input$map_click
    if (!is.null(click)) {
      test_site <- st_sf(geometry = st_sfc(st_point(c(click$lng, click$lat)), crs = 4326))
      click_point(test_site)
      leafletProxy("map") %>%
        clearGroup("click_point") %>%
        addMarkers(data = test_site, group = "click_point")
      showModal(modalDialog(
        title = "Confirm Pour Point",
        "Do you want to delineate the watershed from this location?",
        br(), br(),
        strong("Warning: This process may take 5-10 minutes for large watersheds!"),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_click", "Yes, Delineate")
        )
      ))
    }
  })
  
  # HEAVILY OPTIMIZED watershed processing with timeout protection
  observeEvent(input$confirm_click, {
    removeModal()
    req(click_point())
    
    # Set processing flag
    processing(TRUE)
    
    # Show progress notification
    progress_id <- showNotification(
      "Starting watershed delineation... This may take several minutes.",
      duration = NULL,
      type = "message"
    )
    
    test_site <- click_point()
    test_site_3857 <- st_transform(test_site, 3857)
    
    tryCatch({
      # Smaller buffer for faster processing (trade accuracy for speed)
      buffer_size <- 50000  # Reduced from 100000
      buffer <- st_buffer(test_site_3857, buffer_size)
      
      # Update progress
      removeNotification(progress_id)
      progress_id <- showNotification("Loading raster data...", duration = NULL, type = "message")
      
      # Load rasters with memory optimization
      flow_dir_raster <- rast("data/flow_dir_3857.tif")
      pour_point_template <- rast("data/pour_point_raster_3857.tif")
      
      # Update progress
      removeNotification(progress_id)
      progress_id <- showNotification("Cropping flow direction data...", duration = NULL, type = "message")
      
      flow_dir_crop <- crop(flow_dir_raster, buffer)
      flow_dir_path <- "temp_data/flow_dir_cropped.tif"
      writeRaster(flow_dir_crop, flow_dir_path, overwrite = TRUE, datatype = "INT1U")
      
      # Clean up immediately
      rm(flow_dir_raster)
      gc()
      
      # Update progress
      removeNotification(progress_id)
      progress_id <- showNotification("Creating pour point...", duration = NULL, type = "message")
      
      pour_point <- rast(ext(flow_dir_crop), resolution = res(pour_point_template), crs = crs(pour_point_template))
      pour_point <- setValues(pour_point, 0)
      coords <- st_coordinates(test_site_3857)
      pour_point[cellFromXY(pour_point, coords)] <- 1
      pour_point_path <- "temp_data/pour_point.tif"
      writeRaster(pour_point, pour_point_path, overwrite = TRUE, datatype = "INT1U", gdal = "COMPRESS=LZW")
      
      # Clean up
      rm(pour_point, pour_point_template)
      gc()
      
      # Update progress
      removeNotification(progress_id)
      progress_id <- showNotification("Delineating watershed (this is the slowest step)...", duration = NULL, type = "message")
      
      wbt_watershed(d8_pntr = flow_dir_path, pour_pts = pour_point_path, output = "temp_data/watershed.tif")
      
      # Update progress
      removeNotification(progress_id)
      progress_id <- showNotification("Converting watershed to polygon...", duration = NULL, type = "message")
      
      watershed <- rast("temp_data/watershed.tif")
      watershed[is.na(watershed)] <- NA
      watershed_poly <- as.polygons(watershed, na.rm = TRUE, dissolve = TRUE, simplify = TRUE)
      watershed_sf <- st_as_sf(watershed_poly)[, "geometry"] %>%
        mutate(index = "User Watershed")
      
      # Clean up temporary raster objects from watershed delineation
      rm(flow_dir_crop, watershed, watershed_poly)
      gc()
      
      # Check watershed size and warn if too large
      ws_area <- as.numeric(st_area(watershed_sf) / 1e6)  # Convert to km²
      if(ws_area > 10000) {  # If larger than 10,000 km²
        showNotification(
          paste("Warning: Large watershed detected (", round(ws_area), "km²). Processing may take 10+ minutes."),
          duration = 10,
          type = "warning"
        )
      }
      
      # ---- Variable Extraction - Ultra Memory Optimized ----
      removeNotification(progress_id)
      progress_id <- showNotification("Extracting watershed variables (step 1 of 12)...", duration = NULL, type = "message")
      
      # Initialize with basic watershed metrics
      ws_vars <- watershed_sf %>%
        mutate(ws_area_sqkm = ws_area)
      
      gc() # Clean up after area calculation
      
      # Extract COMID for StreamCat data
      removeNotification(progress_id)
      progress_id <- showNotification("Getting NHD data (step 2 of 12)...", duration = NULL, type = "message")
      
      comids <- get_nhdplus(AOI = test_site) %>% pull(comid)
      
      # Process each data source with progress updates and aggressive cleanup
      removeNotification(progress_id)
      progress_id <- showNotification("Processing StreamCat data (step 3 of 12)...", duration = NULL, type = "message")
      
      streamcat_temp <- watershed_sf %>%
        mutate(comid = comids) %>%
        direct_streamcat_data() %>%
        mutate(across(where(is.numeric), ~ifelse(is.nan(.), 0, .))) %>%
        mutate(across(where(is.numeric), ~ifelse(. < 0, 0, .))) %>%
        st_drop_geometry() %>%
        rename_with(~ paste0(., "_streamcat"), -index) %>%
        select(-c(comid_streamcat))
      
      ws_vars <- ws_vars %>% left_join(streamcat_temp, by = "index")
      rm(streamcat_temp); gc()
      
      # Continue with other data sources, each with progress update and cleanup
      removeNotification(progress_id)
      progress_id <- showNotification("Processing aspect data (step 4 of 12)...", duration = NULL, type = "message")
      
      aspect_temp <- aspect_grabber(watershed_sf) %>% st_drop_geometry()
      ws_vars <- ws_vars %>% left_join(aspect_temp, by = "index")
      rm(aspect_temp); gc()
      
      removeNotification(progress_id)
      progress_id <- showNotification("Processing climate data (step 5 of 12)...", duration = NULL, type = "message")
      
      daymet_temp <- daymet_grabber(watershed_sf) %>% st_drop_geometry()
      ws_vars <- ws_vars %>% left_join(daymet_temp, by = "index")
      rm(daymet_temp); gc()
      
      removeNotification(progress_id)
      progress_id <- showNotification("Processing snow data (step 6 of 12)...", duration = NULL, type = "message")
      
      snow_temp <- snow_persistence_grabber(watershed_sf) %>% st_drop_geometry()
      ws_vars <- ws_vars %>% left_join(snow_temp, by = "index")
      rm(snow_temp); gc()
      
      removeNotification(progress_id)
      progress_id <- showNotification("Processing geology data (step 7 of 12)...", duration = NULL, type = "message")
      
      geology_temp <- geology_grabber(watershed_sf) %>% st_drop_geometry()
      ws_vars <- ws_vars %>% left_join(geology_temp, by = "index")
      rm(geology_temp); gc()
      
      removeNotification(progress_id)
      progress_id <- showNotification("Processing dam data (step 8 of 12)...", duration = NULL, type = "message")
      
      dam_temp <- dam_grabber(watershed_sf) %>% st_drop_geometry()
      ws_vars <- ws_vars %>% left_join(dam_temp, by = "index")
      rm(dam_temp); gc()
      
      removeNotification(progress_id)
      progress_id <- showNotification("Processing land cover data (step 9 of 12)...", duration = NULL, type = "message")
      
      nlcd_temp <- nlcd_grabber(watershed_sf) %>% st_drop_geometry()
      ws_vars <- ws_vars %>% left_join(nlcd_temp, by = "index")
      rm(nlcd_temp); gc()
      
      removeNotification(progress_id)
      progress_id <- showNotification("Processing road data (step 10 of 12)...", duration = NULL, type = "message")
      
      road_temp <- road_density_grabber(watershed_sf) %>% st_drop_geometry()
      ws_vars <- ws_vars %>% left_join(road_temp, by = "index")
      rm(road_temp); gc()
      
      removeNotification(progress_id)
      progress_id <- showNotification("Processing fire data (step 11 of 12)...", duration = NULL, type = "message")
      
      fire_temp <- fire_grabber(watershed_sf) %>% st_drop_geometry()
      ws_vars <- ws_vars %>% left_join(fire_temp, by = "index")
      rm(fire_temp); gc()
      
      removeNotification(progress_id)
      progress_id <- showNotification("Processing final environmental data (step 12 of 12)...", duration = NULL, type = "message")
      
      hydro_temp <- find_dominant_hydroregion(watershed_sf)
      ws_vars <- ws_vars %>% left_join(hydro_temp, by = "index")
      rm(hydro_temp); gc()
      
      climate_temp <- get_climate_historic_frac_overlap(watershed_sf)
      ws_vars <- ws_vars %>% left_join(climate_temp, by = "index")
      rm(climate_temp); gc()
      
      # Final cleanup and standardization
      ws_vars <- ws_vars %>% rename_all(tolower)
      gc()
      
      # Load and run models
      removeNotification(progress_id)
      progress_id <- showNotification("Running prediction models...", duration = NULL, type = "message")
      
      model_files <- list.files("data/models/", full.names = TRUE)
      
      all_predictions <- map_dfr(model_files, function(file) {
        model <- readRDS(file)
        metric <- tools::file_path_sans_ext(basename(file))
        preds <- predict(model, newdata = ws_vars)^2
        
        # Clean up model object immediately
        rm(model)
        gc()
        
        tibble(flow_metric = metric, prediction = preds)
      })
      
      watershed_data(all_predictions)
      
      # Update map with watershed polygon
      leafletProxy("map") %>%
        clearGroup("watershed") %>%
        addPolygons(data = st_transform(watershed_sf, 4326), 
                    color = "red", weight = 2, fillOpacity = 0.3, group = "watershed")
      
      # Success notification
      removeNotification(progress_id)
      showNotification(
        paste("Watershed processing completed successfully! Area:", round(ws_area), "km²"),
        duration = 5,
        type = "message"
      )
      
      # Final cleanup
      rm(ws_vars, all_predictions)
      gc()
      
    }, error = function(e) {
      # Error handling
      removeNotification(progress_id)
      showNotification(
        paste("Error processing watershed:", e$message),
        duration = 10,
        type = "error"
      )
      message("Watershed processing error: ", e$message)
    }, finally = {
      # Always reset processing flag
      processing(FALSE)
      
      # Clean up temp files
      temp_files <- list.files("temp_data", full.names = TRUE, pattern = "\\.(tif|shp)$")
      if(length(temp_files) > 0) {
        unlink(temp_files, recursive = TRUE)
      }
      gc()
    })
  })
  
  output$download_watershed_data <- downloadHandler(
    filename = function() {
      paste0("watershed_flow_predictions_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(watershed_data())
      write_csv(watershed_data(), file)
    }
  )
  
  observeEvent(input$clear_btn, {
    click_point(NULL)
    watershed_data(NULL)
    processing(FALSE)
    leafletProxy("map") %>%
      clearGroup("watershed") %>%
      clearGroup("click_point")
    
    # Clean up temp files
    temp_files <- list.files("temp_data", full.names = TRUE)
    if(length(temp_files) > 0) {
      unlink(temp_files, recursive = TRUE)
    }
    gc()
  })
}

shinyApp(ui = ui, server = server)