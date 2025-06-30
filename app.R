library(shiny)
library(leaflet)
library(sf)
library(bslib)
library(shinyjs)
library(nhdplusTools)
library(purrr)
library(data.table)
library(tidyverse)
library(terra)
library(lwgeom)

# Load all grabber functions
list.files("src/", full.names = TRUE) %>% walk(~source(.))

terraOptions(memfrac = 0.1, todisk = TRUE)

# Load flowlines data instead of raster
flowlines <- readRDS("data/nhd_flowlines.RDS")

if (!dir.exists("temp_data")) {
  dir.create("temp_data", recursive = TRUE)
}

# Define lookup table for flow metrics
flow_metric_lookup <- c(
  "model_all_annual_mean_max_q_mmd" = "Qmax",
  "model_all_annual_mean_min_q_mmd" = "Qmin", 
  "model_all_annual_mean_q_mmd" = "Qdaily",
  "model_all_apr_q_mmd" = "Qapr",
  "model_all_dec_q_mmd" = "Qdec",
  "model_all_feb_q_mmd" = "Qfeb",
  "model_all_jan_q_mmd" = "Qjan",
  "model_all_jul_q_mmd" = "Qjul",
  "model_all_jun_q_mmd" = "Qjun",
  "model_all_mar_q_mmd" = "Qmar",
  "model_all_may_q_mmd" = "Qmay",
  "model_all_nov_q_mmd" = "Qnov",
  "model_all_oct_q_mmd" = "Qoct",
  "model_all_sep_q_mmd" = "Qsep",
  "model_all_aug_q_mmd" = "Qaug",
  "model_all_q_ann_mm" = "Qann",
  "model_q_ann_mm" = "Qann",
  "model_ann" = "Qann",
  "model_all_mean_flowdate_0.1" = "Day10",
  "model_all_mean_flowdate_0.2" = "Day20",
  "model_all_mean_flowdate_0.3" = "Day30",
  "model_all_mean_flowdate_0.4" = "Day40",
  "model_all_mean_flowdate_0.5" = "Day50",
  "model_all_mean_flowdate_0.6" = "Day60",
  "model_all_mean_flowdate_0.7" = "Day70",
  "model_all_mean_flowdate_0.8" = "Day80",
  "model_all_mean_flowdate_0.9" = "Day90",
  "model_all_q5_q_mmd" = "Q5",
  "model_all_q95_q_mmd" = "Q95",
  "model_all_monsoon_frac" = "Monsoon",
  "model_flood_freq_1.5_q_mmd" = "Bankfull"
)

# Define units lookup
units_lookup <- c(
  "Qmax" = "mm/day", "Qmin" = "mm/day", "Qdaily" = "mm/day",
  "Qapr" = "mm/month", "Qdec" = "mm/month", "Qfeb" = "mm/month",
  "Qjan" = "mm/month", "Qjul" = "mm/month", "Qjun" = "mm/month",
  "Qmar" = "mm/month", "Qmay" = "mm/month", "Qnov" = "mm/month",
  "Qoct" = "mm/month", "Qsep" = "mm/month", "Qaug" = "mm/month",
  "Qann" = "mm/year", "Day10" = "day of water year", "Day20" = "day of water year",
  "Day30" = "day of water year", "Day40" = "day of water year", "Day50" = "day of water year",
  "Day60" = "day of water year", "Day70" = "day of water year", "Day80" = "day of water year",
  "Day90" = "day of water year", "Q5" = "mm/day", "Q95" = "mm/day",
  "Monsoon" = "fraction", "Bankfull" = "mm/day"
)

# Define desired order
desired_order <- c("Qann", "Qjan", "Qfeb", "Qmar", "Qapr", "Qmay", "Qjun", 
                   "Qjul", "Qaug", "Qsep", "Qoct", "Qnov", "Qdec", "Qmin", 
                   "Q5", "Qmax", "Q95", "Day10", "Day20", "Day30", "Day40", 
                   "Day50", "Day60", "Day70", "Day80", "Day90", "Monsoon", "Bankfull")

# Function to convert mm to CFS
convert_to_cfs <- function(mm_value, area_sqkm, unit_type, flow_stat = NULL) {
  # Constant: mm * km^2 to m^3
  volume_m3 <- mm_value * area_sqkm * 1e6 * 1e-3
  
  if (unit_type == "mm/day") {
    # m³/day → m³/s → ft³/s
    return(volume_m3 / 86400 * 35.3147)
    
  } else if (unit_type == "mm/month") {
    # Determine number of days in the month
    days_in_month <- case_when(
      flow_stat == "Qjan" ~ 31,
      flow_stat == "Qfeb" ~ 28.25,
      flow_stat == "Qmar" ~ 31,
      flow_stat == "Qapr" ~ 30,
      flow_stat == "Qmay" ~ 31,
      flow_stat == "Qjun" ~ 30,
      flow_stat == "Qjul" ~ 31,
      flow_stat == "Qaug" ~ 31,
      flow_stat == "Qsep" ~ 30,
      flow_stat == "Qoct" ~ 31,
      flow_stat == "Qnov" ~ 30,
      flow_stat == "Qdec" ~ 31,
      TRUE ~ 30.44  # average month length
    )
    return(volume_m3 / (days_in_month * 86400) * 35.3147)
    
  } else if (unit_type == "mm/year") {
    return(volume_m3 / (365.25 * 86400) * 35.3147)
    
  } else {
    return(NA_real_)  # Return numeric NA
  }
}


# Function to convert day of water year to date
day_to_date <- function(day_of_wy) {
  # Water year starts Oct 1
  start_date <- as.Date("2023-10-01") # Using 2023 as reference year
  return(start_date + day_of_wy - 1)
}

ui <- page_fluid(
  useShinyjs(),
  h2("CSUFlow25 Streamflow Prediction"),
  p("Click a point on the map to select a pour point. The point should be on a flowline, which you can see after zooming in to level 12. Confirm before delineation."),
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
  showModal(modalDialog(
    title = "Directions",
    easyClose = TRUE,
    footer = modalButton("Close"),
    HTML("
      <ul>
        <li>Click a location on the map to select a pour point. The point should be on a flowline, which you can see after zooming in to level 12+.</li>
        <li>Confirm the point to begin watershed delineation and flow statistics generation.</li>
        <li>The watershed will be displayed in red.</li>
        <li>Use the 'Clear Watershed' button to reset.</li>
        <li>Once the statistics have been calculated, use the 'Download Flow Stats' button to download.</li>
      </ul>
    ")
  ))
  
  zoom_level <- reactiveVal(7)
  click_point <- reactiveVal(NULL)
  watershed_data <- reactiveVal(NULL)
  delineated_watershed <- reactiveVal(NULL)  # Store delineated watershed for confirmation
  watershed_area <- reactiveVal(NULL)  # Store watershed area for CFS conversion
  
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
          <li>Click a location on the map to select a pour point. The point should be on a flowline, which you can see after zooming in to scale 12+.</li>
          <li>Confirm the point to begin watershed delineation and flow statistics generation.</li>
          <li>The watershed will be displayed in red.</li>
          <li>Use the 'Clear Watershed' button to reset.</li>
          <li>Once the statistics have been calculated, use the 'Download Flow Stats' button to download.</li>
        </ul>
      ")
    ))
  })
  
  observe({
    map_zoom <- input$map_zoom
    map_bounds <- input$map_bounds
    if (!is.null(map_zoom) && !is.null(map_bounds)) {
      zoom_level(map_zoom)
      if (map_zoom >= 12) {
        # Create bounding box for current view
        bbox <- st_bbox(c(xmin = map_bounds$west, ymin = map_bounds$south, 
                          xmax = map_bounds$east, ymax = map_bounds$north), 
                        crs = st_crs(4326))
        
        # Transform bbox to match flowlines CRS
        bbox_poly <- st_as_sfc(bbox) %>% 
          st_transform(st_crs(flowlines))
        
        tryCatch({
          # Filter flowlines to current map extent
          visible_flowlines <- st_filter(flowlines, bbox_poly)
          
          # Transform back to WGS84 for leaflet
          visible_flowlines_4326 <- st_transform(visible_flowlines, 4326)
          
          leafletProxy("map") %>%
            clearGroup("flowlines") %>%
            addPolylines(data = visible_flowlines_4326, 
                         color = "#0000FF", 
                         weight = 1.5, 
                         opacity = 0.7,
                         group = "flowlines")
        }, error = function(e) {
          message("Error filtering flowlines: ", e$message)
        })
      } else {
        leafletProxy("map") %>% clearGroup("flowlines")
      }
    }
  })
  
  output$zoom_level_text <- renderText({
    current_zoom <- zoom_level()
    if (current_zoom < 12) {
      paste("Current zoom level:", current_zoom, "- Zoom in closer to see flowlines")
    } else {
      paste("Current zoom level:", current_zoom, "- Flowlines visible")
    }
  })
  
  observeEvent(input$map_click, {
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
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_click", "Yes, Delineate")
        )
      ))
    }
  })
  
  # First confirmation: Delineate watershed
  observeEvent(input$confirm_click, {
    removeModal()
    req(click_point())
    test_site <- click_point()
    test_site_3857 <- st_transform(test_site, 3857)
    
    # Convert point to the format expected by get_split_catchment
    point_sfc <- st_geometry(test_site_3857)
    
    raindrop <- get_raindrop_trace(point = point_sfc, direction = "down")
    
    nearest_point <- st_as_sf(data.frame(x = raindrop$intersection_point[[1]][1],
                                         y = raindrop$intersection_point[[1]][2]),
                              coords = c("x", "y"),
                              crs = st_crs(raindrop)) %>%
      st_as_sfc() 
    
    # Delineate watershed using nhdplusTools
    better_termination <- get_split_catchment(point = nearest_point, upstream = TRUE)[2,]
    
    # Create watershed_sf with proper structure and fix geometry issues
    watershed_sf <- better_termination %>%
      mutate(index = "User Watershed") %>%
      select(index, geometry)
    
    # Fix any geometry issues
    watershed_sf <- watershed_sf %>%
      st_make_valid() %>%
      st_buffer(0)  # This can help fix topology issues
    
    # Calculate and store watershed area
    ws_area <- as.numeric(st_area(watershed_sf) / 1e6)  # Convert to km2
    watershed_area(ws_area)
    
    message("Watershed delineated successfully")
    
    # Store the delineated watershed for confirmation
    delineated_watershed(watershed_sf)
    
    # Display watershed on map
    leafletProxy("map") %>%
      clearGroup("watershed") %>%
      addPolygons(data = st_transform(watershed_sf, 4326), 
                  color = "red", weight = 2, fillOpacity = 0.3, group = "watershed")
    
    # Show confirmation dialog for watershed
    showModal(modalDialog(
      title = "Confirm Watershed Delineation",
      HTML("
        <p>The watershed has been delineated and is shown in red on the map.</p>
        <p><strong>Is this the correct watershed for your analysis?</strong></p>
        <p>If yes, the system will proceed with variable extraction and flow statistics calculation.</p>
        <p>If no, you can select a different pour point.</p>
      "),
      easyClose = FALSE,
      footer = tagList(
        actionButton("reject_watershed", "No, select different point", class = "btn btn-warning"),
        actionButton("confirm_watershed", "Yes, continue with analysis", class = "btn btn-success")
      )
    ))
    
    # Clean up temporary objects
    rm(better_termination)
    gc()
  })
  
  # Handle watershed rejection
  observeEvent(input$reject_watershed, {
    removeModal()
    
    # Clear the watershed display and stored data
    delineated_watershed(NULL)
    watershed_area(NULL)
    leafletProxy("map") %>%
      clearGroup("watershed")
    
    # Show message to user
    showNotification("Watershed cleared. Please select a new pour point.", 
                     duration = 3)
  })
  
  # Handle watershed confirmation and proceed with analysis
  observeEvent(input$confirm_watershed, {
    removeModal()
    req(delineated_watershed())
    
    watershed_sf <- delineated_watershed()
    test_site <- click_point()
    
    # Show progress modal with progress bar
    showModal(modalDialog(
      title = "Processing Watershed Analysis",
      div(
        h4("Extracting variables and running models..."),
        br(),
        div(id = "progress-text", "Initializing..."),
        br(),
        div(
          class = "progress",
          div(
            id = "progress-bar",
            class = "progress-bar progress-bar-striped progress-bar-animated",
            role = "progressbar",
            style = "width: 0%",
            "0%"
          )
        )
      ),
      easyClose = FALSE,
      footer = NULL
    ))
    
    # Function to update progress
    update_progress <- function(step, total_steps, message) {
      percent <- round((step / total_steps) * 100)
      runjs(sprintf(
        "document.getElementById('progress-bar').style.width = '%d%%';
         document.getElementById('progress-bar').innerHTML = '%d%%';
         document.getElementById('progress-text').innerHTML = '%s';",
        percent, percent, message
      ))
    }
    
    # Total steps: 13 data processing steps + models
    model_files <- list.files("data/models/", full.names = TRUE)
    # Filter out the excluded model
    model_files <- model_files[!grepl("all_annual_meanQ_mmd", model_files)]
    total_steps <- 13 + length(model_files)
    current_step <- 0
    
    # ---- Variable Extraction - Memory Optimized ----
    message("Starting variable extraction...")
    
    # Step 1: Initialize with basic watershed metrics
    current_step <- current_step + 1
    update_progress(current_step, total_steps, "Calculating watershed area...")
    
    ws_vars <- watershed_sf %>%
      st_make_valid() %>%  # Ensure valid geometry
      mutate(ws_area_sqkm = tryCatch({
        as.numeric(st_area(.) / 1e6)
      }, error = function(e) {
        message("Error calculating area, using simplified geometry")
        as.numeric(st_area(st_simplify(., dTolerance = 1)) / 1e6)
      }))
    
    message("Base watershed area calculated")
    gc() # Clean up after area calculation
    
    # Step 2: Extract COMID
    current_step <- current_step + 1
    update_progress(current_step, total_steps, "Extracting COMID for StreamCat data...")
    
    comids <- get_nhdplus(AOI = test_site) %>% pull(comid)
    message("COMID extracted: ", comids)
    
    # Step 3: Process StreamCat data
    current_step <- current_step + 1
    update_progress(current_step, total_steps, "Processing soils data...")
    
    streamcat_temp <- watershed_sf %>%
      mutate(comid = comids) %>%
      direct_streamcat_data() %>%
      mutate(across(where(is.numeric), ~ifelse(is.nan(.), 0, .))) %>%
      mutate(across(where(is.numeric), ~ifelse(. < 0, 0, .))) %>%
      st_drop_geometry() %>%
      rename_with(~ paste0(., "_streamcat"), -index) %>%
      select(-c(comid_streamcat))
    
    ws_vars <- ws_vars %>%
      left_join(streamcat_temp, by = "index")
    
    rm(streamcat_temp)
    gc()
    message("StreamCat data processed and added")
    
    # Step 4: Process aspect data
    current_step <- current_step + 1
    update_progress(current_step, total_steps, "Processing aspect data...")
    
    aspect_temp <- aspect_grabber(watershed_sf) %>% 
      st_drop_geometry()
    
    ws_vars <- ws_vars %>%
      left_join(aspect_temp, by = "index")
    
    rm(aspect_temp)
    gc()
    message("Aspect data processed and added")
    
    # Step 5: Process daymet data
    current_step <- current_step + 1
    update_progress(current_step, total_steps, "Processing SWE and precipitation data...")
    
    daymet_temp <- daymet_grabber(watershed_sf) %>% 
      st_drop_geometry()
    
    ws_vars <- ws_vars %>%
      left_join(daymet_temp, by = "index")
    
    rm(daymet_temp)
    gc()
    message("Daymet data processed and added")
    
    # Step 6: Process snow persistence data
    current_step <- current_step + 1
    update_progress(current_step, total_steps, "Processing snow persistence data...")
    
    snow_temp <- snow_persistence_grabber(watershed_sf) %>% 
      st_drop_geometry()
    
    ws_vars <- ws_vars %>%
      left_join(snow_temp, by = "index")
    
    rm(snow_temp)
    gc()
    message("Snow persistence data processed and added")
    
    # Step 7: Process geology data
    current_step <- current_step + 1
    update_progress(current_step, total_steps, "Processing geology data...")
    
    geology_temp <- geology_grabber(watershed_sf) %>% 
      st_drop_geometry()
    
    ws_vars <- ws_vars %>%
      left_join(geology_temp, by = "index")
    
    rm(geology_temp)
    gc()
    message("Geology data processed and added")
    
    # Step 8: Process dam data
    current_step <- current_step + 1
    update_progress(current_step, total_steps, "Processing dam data...")
    
    dam_temp <- dam_grabber(watershed_sf) %>% 
      st_drop_geometry()
    
    ws_vars <- ws_vars %>%
      left_join(dam_temp, by = "index")
    
    rm(dam_temp)
    gc()
    message("Dam data processed and added")
    
    # Step 9: Process NLCD data
    current_step <- current_step + 1
    update_progress(current_step, total_steps, "Processing land cover data...")
    
    nlcd_temp <- nlcd_grabber(watershed_sf) %>% 
      st_drop_geometry()
    
    ws_vars <- ws_vars %>%
      left_join(nlcd_temp, by = "index")
    
    rm(nlcd_temp)
    gc()
    message("NLCD data processed and added")
    
    # Step 10: Process road density data
    current_step <- current_step + 1
    update_progress(current_step, total_steps, "Processing road density data...")
    
    road_temp <- road_density_grabber(watershed_sf) %>% 
      st_drop_geometry()
    
    ws_vars <- ws_vars %>%
      left_join(road_temp, by = "index")
    
    rm(road_temp)
    gc()
    message("Road density data processed and added")
    
    # Step 11: Process fire data
    current_step <- current_step + 1
    update_progress(current_step, total_steps, "Processing fire history data...")
    
    fire_temp <- fire_grabber(watershed_sf) %>% 
      st_drop_geometry()
    
    ws_vars <- ws_vars %>%
      left_join(fire_temp, by = "index")
    
    rm(fire_temp)
    gc()
    message("Fire data processed and added")
    
    # Step 12: Process hydroregion data
    current_step <- current_step + 1
    update_progress(current_step, total_steps, "Processing hydroregion data...")
    
    hydro_temp <- find_dominant_hydroregion(watershed_sf)
    
    ws_vars <- ws_vars %>%
      left_join(hydro_temp, by = "index")
    
    rm(hydro_temp)
    gc()
    message("Hydroregion data processed and added")
    
    # Step 13: Process climate historic data
    current_step <- current_step + 1
    update_progress(current_step, total_steps, "Processing PET data...")
    
    climate_temp <- get_climate_historic_frac_overlap(watershed_sf)
    
    ws_vars <- ws_vars %>%
      left_join(climate_temp, by = "index")
    
    rm(climate_temp)
    gc()
    message("Climate historic data processed and added")
    
    # Final cleanup and standardization
    ws_vars <- ws_vars %>%
      rename_all(tolower)
    
    message("All variables processed successfully")
    gc() # Final garbage collection
    
    # Run prediction models with confidence intervals
    message("Running prediction models...")
    
    all_predictions <- map_dfr(model_files, function(file) {
      current_step <<- current_step + 1
      metric <- tools::file_path_sans_ext(basename(file))
      update_progress(current_step, total_steps, paste("Running models..."))
      
      tryCatch({
        model <- readRDS(file)
        
        # Get predictions with 95% confidence intervals
        preds <- predict(model, newdata = ws_vars, interval = "confidence", level = 0.95)
        
        # Handle different prediction output formats
        if(is.matrix(preds) && ncol(preds) >= 3) {
          # Standard matrix format with fit, lwr, upr columns
          prediction <- preds[,"fit"]^2      # Point prediction
          lower_ci <- preds[,"lwr"]^2        # Lower confidence interval
          upper_ci <- preds[,"upr"]^2        # Upper confidence interval
        } else {
          # Unknown format - log and skip
          message("Unknown prediction format for model: ", metric)
          return(NULL)
        }
        
        # Clean up model object immediately after use
        rm(model)
        gc()
        
        tibble(
          flow_metric = metric %>% tolower(), 
          prediction = prediction,
          lower_95ci = pmax(0, lower_ci),  # Ensure non-negative
          upper_95ci = upper_ci
        )
        
      }, error = function(e) {
        message("Error processing model ", metric, ": ", e$message)
        return(NULL)
      })
    }) %>%
      filter(!is.null(.))
    
    # Process and format the predictions
    formatted_predictions <- all_predictions %>%
      # Apply lookup table to rename metrics
      mutate(flow_stat = flow_metric_lookup[flow_metric]) %>%
      filter(!is.na(flow_stat)) %>%  # Remove any metrics not in lookup
      # Add units and area
      mutate(
        unit = units_lookup[flow_stat],
        area_sqkm = watershed_area()
      ) 

    # Original metric units (mm-based)
    mm_data <- formatted_predictions %>%
      filter(str_detect(unit, "mm/")) %>%
      mutate(
        value = round(prediction, 3),
        lower_ci = round(lower_95ci, 3),
        upper_ci = round(upper_95ci, 3)
      ) %>%
      select(flow_stat, unit, value, lower_ci, upper_ci) %>%
      mutate(across(everything(), as.character))
    
    # CFS conversions (only for flow metrics, not fractions or days)
    cfs_data <- formatted_predictions %>%
      filter(str_detect(unit, "mm/")) %>%  # Only convert mm-based units
      mutate(
        unit = "cfs",
        value = mapply(convert_to_cfs, prediction, area_sqkm, units_lookup[flow_stat], flow_stat),
        lower_ci = mapply(convert_to_cfs, lower_95ci, area_sqkm, units_lookup[flow_stat], flow_stat),
        upper_ci = mapply(convert_to_cfs, upper_95ci, area_sqkm, units_lookup[flow_stat], flow_stat)
      ) %>%
      mutate(value = round(value,3),
             lower_ci = round(lower_ci, 3),
             upper_ci = round(upper_ci, 3)) %>%
      select(flow_stat, unit, value, lower_ci, upper_ci) %>%
      mutate(across(everything(), as.character))
    
    # Date conversions (for Day metrics, add date equivalent rows)
    doy_data <- formatted_predictions %>%
      filter(str_starts(flow_stat, "Day")) %>%
      mutate(
        # flow_stat = flow_stat,
        unit = "day of water year",
        value = round(prediction),
        lower_ci = round(lower_95ci),
        upper_ci = round(upper_95ci)
      ) %>%
      select(flow_stat, unit, value, lower_ci, upper_ci) %>%
      mutate(across(everything(), as.character))
    
    # Water year date conversions (for Day* metrics)
    water_year_date_data <- formatted_predictions %>%
      filter(str_starts(flow_stat, "Day")) %>%
      mutate(
        #flow_stat,
        unit = "date (water year)",
        value = as.character(day_to_date(round(prediction))),
        lower_ci = as.character(day_to_date(round(lower_95ci))),
        upper_ci = as.character(day_to_date(round(upper_95ci)))
      ) %>%
      select(flow_stat, unit, value, lower_ci, upper_ci) %>%
      mutate(across(everything(), as.character))
    
    
    
    # Combine all data
    formatted_predictions <- bind_rows(mm_data, cfs_data, doy_data, water_year_date_data) %>%
      # Create ordering for flow_stat within each unit type
      mutate(
        flow_stat_order = case_when(
          flow_stat %in% desired_order ~ match(flow_stat, desired_order),
          TRUE ~ 999
        )
      ) %>%
      arrange(flow_stat_order, unit) %>%
      select(-flow_stat_order) #%>%
    
    watershed_data(formatted_predictions)
    message("Model predictions completed")
    
    # Close progress modal and show completion notification
    removeModal()
    showNotification("Analysis complete! You can now download the flow statistics.", 
                     duration = 5, type = "message")
    
    message("Watershed processing complete!")
    
    # Final cleanup
    rm(ws_vars, all_predictions, formatted_predictions)
    gc()
  })
  
  output$download_watershed_data <- downloadHandler(
    filename = function() {
      paste0("watershed_flow_stats_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(watershed_data(), file)
    }
  )
  
  observeEvent(input$clear_btn, {
    click_point(NULL)
    watershed_data(NULL)
    delineated_watershed(NULL)  # Clear the stored watershed
    watershed_area(NULL)  # Clear stored area
    leafletProxy("map") %>%
      clearGroup("watershed") %>%
      clearGroup("click_point") %>%
      clearGroup("flowlines")
  })
}

shinyApp(ui = ui, server = server)