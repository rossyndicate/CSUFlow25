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

# Load all grabber functions
list.files("src/", full.names = TRUE) %>% walk(~source(.))

wbt_options(max_procs = 1)
terraOptions(memfrac = 0.6, todisk = TRUE)

raster_data <- rast("data/flow_network_3857.tif")

if (!dir.exists("temp_data")) {
  dir.create("temp_data", recursive = TRUE)
}

ui <- page_fluid(
  useShinyjs(),
  h2("CSUFlow25 Streamflow Prediction"),
  p("Click a point on the map to select a pour point. Confirm before delineation."),
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
        <li>Click a location on the map to select a pour point.</li>
        <li>Confirm the point to begin watershed delineation and flow statistics generation.</li>
        <li>The watershed will be displayed in red.</li>
        <li>Zoom in to view raster stream network at scale 15+.</li>
        <li>Use the 'Clear Watershed' button to reset.</li>
        <li>Once the statistics have been calculated, use the 'Download Flow Stats' button to download.</li>
      </ul>
    ")
  ))
  
  zoom_level <- reactiveVal(7)
  click_point <- reactiveVal(NULL)
  watershed_data <- reactiveVal(NULL)
  
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
        </ul>
      ")
    ))
  })
  
  observe({
    map_zoom <- input$map_zoom
    map_bounds <- input$map_bounds
    if (!is.null(map_zoom) && !is.null(map_bounds)) {
      zoom_level(map_zoom)
      if (map_zoom >= 15) {
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
        ext_zoom <- ext(sw[1], ne[1], sw[2], ne[2]) + 0.1 * c(-1, 1, -1, 1) * c(ne[1] - sw[1], ne[1] - sw[1], ne[2] - sw[2], ne[2] - sw[2])
        tryCatch({
          cropped_raster <- crop(raster_data, ext_zoom)
          leafletProxy("map") %>%
            clearImages() %>%
            addRasterImage(cropped_raster, opacity = 0.7, project = FALSE,
                           colors = colorFactor(c("transparent", "#0000FF"), c(0, 1), na.color = "transparent"))
        }, error = function(e) {
          message("Error cropping raster: ", e$message)
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
  
  # observeEvent(input$confirm_click, {
  #   removeModal()
  #   req(click_point())
  #   test_site <- click_point()
  #   test_site_3857 <- st_transform(test_site, 3857)
  #   buffer <- st_buffer(test_site_3857, 100000)
  #   
  #   flow_dir_raster <- rast("data/flow_dir_3857.tif")
  #   pour_point_template <- rast("data/pour_point_raster_3857.tif")
  #   
  #   flow_dir_crop <- crop(flow_dir_raster, buffer)
  #   flow_dir_path <- "temp_data/flow_dir_cropped.tif"
  #   writeRaster(flow_dir_crop, flow_dir_path, overwrite = TRUE, datatype = "INT1U")
  #   
  #   pour_point <- rast(ext(flow_dir_crop), resolution = res(pour_point_template), crs = crs(pour_point_template))
  #   pour_point <- setValues(pour_point, 0)
  #   coords <- st_coordinates(test_site_3857)
  #   pour_point[cellFromXY(pour_point, coords)] <- 1
  #   pour_point_path <- "temp_data/pour_point.tif"
  #   writeRaster(pour_point, pour_point_path, overwrite = TRUE, datatype = "INT1U", gdal = "COMPRESS=LZW")
  #   
  #   wbt_watershed(d8_pntr = flow_dir_path, pour_pts = pour_point_path, output = "temp_data/watershed.tif")
  #   
  #   watershed <- rast("temp_data/watershed.tif")
  #   watershed[is.na(watershed)] <- NA
  #   watershed_poly <- as.polygons(watershed, na.rm = TRUE, dissolve = TRUE, simplify = TRUE)
  #   watershed_sf <- st_as_sf(watershed_poly)[, "geometry"] %>%
  #     mutate(index = "User Watershed")
  #   
  #   # ---- Variable Extraction ----
  #   comids <- get_nhdplus(AOI = test_site) %>% pull(comid)
  #   
  #   # watershed_sf <- st_read("for_testing/watersheds_shapefile_20250527.shp") %>%
  #   #   mutate(index = "User Watershed")  %>% .[1,] %>%
  #   #   select(index)
  #   # 
  #   # comids <- 17906125
  #   
  #   streamcat_sim <- watershed_sf %>%
  #     mutate(comid = comids) %>%
  #     direct_streamcat_data() %>%
  #     mutate(across(where(is.numeric), ~ifelse(is.nan(.), 0, .))) %>%
  #     mutate(across(where(is.numeric), ~ifelse(. < 0, 0, .))) %>%
  #     st_drop_geometry() %>%
  #     rename_with(~ paste0(., "_streamcat"), -index) %>%
  #     select(-c(comid_streamcat)) 
  #   
  #   ws_vars <- watershed_sf %>%
  #     mutate(ws_area_sqkm = as.numeric(st_area(.) / 1e6)) %>%
  #     left_join(aspect_grabber(watershed_sf) %>% st_drop_geometry(), by = "index") %>%
  #     left_join(daymet_grabber(watershed_sf) %>% st_drop_geometry(), by = "index") %>%
  #     left_join(snow_persistence_grabber(watershed_sf) %>% st_drop_geometry(), by = "index") %>%
  #     left_join(geology_grabber(watershed_sf) %>% st_drop_geometry(), by = "index") %>%
  #     left_join(dam_grabber(watershed_sf) %>% st_drop_geometry(), by = "index") %>%
  #     left_join(nlcd_grabber(watershed_sf) %>% st_drop_geometry(), by = "index") %>%
  #     left_join(road_density_grabber(watershed_sf) %>% st_drop_geometry(), by = "index") %>%
  #     left_join(fire_grabber(watershed_sf) %>% st_drop_geometry(), by = "index") %>%
  #     left_join(find_dominant_hydroregion(watershed_sf), by = "index") %>%
  #     left_join(get_climate_historic_frac_overlap(watershed_sf), by = "index") %>%
  #     left_join(streamcat_sim, by = "index") %>%
  #     rename_all(tolower)
  #   
  #   # Load in and run models on ws_vars
  #   model_files <- list.files("data/models/", full.names = TRUE)
  #   
  #   all_predictions <- map_dfr(model_files, function(file) {
  #     
  #     model <- readRDS(file)
  #     
  #     metric <- tools::file_path_sans_ext(basename(file))
  #     
  #     preds <- predict(model, newdata = ws_vars)^2
  #     
  #     tibble(flow_metric = metric, prediction = preds)
  #     
  #   })
  #   
  #   watershed_data(all_predictions)
  #   
  #  # watershed_data(ws_vars) when having var table
  #   
  #   leafletProxy("map") %>%
  #     clearGroup("watershed") %>%
  #     addPolygons(data = st_transform(watershed_sf, 4326), color = "red", weight = 2, fillOpacity = 0.3, group = "watershed")
  # })
  
  # Memory-optimized watershed variable extraction
  # Process one variable at a time with explicit garbage collection
  
  observeEvent(input$confirm_click, {
    removeModal()
    req(click_point())
    test_site <- click_point()
    test_site_3857 <- st_transform(test_site, 3857)
    buffer <- st_buffer(test_site_3857, 100000)
    
    flow_dir_raster <- rast("data/flow_dir_3857.tif")
    pour_point_template <- rast("data/pour_point_raster_3857.tif")
    
    flow_dir_crop <- crop(flow_dir_raster, buffer)
    flow_dir_path <- "temp_data/flow_dir_cropped.tif"
    writeRaster(flow_dir_crop, flow_dir_path, overwrite = TRUE, datatype = "INT1U")
    
    pour_point <- rast(ext(flow_dir_crop), resolution = res(pour_point_template), crs = crs(pour_point_template))
    pour_point <- setValues(pour_point, 0)
    coords <- st_coordinates(test_site_3857)
    pour_point[cellFromXY(pour_point, coords)] <- 1
    pour_point_path <- "temp_data/pour_point.tif"
    writeRaster(pour_point, pour_point_path, overwrite = TRUE, datatype = "INT1U", gdal = "COMPRESS=LZW")
    
    wbt_watershed(d8_pntr = flow_dir_path, pour_pts = pour_point_path, output = "temp_data/watershed.tif")
    
    watershed <- rast("temp_data/watershed.tif")
    watershed[is.na(watershed)] <- NA
    watershed_poly <- as.polygons(watershed, na.rm = TRUE, dissolve = TRUE, simplify = TRUE)
    watershed_sf <- st_as_sf(watershed_poly)[, "geometry"] %>%
      mutate(index = "User Watershed")
    
    # Clean up temporary raster objects from watershed delineation
    rm(flow_dir_raster, flow_dir_crop, pour_point, pour_point_template, watershed, watershed_poly)
    gc()
    
    # ---- Variable Extraction - Memory Optimized ----
    # Start with base watershed data
    message("Starting variable extraction...")
    
    # Initialize with basic watershed metrics
    ws_vars <- watershed_sf %>%
      mutate(ws_area_sqkm = as.numeric(st_area(.) / 1e6))
    
    message("Base watershed area calculated")
    gc() # Clean up after area calculation
    
    # Extract COMID for StreamCat data
    comids <- get_nhdplus(AOI = test_site) %>% pull(comid)
    message("COMID extracted: ", comids)
    
    # Process StreamCat data first (usually lighter on memory)
    message("Processing StreamCat data...")
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
    
    # Process aspect data
    message("Processing aspect data...")
    aspect_temp <- aspect_grabber(watershed_sf) %>% 
      st_drop_geometry()
    
    ws_vars <- ws_vars %>%
      left_join(aspect_temp, by = "index")
    
    rm(aspect_temp)
    gc()
    message("Aspect data processed and added")
    
    # Process daymet data
    message("Processing daymet data...")
    daymet_temp <- daymet_grabber(watershed_sf) %>% 
      st_drop_geometry()
    
    ws_vars <- ws_vars %>%
      left_join(daymet_temp, by = "index")
    
    rm(daymet_temp)
    gc()
    message("Daymet data processed and added")
    
    # Process snow persistence data
    message("Processing snow persistence data...")
    snow_temp <- snow_persistence_grabber(watershed_sf) %>% 
      st_drop_geometry()
    
    ws_vars <- ws_vars %>%
      left_join(snow_temp, by = "index")
    
    rm(snow_temp)
    gc()
    message("Snow persistence data processed and added")
    
    # Process geology data
    message("Processing geology data...")
    geology_temp <- geology_grabber(watershed_sf) %>% 
      st_drop_geometry()
    
    ws_vars <- ws_vars %>%
      left_join(geology_temp, by = "index")
    
    rm(geology_temp)
    gc()
    message("Geology data processed and added")
    
    # Process dam data
    message("Processing dam data...")
    dam_temp <- dam_grabber(watershed_sf) %>% 
      st_drop_geometry()
    
    ws_vars <- ws_vars %>%
      left_join(dam_temp, by = "index")
    
    rm(dam_temp)
    gc()
    message("Dam data processed and added")
    
    # Process NLCD data
    message("Processing NLCD data...")
    nlcd_temp <- nlcd_grabber(watershed_sf) %>% 
      st_drop_geometry()
    
    ws_vars <- ws_vars %>%
      left_join(nlcd_temp, by = "index")
    
    rm(nlcd_temp)
    gc()
    message("NLCD data processed and added")
    
    # Process road density data
    message("Processing road density data...")
    road_temp <- road_density_grabber(watershed_sf) %>% 
      st_drop_geometry()
    
    ws_vars <- ws_vars %>%
      left_join(road_temp, by = "index")
    
    rm(road_temp)
    gc()
    message("Road density data processed and added")
    
    # Process fire data
    message("Processing fire data...")
    fire_temp <- fire_grabber(watershed_sf) %>% 
      st_drop_geometry()
    
    ws_vars <- ws_vars %>%
      left_join(fire_temp, by = "index")
    
    rm(fire_temp)
    gc()
    message("Fire data processed and added")
    
    # Process hydroregion data
    message("Processing hydroregion data...")
    hydro_temp <- find_dominant_hydroregion(watershed_sf)
    
    ws_vars <- ws_vars %>%
      left_join(hydro_temp, by = "index")
    
    rm(hydro_temp)
    gc()
    message("Hydroregion data processed and added")
    
    # Process climate historic data
    message("Processing climate historic data...")
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
    
    # Load in and run models on ws_vars
    message("Running prediction models...")
    model_files <- list.files("data/models/", full.names = TRUE)
    
    all_predictions <- map_dfr(model_files, function(file) {
      model <- readRDS(file)
      metric <- tools::file_path_sans_ext(basename(file))
      preds <- predict(model, newdata = ws_vars)^2
      
      # Clean up model object immediately after use
      rm(model)
      gc()
      
      tibble(flow_metric = metric, prediction = preds)
    })
    
    watershed_data(all_predictions)
    message("Model predictions completed")
    
    # Update map with watershed polygon
    leafletProxy("map") %>%
      clearGroup("watershed") %>%
      addPolygons(data = st_transform(watershed_sf, 4326), 
                  color = "red", weight = 2, fillOpacity = 0.3, group = "watershed")
    
    message("Watershed processing complete!")
    
    # Final cleanup
    rm(ws_vars, all_predictions)
    gc()
  })
  
  output$download_watershed_data <- downloadHandler(
    filename = function() {
      paste0("watershed_with_vars_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(watershed_data(), file)
    }
  )
  
  observeEvent(input$clear_btn, {
    click_point(NULL)
    watershed_data(NULL)
    leafletProxy("map") %>%
      clearGroup("watershed") %>%
      clearGroup("click_point")
  })
}

shinyApp(ui = ui, server = server)
