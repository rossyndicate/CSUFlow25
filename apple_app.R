library(shiny)
library(leaflet)
library(terra)
library(sf)
library(whitebox)
library(bslib)
library(shinyjs)
library(dplyr)
library(readr)
library(nhdplusTools)
library(purrr)
library(data.table)
library(tidyverse)

# Load all grabber functions
list.files("src/", full.names = TRUE) %>% walk(~source(.))

wbt_options(max_procs = parallel::detectCores() - 1)
terraOptions(memfrac = 0.8, todisk = TRUE)

raster_data <- rast("data/for_app/flow_network_3857.tif")

if (!dir.exists("data/for_app/temp")) {
  dir.create("data/for_app/temp", recursive = TRUE)
}

ui <- page_fluid(
  useShinyjs(),
  h2("Watershed Delineation Tool"),
  p("Click a point on the map to select a pour point. Confirm before delineation."),
  fluidRow(
    column(3, actionButton("clear_btn", "Clear Watershed", icon = icon("trash"))),
    column(3, actionButton("help_btn", "Help / Directions", icon = icon("info-circle"))),
    column(3, downloadButton("download_watershed_data", "Download Watershed Data")),
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
        <li>Confirm the point to begin watershed delineation.</li>
        <li>The watershed will be displayed in red.</li>
        <li>Zoom in to view raster stream network at scale 15+.</li>
        <li>Use the 'Clear Watershed' button to reset.</li>
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
          <li>Confirm the point to begin watershed delineation.</li>
          <li>The watershed will be displayed in red.</li>
          <li>Zoom in to view raster stream network at scale 15+.</li>
          <li>Use the 'Clear Watershed' button to reset.</li>
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
  
  observeEvent(input$confirm_click, {
    removeModal()
    req(click_point())
    test_site <- click_point()
    test_site_3857 <- st_transform(test_site, 3857)
    buffer <- st_buffer(test_site_3857, 100000)
    
    flow_dir_raster <- rast("data/for_app/flow_dir_3857.tif")
    pour_point_template <- rast("data/for_app/pour_point_raster_3857.tif")
    
    flow_dir_crop <- crop(flow_dir_raster, buffer)
    flow_dir_path <- "data/for_app/temp/flow_dir_cropped.tif"
    writeRaster(flow_dir_crop, flow_dir_path, overwrite = TRUE, datatype = "INT1U")
    
    pour_point <- rast(ext(flow_dir_crop), resolution = res(pour_point_template), crs = crs(pour_point_template))
    pour_point <- setValues(pour_point, 0)
    coords <- st_coordinates(test_site_3857)
    pour_point[cellFromXY(pour_point, coords)] <- 1
    pour_point_path <- "data/for_app/temp/pour_point.tif"
    writeRaster(pour_point, pour_point_path, overwrite = TRUE, datatype = "INT1U", gdal = "COMPRESS=LZW")
    
    wbt_watershed(d8_pntr = flow_dir_path, pour_pts = pour_point_path, output = "data/for_app/temp/watershed.tif")
    
    watershed <- rast("data/for_app/temp/watershed.tif")
    watershed[is.na(watershed)] <- NA
    watershed_poly <- as.polygons(watershed, na.rm = TRUE, dissolve = TRUE, simplify = TRUE)
    watershed_sf <- st_as_sf(watershed_poly)[, "geometry"] %>%
      mutate(site_no = "User Watershed")
    
    # ---- Variable Extraction ----
    comids <- get_nhdplus(AOI = test_site) %>% pull(comid)
    
    streamcat_sim <- watershed_sf %>%
      mutate(comid = comids) %>%
      simple_streamcat_data() %>%
      mutate(across(where(is.numeric), ~ifelse(is.nan(.), 0, .))) %>%
      mutate(across(where(is.numeric), ~ifelse(. < 0, 0, .))) %>%
      st_drop_geometry() %>%
      rename_with(~ paste0(., "_streamcat"), -site_no) %>%
      select(-c(comid_streamcat))
    
    ws_vars <- watershed_sf %>%
      mutate(ws_area_sqkm = as.numeric(st_area(.) / 1e6)) %>%
      left_join(aspect_grabber(watershed_sf) %>% st_drop_geometry(), by = "site_no") %>%
      left_join(daymet_grabber(watershed_sf) %>% st_drop_geometry(), by = "site_no") %>%
      left_join(snow_persistence_grabber(watershed_sf) %>% st_drop_geometry(), by = "site_no") %>%
      left_join(geology_grabber(watershed_sf) %>% st_drop_geometry(), by = "site_no") %>%
      left_join(dam_grabber(watershed_sf) %>% st_drop_geometry(), by = "site_no") %>%
      left_join(nlcd_grabber(watershed_sf) %>% st_drop_geometry(), by = "site_no") %>%
      left_join(road_density_grabber(watershed_sf) %>% st_drop_geometry(), by = "site_no") %>%
      left_join(streamcat_sim, by = "site_no") %>%
      rename(index = site_no)
    
    watershed_data(ws_vars)
    
    leafletProxy("map") %>%
      clearGroup("watershed") %>%
      addPolygons(data = st_transform(watershed_sf, 4326), color = "red", weight = 2, fillOpacity = 0.3, group = "watershed")
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
