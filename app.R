options(shiny.maxRequestSize = 200*1024^2)  # allow up to ~200 MB uploads

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
library(openxlsx)
library(tools)

# Load all grabber functions
list.files("src/", full.names = TRUE) %>% walk(~source(.))

terraOptions(memfrac = 0.1, todisk = TRUE)

# Load flowlines data instead of raster
flowlines <- readRDS("data/nhd_flowlines.RDS")

# Load metadata for watershed variables
metadata <- read_csv("data/watersheds_with_vars_meta.csv")

if (!dir.exists("temp_data")) dir.create("temp_data", recursive = TRUE)
if (!dir.exists("temp_data/uploaded_shapefile")) dir.create("temp_data/uploaded_shapefile", recursive = TRUE)

# Lookup: flow metrics -> short names
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

# Units lookup
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

# Desired order for final table
desired_order <- c("Qann", "Qjan", "Qfeb", "Qmar", "Qapr", "Qmay", "Qjun", 
                   "Qjul", "Qaug", "Qsep", "Qoct", "Qnov", "Qdec", "Qmin", 
                   "Q5", "Qmax", "Q95", "Day10", "Day20", "Day30", "Day40", 
                   "Day50", "Day60", "Day70", "Day80", "Day90", "Monsoon", "Bankfull")

# Convert mm to cfs
convert_to_cfs <- function(mm_value, area_sqkm, unit_type, flow_stat = NULL) {
  volume_m3 <- mm_value * area_sqkm * 1e6 * 1e-3
  if (unit_type == "mm/day") {
    return(volume_m3 / 86400 * 35.3147)
  } else if (unit_type == "mm/month") {
    days_in_month <- dplyr::case_when(
      flow_stat == "Qjan" ~ 31, flow_stat == "Qfeb" ~ 28.25,
      flow_stat == "Qmar" ~ 31, flow_stat == "Qapr" ~ 30,
      flow_stat == "Qmay" ~ 31, flow_stat == "Qjun" ~ 30,
      flow_stat == "Qjul" ~ 31, flow_stat == "Qaug" ~ 31,
      flow_stat == "Qsep" ~ 30, flow_stat == "Qoct" ~ 31,
      flow_stat == "Qnov" ~ 30, flow_stat == "Qdec" ~ 31,
      TRUE ~ 30.44
    )
    return(volume_m3 / (days_in_month * 86400) * 35.3147)
  } else if (unit_type == "mm/year") {
    return(volume_m3 / (365.25 * 86400) * 35.3147)
  }
  return(NA_real_)
}

day_to_date <- function(day_of_wy) {
  start_date <- as.Date("2023-10-01")
  date <- start_date + day_of_wy - 1
  return(format(date, "%B %d"))
}

ui <- page_fluid(
  useShinyjs(),
  
  # CSS for the off-network toggle panel
  tags$style(HTML("
    #off_network_panel {
      background-color: #fff8e1;
      border: 1px solid #ffe082;
      border-radius: 6px;
      padding: 10px 14px;
      margin-top: 8px;
    }
    .off-network-label {
      font-weight: 500;
      color: #5d4037;
    }
    .off-network-hint {
      font-size: 12px;
      color: #795548;
      margin-top: 4px;
    }
    .delineation-mode-badge {
      display: inline-block;
      font-size: 11px;
      font-weight: 600;
      padding: 2px 8px;
      border-radius: 10px;
      margin-left: 8px;
      vertical-align: middle;
    }
    .badge-standard {
      background: #e3f2fd;
      color: #1565c0;
    }
    .badge-offnetwork {
      background: #fff3e0;
      color: #e65100;
    }
  ")),
  
  h2("CSUFlow25 Streamflow Prediction"),
  p("Choose to upload your own watershed polygon or delineate from a map click. 
    If delineating, the point should be on a flowline (visible at zoom level 12+)."),
  
  fluidRow(
    column(4, 
           actionButton("clear_btn", "Start Over", 
                        icon = icon("refresh"), 
                        class = "btn btn-danger btn-lg",
                        style = "width: 100%;")),
    column(2, 
           actionButton("help_btn", "Help", 
                        icon = icon("info-circle"),
                        style = "width: 100%;")),
    column(3, 
           downloadButton("download_watershed_data", "Download Results",
                          style = "width: 100%;")),
    column(3, 
           textOutput("zoom_level_text")),
    conditionalPanel(
      condition = "input.input_method == 'upload'",
      fileInput("uploaded_file", "Upload watershed shapefile (.zip) or GeoJSON", 
                accept = c(".zip", ".geojson"))
    )
  ),
  
  # Off-network toggle: only shown when delineating AND zoom >= 12
  conditionalPanel(
    condition = "input.input_method == 'delineate' && output.show_off_network_toggle",
    div(
      id = "off_network_panel",
      fluidRow(
        column(8,
               tags$label(
                 class = "off-network-label",
                 "My stream of interest does not have an NHD flowline"
               ),
               div(
                 class = "off-network-hint",
                 "Use this for small tributaries or headwater streams not mapped in NHD. 
             Click anywhere in the small watershed."
               )
        ),
        column(4,
               div(
                 style = "padding-top: 6px;",
                 checkboxInput(
                   inputId = "off_network_mode",
                   label   = "Enable",
                   value   = FALSE
                 )
               )
        )
      )
    )
  ),
  
  card(
    full_screen = TRUE,
    card_header(
      "Interactive Map",
      # Badge showing current delineation mode — only visible during delineate workflow
      conditionalPanel(
        condition = "input.input_method == 'delineate'",
        uiOutput("delineation_mode_badge")
      )
    ),
    leafletOutput("map", height = "600px")
  ),
  
  # Watershed confirmation panel (below map)
  conditionalPanel(
    condition = "output.show_watershed_panel",
    card(
      card_header("Watershed Confirmation"),
      div(
        id = "watershed_confirmation_content",
        style = "padding: 20px;",
        htmlOutput("watershed_confirmation_html")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # --- Reactive state ---
  workflow_choice        <- reactiveVal(NULL)
  zoom_level             <- reactiveVal(7)
  click_point            <- reactiveVal(NULL)
  watershed_data         <- reactiveVal(NULL)
  watershed_attributes   <- reactiveVal(NULL)
  current_watershed      <- reactiveVal(NULL)
  watershed_area         <- reactiveVal(NULL)
  watershed_source       <- reactiveVal(NULL)
  show_confirmation_panel <- reactiveVal(FALSE)
  
  # Controls visibility of the off-network toggle (zoom >= 12 while in delineate mode)
  output$show_off_network_toggle <- reactive({
    !is.null(workflow_choice()) && 
      workflow_choice() == "delineate" && 
      zoom_level() >= 12
  })
  outputOptions(output, "show_off_network_toggle", suspendWhenHidden = FALSE)
  
  # Badge reflecting current delineation mode
  output$delineation_mode_badge <- renderUI({
    req(workflow_choice() == "delineate")
    if (isTRUE(input$off_network_mode)) {
      tags$span(class = "delineation-mode-badge badge-offnetwork", 
                icon("exclamation-triangle"), " Off-network mode")
    } else {
      tags$span(class = "delineation-mode-badge badge-standard", 
                icon("map-marker"), " Standard NHD mode")
    }
  })
  
  # Control visibility of watershed confirmation panel
  output$show_watershed_panel <- reactive({ show_confirmation_panel() })
  outputOptions(output, "show_watershed_panel", suspendWhenHidden = FALSE)
  
  # --- Helper: reset all app state ---
  reset_app_state <- function() {
    workflow_choice(NULL)
    zoom_level(7)
    click_point(NULL)
    watershed_data(NULL)
    watershed_attributes(NULL)
    current_watershed(NULL)
    watershed_area(NULL)
    watershed_source(NULL)
    show_confirmation_panel(FALSE)
    
    leafletProxy("map") %>%
      clearGroup("watershed") %>%
      clearGroup("click_point") %>%
      clearGroup("flowlines") %>%
      setView(lng = -105.5, lat = 39, zoom = 7)
    
    shinyjs::reset("uploaded_file")
    removeNotification(id = NULL)
  }
  
  # --- Method chooser modal (shown on startup) ---
  observe({
    showModal(modalDialog(
      title = "Choose Watershed Input Method",
      easyClose = FALSE,
      footer = NULL,
      radioButtons("input_method", "Select how you want to provide the watershed:",
                   choices = c("Delineate with map click" = "delineate",
                               "Upload my own shapefile/GeoJSON" = "upload")),
      actionButton("proceed_method", "Proceed")
    ))
  })
  
  observeEvent(input$proceed_method, {
    req(input$input_method)
    workflow_choice(input$input_method)
    removeModal()
    if (input$input_method == "upload") {
      showNotification("Upload a zipped shapefile (.zip) or a GeoJSON to continue.", type = "message")
    } else {
      showNotification("Click on the map to delineate. Zoom to level 12+ to see flowlines.", type = "message")
    }
  })
  
  # --- Map setup ---
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
          <li>Choose to upload a watershed or delineate with the map.</li>
          <li><strong>Standard delineation:</strong> Zoom to level 12+, click on a blue NHD flowline.</li>
          <li><strong>Off-network delineation:</strong> When zoomed to level 12+, enable the 
              <em>\"My stream of interest does not have an NHD flowline\"</em> toggle, then 
              click anywhere within the small headwater watershed. No need to be on a flowline.</li>
          <li>For upload: select a zipped shapefile or GeoJSON file.</li>
          <li>Once the watershed appears in red, confirm to run flow analysis or start over.</li>
          <li>Use 'Start Over' to reset and choose a new method or watershed.</li>
        </ul>
      ")
    ))
  })
  
  # --- Flowlines on zoom ---
  observe({
    map_zoom   <- input$map_zoom
    map_bounds <- input$map_bounds
    if (!is.null(map_zoom) && !is.null(map_bounds)) {
      zoom_level(map_zoom)
      if (map_zoom >= 12) {
        bbox <- st_bbox(c(xmin = map_bounds$west, ymin = map_bounds$south, 
                          xmax = map_bounds$east, ymax = map_bounds$north), 
                        crs = st_crs(4326))
        bbox_poly <- st_as_sfc(bbox) %>% st_transform(st_crs(flowlines))
        tryCatch({
          visible_flowlines     <- st_filter(flowlines, bbox_poly)
          visible_flowlines_4326 <- st_transform(visible_flowlines, 4326)
          leafletProxy("map") %>%
            clearGroup("flowlines") %>%
            addPolylines(data = visible_flowlines_4326, 
                         color = "#0000FF", weight = 1.5, opacity = 0.7,
                         group = "flowlines")
        }, error = function(e) message("Error filtering flowlines: ", e$message))
      } else {
        leafletProxy("map") %>% clearGroup("flowlines")
      }
    }
  })
  
  output$zoom_level_text <- renderText({
    cz <- zoom_level()
    if (cz < 12 && !is.null(workflow_choice()) && workflow_choice() == "delineate") {
      paste("Zoom:", cz, "— zoom in to see flowlines (level 12+)")
    } else if (!is.null(workflow_choice()) && workflow_choice() == "delineate") {
      paste("Zoom:", cz, "— flowlines visible")
    } else {
      paste("Zoom:", cz)
    }
  })
  
  # --- Map click: dispatch to correct delineation method ---
  observeEvent(input$map_click, {
    if (is.null(workflow_choice()) || workflow_choice() != "delineate") return(NULL)
    click <- input$map_click
    if (!is.null(click)) {
      test_site <- st_sf(geometry = st_sfc(st_point(c(click$lng, click$lat)), crs = 4326))
      click_point(test_site)
      leafletProxy("map") %>%
        clearGroup("click_point") %>%
        addMarkers(data = test_site, group = "click_point")
      
      # Route to the appropriate delineation function
      if (isTRUE(input$off_network_mode)) {
        do_delineation_off_network(test_site)
      } else {
        do_delineation(test_site)
      }
    }
  })
  
  # ---------------------------------------------------------------
  # DELINEATION METHOD 1: Standard NHD snap-and-split
  # ---------------------------------------------------------------
  do_delineation <- function(test_site) {
    showModal(modalDialog(
      title = "Delineating Watershed (Standard)",
      "Snapping to NHD flowline and delineating upstream watershed — please wait...",
      easyClose = FALSE,
      footer = NULL
    ))
    
    test_site_3857 <- st_transform(test_site, 3857)
    point_sfc      <- st_geometry(test_site_3857)
    
    tryCatch({
      raindrop      <- get_raindrop_trace(point = point_sfc, direction = "down")
      nearest_point <- st_as_sf(
        data.frame(x = raindrop$intersection_point[[1]][1],
                   y = raindrop$intersection_point[[1]][2]),
        coords = c("x", "y"), crs = st_crs(raindrop)
      ) %>% st_as_sfc()
      
      better_termination <- get_split_catchment(point = nearest_point, upstream = TRUE)[2, ]
      
      watershed_sf <- better_termination %>%
        mutate(index = "Delineated Watershed") %>%
        select(index, geometry) %>%
        st_make_valid() %>%
        st_buffer(0)
      
      ws_area <- as.numeric(st_area(watershed_sf) / 1e6)
      watershed_area(ws_area)
      current_watershed(watershed_sf)
      watershed_source("delineated")
      
      watershed_4326 <- st_transform(watershed_sf, 4326)
      leafletProxy("map") %>%
        clearGroup("watershed") %>%
        addPolygons(data = watershed_4326, 
                    color = "red", weight = 2, fillOpacity = 0.3, group = "watershed")
      bounds <- st_bbox(watershed_4326)
      leafletProxy("map") %>%
        fitBounds(bounds["xmin"], bounds["ymin"], bounds["xmax"], bounds["ymax"])
      
      removeModal()
      show_watershed_confirmation_panel(ws_area, "delineated")
      rm(better_termination); gc()
      
    }, error = function(e) {
      removeModal()
      showNotification(paste("Error delineating watershed:", e$message), type = "error", duration = 8)
    })
  }
  
  # ---------------------------------------------------------------
  # DELINEATION METHOD 2: Off-network (small / unmapped tributaries)
  # ---------------------------------------------------------------
  do_delineation_off_network <- function(test_site) {
    showModal(modalDialog(
      title = "Delineating Watershed (Off-network)",
      div(
        p("Detecting small tributary / unmapped stream area — this may take a moment..."),
        tags$ul(
          tags$li("Checking proximity to NHD flowlines"),
          tags$li("Building boundary-point ensemble"),
          tags$li("Selecting largest contributing catchment")
        )
      ),
      easyClose = FALSE,
      footer = NULL
    ))
    
    # Use EPSG:5070 (CONUS Albers, metre-based) as the single working CRS.
    # get_nhdplus and get_split_catchment both expect/return 4326, so we
    # reproject their outputs immediately after each call.
    working_crs <- 5070
    aoi <- st_transform(test_site, working_crs)
    
    tryCatch({
      # get_nhdplus returns 4326 — reproject to working CRS right away
      flowline_danger_zone <- get_nhdplus(AOI = st_transform(aoi, 4326),
                                          realization = "flowline") %>%
        st_transform(working_crs) %>%
        st_buffer(30)   # 30 m buffer, valid because we're in a metre CRS
      
      # Always use the boundary-point ensemble approach — build a 35-m buffer,
      # sample its boundary while avoiding the flowline corridor, then select
      # the largest split catchment from all candidate points.
      site_buffer <- aoi %>% st_buffer(35)
      
      boundary_points <- site_buffer %>%
        st_boundary() %>%
        st_cast("POINT") %>%
        mutate(point_id = row_number()) %>%
        st_difference(., flowline_danger_zone) %>%   # exclude flowline corridor
        filter(point_id %% 50 == 0) %>%              # thin to every 50th point
        bind_rows(aoi)                                # always include the original click
      
      # get_split_catchment expects 4326; reproject each point before calling,
      # then immediately bring the result back to working_crs
      splits <- vector("list", nrow(boundary_points))
      for (i in seq_len(nrow(boundary_points))) {
        pt_4326 <- st_transform(boundary_points[i, ], 4326) %>% st_as_sfc()
        splits[[i]] <- tryCatch(
          get_split_catchment(point = pt_4326) %>%
            st_transform(working_crs) %>%
            filter(is.na(catchmentID)) %>%
            st_make_valid() %>%
            mutate(area = as.numeric(st_area(.))),
          error = function(e) NULL
        )
      }
      
      splits <- bind_rows(Filter(Negate(is.null), splits))
      
      if (nrow(splits) == 0) stop("No valid catchment splits found near this location.")
      
      watershed_sf <- splits %>%
        filter(area == max(area)) %>%
        slice(1) %>%
        select(-any_of(c("catchmentID", "id"))) %>%
        mutate(index = "Off-network Watershed") %>%
        st_make_valid() %>%
        st_buffer(0)
      
      # Project back to 4326 for display; compute area in km²
      watershed_4326 <- st_transform(watershed_sf, 4326)
      ws_area        <- as.numeric(st_area(st_transform(watershed_sf, 3857)) / 1e6)
      
      watershed_area(ws_area)
      current_watershed(watershed_sf)
      watershed_source("delineated")
      
      leafletProxy("map") %>%
        clearGroup("watershed") %>%
        addPolygons(data = watershed_4326,
                    color = "orange", weight = 2, fillOpacity = 0.3, group = "watershed")
      bounds <- st_bbox(watershed_4326)
      leafletProxy("map") %>%
        fitBounds(bounds["xmin"], bounds["ymin"], bounds["xmax"], bounds["ymax"])
      
      removeModal()
      show_watershed_confirmation_panel(ws_area, "delineated (off-network)")
      gc()
      
    }, error = function(e) {
      removeModal()
      showNotification(
        paste("Off-network delineation failed:", e$message,
              "— try clicking closer to the stream head, or switch to standard mode."),
        type = "error", duration = 10
      )
    })
  }
  
  # --- Upload workflow ---
  observe({
    if (is.null(workflow_choice()) || workflow_choice() != "upload") return(NULL)
    req(input$uploaded_file)
    
    current_watershed(NULL)
    watershed_area(NULL)
    
    ext     <- tolower(tools::file_ext(input$uploaded_file$name))
    user_ws <- NULL
    read_err <- NULL
    
    up_dir <- file.path(tempdir(), paste0("ws_", as.integer(Sys.time())))
    dir.create(up_dir, recursive = TRUE, showWarnings = FALSE)
    
    tryCatch({
      if (ext == "zip") {
        unzip(input$uploaded_file$datapath, exdir = up_dir)
        shp_files <- list.files(up_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
        if (length(shp_files) == 0) stop("No .shp file found inside the ZIP.")
        pick_valid_shp <- function(paths) {
          for (p in paths) {
            base   <- tools::file_path_sans_ext(p)
            comp_ok <- all(file.exists(paste0(base, c(".dbf", ".shx"))))
            if (comp_ok) return(p)
          }
          return(paths[1])
        }
        shp_path <- pick_valid_shp(shp_files)
        user_ws  <- suppressWarnings(sf::st_read(dsn = shp_path, quiet = TRUE))
      } else if (ext == "geojson" || grepl("\\.geojson$", tolower(input$uploaded_file$name))) {
        user_ws <- suppressWarnings(sf::st_read(input$uploaded_file$datapath, quiet = TRUE))
      } else {
        stop("Unsupported file type. Upload a zipped shapefile (.zip) or a GeoJSON.")
      }
    }, error = function(e) read_err <<- e$message)
    
    if (is.null(user_ws)) {
      showNotification(paste0("Failed to read uploaded watershed: ", 
                              if (!is.null(read_err)) read_err else "unknown error"),
                       type = "error", duration = 8)
      return(NULL)
    }
    
    tryCatch({
      user_ws <- suppressWarnings(sf::st_zm(user_ws, drop = TRUE, what = "ZM"))
      gtype   <- unique(sf::st_geometry_type(user_ws))
      if (!any(grepl("POLYGON", gtype))) {
        stop(sprintf("Uploaded layer is not polygonal (geometry types: %s).",
                     paste(gtype, collapse = ", ")))
      }
      user_ws <- sf::st_make_valid(user_ws) %>%
        dplyr::mutate(dissolve_id = 1) %>%
        dplyr::group_by(dissolve_id) %>%
        dplyr::summarise(.groups = "drop")
      
      if (is.na(sf::st_crs(user_ws))) {
        showNotification("Warning: no CRS found; assuming EPSG:4326.", type = "warning")
        sf::st_crs(user_ws) <- 4326
      } else {
        user_ws <- sf::st_transform(user_ws, 4326)
      }
      
      if (!"index" %in% names(user_ws)) {
        user_ws <- dplyr::mutate(user_ws, index = "Uploaded Watershed") %>%
          dplyr::select(index, geometry = geometry)
      } else {
        user_ws <- dplyr::select(user_ws, index, geometry = geometry)
      }
      
      ws_area_km2 <- as.numeric(sf::st_area(sf::st_transform(user_ws, 3857)) / 1e6)
      current_watershed(user_ws)
      watershed_area(ws_area_km2)
      watershed_source("uploaded")
      
      leafletProxy("map") %>%
        clearGroup("watershed") %>%
        addPolygons(data = user_ws, color = "red", weight = 2, fillOpacity = 0.3, group = "watershed")
      bounds <- sf::st_bbox(user_ws)
      leafletProxy("map") %>%
        fitBounds(bounds["xmin"], bounds["ymin"], bounds["xmax"], bounds["ymax"])
      
      show_watershed_confirmation_panel(ws_area_km2, "uploaded")
      
    }, error = function(e) {
      showNotification(paste0("Problem preparing uploaded geometry: ", e$message),
                       type = "error", duration = 10)
    })
  })
  
  # --- Confirmation panel ---
  show_watershed_confirmation_panel <- function(ws_area, source_type) {
    show_confirmation_panel(TRUE)
    output$watershed_confirmation_html <- renderUI({
      div(
        h4(paste("Confirm", str_to_title(source_type), "Watershed")),
        p(paste("The watershed has been", source_type, "and is shown on the map.")),
        p(HTML(paste0("Estimated area: <strong>", round(ws_area, 2), " km²</strong>"))),
        p("Would you like to proceed with flow statistics calculation for this watershed?"),
        br(),
        div(
          style = "text-align: center;",
          actionButton("start_over_from_confirm", "Start Over", 
                       class = "btn btn-secondary", style = "margin-right: 10px;"),
          actionButton("choose_different", "Choose Different Watershed", 
                       class = "btn btn-warning", style = "margin-right: 10px;"),
          actionButton("confirm_watershed", "Yes, Calculate Flow Statistics", 
                       class = "btn btn-success")
        )
      )
    })
  }
  
  # --- Start Over handlers ---
  show_method_modal <- function() {
    showModal(modalDialog(
      title = "Choose Watershed Input Method",
      easyClose = FALSE,
      footer = NULL,
      radioButtons("input_method", "Select how you want to provide the watershed:",
                   choices = c("Delineate with map click" = "delineate",
                               "Upload my own shapefile/GeoJSON" = "upload")),
      actionButton("proceed_method", "Proceed")
    ))
    showNotification("Starting over — choose your watershed input method.", 
                     duration = 3, type = "message")
  }
  
  observeEvent(input$start_over,            { removeModal(); reset_app_state(); show_method_modal() })
  observeEvent(input$clear_btn,             { removeModal(); reset_app_state(); show_method_modal() })
  observeEvent(input$start_over_from_confirm, { reset_app_state(); show_method_modal() })
  
  observeEvent(input$choose_different, {
    current_watershed(NULL); watershed_area(NULL); watershed_source(NULL)
    click_point(NULL); watershed_data(NULL); watershed_attributes(NULL)
    show_confirmation_panel(FALSE)
    leafletProxy("map") %>% clearGroup("watershed") %>% clearGroup("click_point")
    if (!is.null(workflow_choice())) {
      if (workflow_choice() == "upload") {
        showNotification("Upload a different watershed file.", duration = 3, type = "message")
      } else {
        showNotification("Click a different location to delineate a new watershed.", 
                         duration = 3, type = "message")
      }
    }
  })
  
  # --- Analysis workflow ---
  observeEvent(input$confirm_watershed, {
    req(current_watershed())
    show_confirmation_panel(FALSE)
    do_analysis(current_watershed(), watershed_source())
  })
  
  observeEvent(input$cancel_analysis, {
    removeModal(); reset_app_state(); show_method_modal()
    showNotification("Analysis canceled.", duration = 3, type = "warning")
  })
  
  # --- Shared analysis function ---
  do_analysis <- function(watershed_sf, source_type) {
    showModal(modalDialog(
      title = "Processing Watershed Analysis",
      div(
        h4("Extracting variables and running models..."),
        br(),
        div(id = "progress-text", "Initializing..."),
        br(),
        div(class = "progress",
            div(id = "progress-bar",
                class = "progress-bar progress-bar-striped progress-bar-animated",
                role = "progressbar", style = "width: 0%", "0%")),
        br(),
        actionButton("cancel_analysis", "Cancel & Start Over", class = "btn btn-warning btn-sm")
      ),
      easyClose = FALSE,
      footer = NULL
    ))
    
    update_progress <- function(step, total_steps, message) {
      percent <- round((step / total_steps) * 100)
      runjs(sprintf(
        "document.getElementById('progress-bar').style.width = '%d%%';
         document.getElementById('progress-bar').innerHTML = '%d%%';
         document.getElementById('progress-text').innerHTML = '%s';",
        percent, percent, message
      ))
    }
    
    model_files  <- list.files("data/models/", full.names = TRUE)
    model_files  <- model_files[!grepl("all_annual_meanQ_mmd", model_files)]
    total_steps  <- 13 + length(model_files)
    current_step <- 0
    
    # Steps 1–13: variable extraction (unchanged from original)
    current_step <- current_step + 1
    update_progress(current_step, total_steps, "Calculating watershed area...")
    ws_vars <- watershed_sf %>%
      st_make_valid() %>%
      mutate(ws_area_sqkm = tryCatch({
        as.numeric(st_area(.) / 1e6)
      }, error = function(e) as.numeric(st_area(st_simplify(., dTolerance = 1)) / 1e6)))
    gc()
    
    current_step <- current_step + 1
    update_progress(current_step, total_steps, "Extracting COMID for StreamCat data...")
    if (source_type %in% c("uploaded", "delineated (off-network)")) {
      
      ws_4326   <- st_transform(watershed_sf, 4326)
      nearby_ws <- nhdplusTools::get_nhdplus(AOI = ws_4326, realization = "catchment") %>%
        left_join(., data.table::fread("data/COLORADO_SOIL_STREAMCAT.csv"),
                  by = c("featureid" = "comid")) %>%
        mutate(area_ws = st_area(.))
      
      sf_use_s2(FALSE)
      overlaps <- st_intersection(nearby_ws, ws_4326) %>%
        mutate(percent_overlap = as.numeric(st_area(.) / area_ws) * 100)
      sf_use_s2(TRUE)
      
      # Progressive threshold relaxation: 75 -> 50 -> 25 -> largest intersection
      comids <- NULL
      for (thresh in c(75, 50, 25)) {
        candidate <- overlaps %>%
          filter(percent_overlap > thresh) %>%
          filter(wsareasqkm == max(wsareasqkm, na.rm = TRUE))
        if (nrow(candidate) > 0) {
          comids <- candidate %>% pull(featureid)
          message("COMID selected at >", thresh, "% overlap: ", paste(comids, collapse = ", "))
          break
        }
      }
      # Last resort: catchment with the largest raw intersection area
      if (is.null(comids) || length(comids) == 0) {
        comids <- overlaps %>%
          mutate(int_area = as.numeric(st_area(.))) %>%
          filter(int_area == max(int_area)) %>%
          slice(1) %>%
          pull(featureid)
        message("COMID selected by largest intersection (no threshold met): ",
                paste(comids, collapse = ", "))
      }
      
    } else {
      comids <- tryCatch({
        if (!is.null(click_point())) {
          get_nhdplus(AOI = click_point()) %>% pull(comid)
        } else {
          centroid_pt <- st_centroid(st_transform(watershed_sf, 4326))
          get_nhdplus(AOI = centroid_pt) %>% pull(comid)
        }
      }, error = function(e) { message("COMID extraction failed: ", e$message) })
    }
    
    current_step <- current_step + 1
    update_progress(current_step, total_steps, "Processing soils/StreamCat data...")
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
    
    current_step <- current_step + 1
    update_progress(current_step, total_steps, "Processing aspect data...")
    aspect_temp <- aspect_grabber(watershed_sf) %>% st_drop_geometry()
    ws_vars <- ws_vars %>% left_join(aspect_temp, by = "index")
    rm(aspect_temp); gc()
    
    current_step <- current_step + 1
    update_progress(current_step, total_steps, "Processing SWE and precipitation data...")
    daymet_temp <- daymet_grabber(watershed_sf) %>% st_drop_geometry()
    ws_vars <- ws_vars %>% left_join(daymet_temp, by = "index")
    rm(daymet_temp); gc()
    
    current_step <- current_step + 1
    update_progress(current_step, total_steps, "Processing snow persistence data...")
    snow_temp <- snow_persistence_grabber(watershed_sf) %>% st_drop_geometry()
    ws_vars <- ws_vars %>% left_join(snow_temp, by = "index")
    rm(snow_temp); gc()
    
    current_step <- current_step + 1
    update_progress(current_step, total_steps, "Processing geology data...")
    geology_temp <- geology_grabber(watershed_sf) %>% st_drop_geometry()
    ws_vars <- ws_vars %>% left_join(geology_temp, by = "index")
    rm(geology_temp); gc()
    
    current_step <- current_step + 1
    update_progress(current_step, total_steps, "Processing dam data...")
    dam_temp <- dam_grabber(watershed_sf) %>% st_drop_geometry()
    ws_vars <- ws_vars %>% left_join(dam_temp, by = "index")
    rm(dam_temp); gc()
    
    current_step <- current_step + 1
    update_progress(current_step, total_steps, "Processing land cover data...")
    nlcd_temp <- nlcd_grabber(watershed_sf) %>% st_drop_geometry()
    ws_vars <- ws_vars %>% left_join(nlcd_temp, by = "index")
    rm(nlcd_temp); gc()
    
    current_step <- current_step + 1
    update_progress(current_step, total_steps, "Processing road density data...")
    road_temp <- road_density_grabber(watershed_sf) %>% st_drop_geometry()
    ws_vars <- ws_vars %>% left_join(road_temp, by = "index")
    rm(road_temp); gc()
    
    # current_step <- current_step + 1
    # update_progress(current_step, total_steps, "Processing fire history data...")
    # fire_temp <- fire_grabber(watershed_sf) %>% st_drop_geometry()
    # ws_vars <- ws_vars %>% left_join(fire_temp, by = "index")
    # rm(fire_temp); gc()
    
    current_step <- current_step + 1
    update_progress(current_step, total_steps, "Processing hydroregion data...")
    hydro_temp <- find_dominant_hydroregion(watershed_sf)
    ws_vars <- ws_vars %>% left_join(hydro_temp, by = "index")
    rm(hydro_temp); gc()
    
    current_step <- current_step + 1
    update_progress(current_step, total_steps, "Processing PET data...")
    climate_temp <- get_climate_historic_frac_overlap(watershed_sf)
    ws_vars <- ws_vars %>% left_join(climate_temp, by = "index")
    rm(climate_temp); gc()
    
    ws_vars <- ws_vars %>% st_drop_geometry() %>% rename_all(tolower)
    message("All variables processed successfully"); gc()
    
    # Model predictions
    all_predictions <- map_dfr(model_files, function(file) {
      current_step <<- current_step + 1
      metric <- tools::file_path_sans_ext(basename(file))
      update_progress(current_step, total_steps, "Running models...")
      tryCatch({
        model <- readRDS(file)
        preds <- predict(model, newdata = ws_vars, interval = "confidence", level = 0.95)
        if (is.matrix(preds) && ncol(preds) >= 3) {
          tibble(
            flow_metric = metric %>% tolower(),
            prediction  = preds[, "fit"]^2,
            lower_95ci  = preds[, "lwr"]^2,
            upper_95ci  = preds[, "upr"]^2
          )
        } else {
          message("Unknown prediction format for model: ", metric); NULL
        }
      }, error = function(e) { message("Error processing model ", metric, ": ", e$message); NULL })
    }) %>% filter(!is.null(.))
    
    formatted_predictions <- all_predictions %>%
      mutate(flow_stat = flow_metric_lookup[flow_metric]) %>%
      filter(!is.na(flow_stat)) %>%
      mutate(unit = units_lookup[flow_stat], area_sqkm = as.numeric(watershed_area()))
    
    mm_data <- formatted_predictions %>%
      filter(str_detect(unit, "mm/")) %>%
      mutate(value = round(prediction, 3), lower_ci = round(lower_95ci, 3),
             upper_ci = round(upper_95ci, 3)) %>%
      select(flow_stat, unit, value, lower_ci, upper_ci) %>%
      mutate(across(everything(), as.character))
    
    cfs_data <- formatted_predictions %>%
      filter(str_detect(unit, "mm/")) %>%
      mutate(unit = "cfs",
             value    = mapply(convert_to_cfs, prediction, area_sqkm, units_lookup[flow_stat], flow_stat),
             lower_ci = mapply(convert_to_cfs, lower_95ci,  area_sqkm, units_lookup[flow_stat], flow_stat),
             upper_ci = mapply(convert_to_cfs, upper_95ci,  area_sqkm, units_lookup[flow_stat], flow_stat)) %>%
      mutate(value = round(value, 3), lower_ci = round(lower_ci, 3), upper_ci = round(upper_ci, 3)) %>%
      select(flow_stat, unit, value, lower_ci, upper_ci) %>%
      mutate(across(everything(), as.character))
    
    doy_data <- formatted_predictions %>%
      filter(str_starts(flow_stat, "Day")) %>%
      mutate(unit = "day of water year",
             value = round(prediction), lower_ci = round(lower_95ci), upper_ci = round(upper_95ci)) %>%
      select(flow_stat, unit, value, lower_ci, upper_ci) %>%
      mutate(across(everything(), as.character))
    
    water_year_date_data <- formatted_predictions %>%
      filter(str_starts(flow_stat, "Day")) %>%
      mutate(unit = "date",
             value    = as.character(day_to_date(round(prediction))),
             lower_ci = as.character(day_to_date(round(lower_95ci))),
             upper_ci = as.character(day_to_date(round(upper_95ci)))) %>%
      select(flow_stat, unit, value, lower_ci, upper_ci) %>%
      mutate(across(everything(), as.character))
    
    other_data <- formatted_predictions %>%
      filter(flow_stat %in% c("Monsoon", "Bankfull")) %>%
      mutate(value = round(prediction, 3), lower_ci = round(lower_95ci, 3),
             upper_ci = round(upper_95ci, 3)) %>%
      select(flow_stat, unit, value, lower_ci, upper_ci) %>%
      mutate(across(everything(), as.character))
    
    pretty_ws_vars <- ws_vars %>%
      st_drop_geometry() %>%
      select(ws_area_sqkm, rckdepws_streamcat:impervious_percent,
             road_density_km_per_km2:avg_tot_pet) %>%
      select(-c(wsareasqkm_streamcat, geometry.y)) %>%
      mutate(across(everything(), as.character)) %>%
      select(where(~ !is.list(.))) %>%
      pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
      left_join(metadata %>% select(variable, units, description, source), by = "variable") %>%
      select(variable, value, units, description, source)
    
    final_preds <- bind_rows(mm_data, cfs_data, doy_data, water_year_date_data, other_data) %>%
      mutate(flow_stat_order = case_when(
        flow_stat %in% desired_order ~ match(flow_stat, desired_order), TRUE ~ 999
      )) %>%
      arrange(flow_stat_order, unit) %>%
      select(-flow_stat_order)
    
    watershed_data(final_preds)
    watershed_attributes(pretty_ws_vars)
    
    removeModal()
    showNotification("Analysis complete! You can now download the flow statistics.", 
                     duration = 5, type = "message")
    rm(ws_vars, all_predictions, formatted_predictions, final_preds); gc()
  }
  
  # --- Download ---
  output$download_watershed_data <- downloadHandler(
    filename = function() paste0("watershed_analysis_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      req(watershed_data(), watershed_attributes())
      wb <- createWorkbook()
      addWorksheet(wb, "flow_statistics")
      writeData(wb, "flow_statistics", watershed_data())
      addWorksheet(wb, "watershed_attributes")
      writeData(wb, "watershed_attributes", watershed_attributes())
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui = ui, server = server)