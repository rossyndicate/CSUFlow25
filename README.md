# CSUFlow25

CSUFlow25 is a Shiny app for predicting streamflow metrics for ungaged watersheds in Colorado. Users can either delineate a watershed from the interactive map or upload a watershed polygon, and the app extracts watershed characteristics and applies CSUFlow25 regression models to generate flow predictions.

The app estimates annual, monthly, low-flow, high-flow, timing, and bankfull metrics using watershed predictors such as topography, snow, climate, land cover, soils, geology, dams, roads, hydrologic region, and PET.

This app is hosted online by the Colorado State University Geospatial Centroid [here](https://geocentroid.shinyapps.io/CSUFlow25/).

The associated report can be accessed HERE (... eventually. The home for this report still pending).

## Run locally

This project is an RStudio/Shiny app, not an R package. From the project root, run:

``` r
install.packages(c(
  "shiny", "leaflet", "sf", "bslib", "shinyjs", "nhdplusTools",
  "purrr", "data.table", "tidyverse", "terra", "lwgeom",
  "openxlsx", "elevatr", "climateR"
))

shiny::runApp()
```

Notes:

-   The app expects the large local datasets in `data/` to remain in place.
-   Some steps rely on external services or internet access, including NHD-based delineation, elevation retrieval, and GridMET climate data.
-   Spatial packages may require system libraries such as GDAL, GEOS, and PROJ.

## Project structure

-   `app.R`: main Shiny application
-   `src/`: helper functions for watershed variable extraction
-   `data/`: preprocessed rasters, model files, road/fire/geology layers, and metadata
-   `temp_data/`: temporary files created while the app runs

## Inputs and outputs

Inputs:

-   Delineate a watershed from the map
-   Upload a watershed polygon as a zipped shapefile or GeoJSON

Outputs:

-   Predicted streamflow metrics
-   Extracted watershed attributes used by the models
-   Downloadable results from the app interface
