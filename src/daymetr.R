# # Install and load required packages
# if (!require("daymetr")) install.packages("daymetr")
# if (!require("raster")) install.packages("raster")
# if (!require("terra")) install.packages("terra")
# if (!require("tidyverse")) install.packages("tidyverse")
# 
# library(daymetr)
# library(raster)
# library(terra)
# library(tidyverse)
# 
# watershed <- watersheds[1,] 
# bbox <- #tigris::states() %>% filter(STUSPS == "CO") %>%
#   watershed %>%
#   st_make_valid() %>%
#   st_bbox()
# bbox <- c(bbox[1], bbox[3], bbox[2], bbox[4])
# # Method 2: Alternative - Load a shapefile if you have one
# # aoi <- terra::vect("path/to/your/shapefile.shp")
# # bbox <- as.vector(terra::ext(aoi))
# 
# dir.create(paste0("data/katie/daymet/", watershed$site_no))
# 
# # Set parameters
# start_year <- 1999
# end_year <- 2024
# variables <- c("tmin", "tmax", "prcp", "swe")  # Available variables: tmin, tmax, prcp, srad, vp, swe, dayl
# 
# # Download Daymet tiles for the region
# daymet_tiles <- daymetr::download_daymet_tiles(
#   location = bbox,
#   start = start_year,
#   end = end_year,
#   param = variables,
#   path = paste0("data/katie/daymet/", watershed$site_no),  # Output directory
#   silent = FALSE
# )
# 
# # Function to process a single variable and aggregate to monthly values
# process_variable <- function(variable, years) {
#   monthly_data <- list()
#   
#   for (year in years) {
#     # Find all files for this variable and year
#     files <- list.files(
#       path = "daymet_data", 
#       pattern = paste0(variable, "_", year), 
#       full.names = TRUE
#     )
#     
#     if (length(files) == 0) next
#     
#     # Read as a raster stack
#     r_stack <- raster::stack(files)
#     
#     # Calculate monthly means
#     # Assuming 365 or 366 daily layers
#     days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
#     if (lubridate::leap_year(year)) days_in_month[2] <- 29
#     
#     # Create index for which day belongs to which month
#     month_index <- rep(1:12, days_in_month)
#     
#     # Aggregate by month
#     start_idx <- 1
#     for (month in 1:12) {
#       end_idx <- start_idx + days_in_month[month] - 1
#       
#       # For precipitation, we sum the values for each month
#       if (variable == "prcp") {
#         monthly_data[[paste0(year, "_", month)]] <- sum(r_stack[[start_idx:end_idx]])
#       } else {
#         # For other variables like temperature, we average
#         monthly_data[[paste0(year, "_", month)]] <- mean(r_stack[[start_idx:end_idx]])
#       }
#       
#       start_idx <- end_idx + 1
#     }
#   }
#   
#   return(monthly_data)
# }
# 
# # Process each variable
# years <- start_year:end_year
# monthly_results <- list()
# 
# for (var in variables) {
#   cat("Processing variable:", var, "\n")
#   monthly_results[[var]] <- process_variable(var, years)
# }
# 
# # Save the monthly data to disk
# # Convert list to raster stack and write to disk
# for (var in names(monthly_results)) {
#   var_stack <- raster::stack(monthly_results[[var]])
#   
#   # Create names for the layers in format YYYY_MM
#   layer_names <- names(monthly_results[[var]])
#   names(var_stack) <- layer_names
#   
#   # Save as GeoTIFF
#   writeRaster(var_stack, 
#               filename = paste0("monthly_", var, ".tiff"), 
#               format = "GTiff", 
#               overwrite = TRUE)
#   
#   cat("Saved monthly", var, "data to disk\n")
# }
# 
# # Optional: If you want the data as a dataframe for analysis
# # Extract values for a specific point or the mean across your AOI
# extract_monthly_data <- function(var_name) {
#   var_stack <- raster::stack(monthly_results[[var_name]])
#   
#   # If you have a polygon, you can extract mean values for your area
#   # if (exists("aoi")) {
#   #   values <- extract(var_stack, aoi, fun = mean, na.rm = TRUE)
#   # } else {
#   
#   # Or extract cell values across the extent
#   values <- cellStats(var_stack, stat = "mean", na.rm = TRUE)
#   # }
#   
#   # Create dataframe
#   df <- data.frame(
#     value = values,
#     variable = var_name
#   )
#   
#   # Parse year and month from names
#   date_parts <- stringr::str_split_fixed(names(values), "_", 2)
#   df$year <- as.numeric(date_parts[, 1])
#   df$month <- as.numeric(date_parts[, 2])
#   
#   return(df)
# }
# 
# # Combine all variables into one dataframe
# monthly_df <- bind_rows(lapply(variables, extract_monthly_data))
# 
# # Save to CSV
# write.csv(monthly_df, "monthly_daymet_summary.csv", row.names = FALSE)
# 
# print("Processing complete! Monthly Daymet data has been downloaded and aggregated.")