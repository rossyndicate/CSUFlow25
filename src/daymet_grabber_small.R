daymet_grabber <- function(watersheds){
  
  # Function to process the data
  process_data <- function(stack, var = "swe") {
    
    stack_crs <- crs(stack[[1]])
    
    # transform AOI to match raster CRS if needed
    if(st_crs(watersheds) != stack_crs) {
      watersheds <- st_transform(watersheds, crs = stack_crs)
    } else {
      watersheds <- watersheds
    }
    
    ws_vect <- terra::vect(watersheds)  
    
    values <- terra::extract(stack, ws_vect, weights = TRUE,
                             fun = mean, na.rm = TRUE, ID = TRUE)
    
    # Extract month from column names
    col_names <- colnames(values)[-1]  # Exclude the ID column
    months <- as.numeric(str_extract(col_names, "[0-9]+$"))
    
    # Create a data frame with ID, column name, month, and value
    raw_data <- values %>%
      pivot_longer(cols = -ID, names_to = "column", values_to = "value") %>%
      mutate(month = as.numeric(str_extract(column, "[0-9]+$")),
             year = as.numeric(str_extract(column, "(?<=_)[0-9]{4}(?=_[0-9]+$)")),
             wyear = ifelse(month %in% c(10,11,12), year + 1, year)) %>%
      # do by wateryear so remove 2000 data before october and 2020 data after september
      filter(wyear >= 2001 & wyear <= 2020)
    
    
    monthly_avgs <- raw_data %>%
      # Calculate monthly averages across years for each watershed
      group_by(ID, month) %>%
      summarize(monthly_avg = mean(value, na.rm = TRUE), .groups = "drop") %>%
      arrange(ID, month) %>%
      mutate(month_name = tolower(month.abb[month]))
    
    
    # calculate annual mean for each watershed
    annual_avgs <- raw_data %>%
      group_by(ID) %>%
      summarize(avg = mean(value, na.rm = TRUE),
                .groups = "drop")
    
    # caluclate mean annual peak of monthly values 
    peak_avgs <- raw_data %>%
      group_by(ID, wyear) %>%
      filter(value == max(value, na.rm = TRUE)) %>%
      ungroup() %>%
      group_by(ID) %>%
      summarize(peak = mean(value))
    
    # avg. mean sum
    sum_avgs <- raw_data %>%
      group_by(ID, wyear) %>%
      summarize(sum = sum(value, na.rm = TRUE)) %>%
      ungroup() %>%
      group_by(ID) %>%
      summarize(avg_sum = mean(sum))
    
    # Join the results
    result <- monthly_avgs %>%
      pivot_wider(names_from = "month_name", values_from = monthly_avg, id_cols = "ID") %>%
      left_join(annual_avgs, by = "ID") %>%
      left_join(peak_avgs, by = "ID") %>%
      left_join(sum_avgs, by = "ID") %>%
      rename_with(~ paste0(var, "_", .), -ID)
    
    return(result)
  }
  
  swe_stack <- readRDS("data/test_swe_crop.RDS") %>%
    process_data(stack = ., var = "swe") %>%
    select(-c(ID, swe_jul, swe_aug, swe_sep, swe_oct, swe_nov, swe_dec))
  
  precip_stack <- readRDS("data/test_prcp_crop.RDS") %>%
    process_data(stack = ., var = "prcp") %>%
    select(-ID)
  
  watersheds <- watersheds %>% 
    cbind(swe_stack) %>%
    cbind(precip_stack)
  
  return(watersheds)
  
}

