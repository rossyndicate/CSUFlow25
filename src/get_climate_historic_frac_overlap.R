#' @param sf A shapefile of your area(s) of interest (AOI) for pulling in GridMET data. Can be either point of polygon AOIs.
#' @param col_name The column identifying the name of each area of interest.
#' @param start Start date for pulling in data. Default is "1979-01-01".
#' @param end End date for pulling in data. Default is "2023-12-31".
#' @param vars Vector of variables you would like to pull data for. Options include: "tmmx", "tmmn", "pr", "rmax", "rmin", "pet", "etr", "vpd", "vs". Default is "tmmx", "tmmn", and "pr".
#' @return a data frame of requested data for each area of interest, with the fraction of each grid cell within the AOI included. 
#' The output includes:
#' - Spatial coordinates (`x`, `y`) of each GridMET grid cell' centroid used
#' - Date of observation (`date`)
#' - Climate variables in standard units, e.g.:
#'     - `ppt_mm`: precipitation (mm)
#'     - `tmax_C`: daily maximum temperature (°C)
#'     - `tmin_C`: daily minimum temperature (°C)
#'     - `tmean_C`: daily mean temperature (°C)
#' - A user-specified AOI identifier column (`col_name`)
#' - `area_weight`: the approximate fraction of each cell that is covered by the AOI (can be used to compute a weighted mean).
#' For polygon inputs, values are derived from masked raster cells and represent fraction of each grid cell covered by the AOI.
#' For point inputs, values represent the single grid cell containing the point (percent overlap = NA).

get_climate_historic_frac_overlap <- function(sf,
                                              col_name = "index",
                                              start = "1999-10-01",
                                              end = "2024-09-30"#,
                                              #vars = c("tmmx", "tmmn", "pr")
                                              ) {
  
  sf_use_s2(TRUE)
  
  sf <- sf %>%
    dplyr::rename("site_no" = {{col_name}})
  
  all_climate_data <- vector("list", length = nrow(sf))
  
  if(any(unique(sf::st_geometry_type(sf)) %in% c("POLYGON", "MULTIPOLYGON"))){
    
    for (i in 1:nrow(sf)) {
      
      aoi <- sf[i,]
      
      print(paste0('Downloading GridMET for ', aoi$site_no, "."))
      
      # download climate data using a 4km buffer to ensure full cell capture at aoi edges
      # getGridMET does NOT always grab all the grid cells that touch an AOI
      clim <- climateR::getGridMET(AOI = st_buffer(aoi, 4000),
                                   varname = "pet",
                                   startDate = start,
                                   endDate = end)
      
      if(!is.data.frame(clim) | inherits(clim[[1]], "SpatRaster")){
        
        clim_crs <- crs(clim[[1]])
        
        # transform AOI to match raster CRS if needed
        if(st_crs(aoi) != clim_crs) {
          aoi_trans <- st_transform(aoi, crs = clim_crs)
        } else {
          aoi_trans <- aoi
        }
        
        all_climate_data[[i]] <- clim %>%
          map(~terra::extract(., vect(aoi_trans), xy = TRUE, weights = TRUE)) %>%
          map_dfr(~ as.data.frame(., xy = TRUE)) %>%
          data.table() %>%
          pivot_longer(-c(x, y, ID, weight),
                       names_to = "var_temp",
                       values_to = "val") %>%
          separate_wider_delim(var_temp, "_", names = c("var", "date")) %>%
          drop_na(val) %>%
          group_by(x, y, date) %>%
          pivot_wider(names_from = "var", values_from = "val") %>%
          dplyr::mutate(date = as.Date(date),
                        #ppt_mm = pr,
                        #tmax_C = tmmx - 273.15,
                        #tmin_C = tmmn - 273.15,
                        #tmean_C = (tmax_C + tmin_C)/2,
                        site_no = aoi$site_no) %>%
          rename(area_weight = weight) #%>%
          #dplyr::select(-c(ID, tmmx, tmmn, pr))
        
      } else {
        
        # when the returned object is already a clean data.frame (e.g., small aoi)
        
        all_climate_data[[i]] <- clim %>%
          data.table() %>%
          # since polygon grabbed a single grid, gridMET does not provide the coordinates
          # of the gridMET cell, so we fill in x and y with the coordinates
          # of the sf object:
          dplyr::mutate(x = sf::st_coordinates(st_centroid(aoi))[[1]],
                        y =  sf::st_coordinates(st_centroid(aoi))[[2]]) %>%
          # Then do all other cleaning steps done for polygon sf objects:
          dplyr::mutate(date = as.Date(date),
                        #ppt_mm = pr,
                        #tmax_C = tmmx - 273.15,
                        #tmin_C = tmmn - 273.15,
                        #tmean_C = (tmax_C + tmin_C)/2,
                        site_no = aoi$site_no,
                        # for a single grid cell fract of total aoi area divided 
                        # by 4km grid cell. Not really important tho since no 
                        # weighting really needed for these.
                        area_weight = as.numeric(st_area(aoi_trans)/4000000)) #%>%
          #dplyr::select(-c(tmmx, tmmn, pr))
        
      }
    }
    
    # combine all aoi outputs into one dataframe
    all_climate_data <- all_climate_data %>%
      bind_rows() %>%
      as.data.table() %>%
      mutate(wyear = ifelse(month(date) >= 10, year(date) + 1, year(date))) %>%
      group_by(site_no, x, y, area_weight, wyear) %>%
      summarize(total_pet = sum(pet, na.rm = TRUE)) %>%
      ungroup() %>%
      group_by(site_no, x, y, area_weight) %>%
      summarize(mean_tot_pet = mean(total_pet)) %>%
      ungroup() %>%
      group_by(site_no) %>%
      summarize(
        avg_tot_pet = sum(mean_tot_pet * area_weight) / sum(area_weight),
        .groups = "drop"
      ) %>%
      rename(index = site_no)
    
    return(all_climate_data)
    
  } else if(unique(sf::st_geometry_type(sf)) == "POINT"){
    
    for (i in 1:nrow(sf)) {
      
      aoi <- sf[i,]
      
      print(paste0('Downloading GridMET for ', aoi$site_no, "."))
      
      clim <- climateR::getGridMET(AOI = aoi,
                                   varname = "pet",
                                   startDate = start,
                                   endDate = end)
      
      all_climate_data[[i]] <- clim %>%
        data.table() %>%
        # since point pulls from gridMET do not provide the coordinates
        # of the gridMET cell, we fill in x and y with the coordinates
        # of the sf object:
        dplyr::mutate(x = sf::st_coordinates(st_centroid(aoi))[[1]],
                      y =  sf::st_coordinates(st_centroid(aoi))[[2]]) %>%
        # Then do all other cleaning steps done for polygon sf objects:
        dplyr::mutate(date = as.Date(date),
                      #ppt_mm = pr,
                      #tmax_C = tmmx - 273.15,
                      #tmin_C = tmmn - 273.15,
                      #tmean_C = (tmax_C + tmin_C)/2,
                      site_no = aoi$site_no,
                      # For a point, this concept doesn't really apply
                      area_weight = NA) #%>%
        #dplyr::select(-c(tmmx, tmmn, pr))
      
    }
    
    all_climate_data <- all_climate_data %>%
      bind_rows() %>%
      rename(index = site_no) %>%
      as.data.table() %>%
      mutate(wyear = ifelse(month(date) >= 10, year(date) + 1, year(date))) %>%
      group_by(site_no, x, y, area_weight, wyear) %>%
      summarize(total_pet = sum(pet, na.rm = TRUE)) %>%
      ungroup() %>%
      group_by(site_no, x, y, area_weight) %>%
      summarize(mean_tot_pet = mean(total_pet)) %>%
      ungroup() %>%
      group_by(site_no) %>%
      summarize(
        avg_tot_pet = sum(mean_tot_pet * area_weight) / sum(area_weight),
        .groups = "drop"
      ) %>%
      rename(index = site_no)
    
    return(all_climate_data)
    
  } else {stop("Your sf feature is neither a polygon nor point feature, or it needs to be made valid.")}
  
}

# test1 <- get_climate_historic_frac_overlap(sf = getParkBoundary(c("BLCA", "SAND", "ROMO")), col_name = "UNIT_CODE")
# test2 <- get_climate_historic_frac_overlap(sf = getParkBoundary(c("BLCA", "SAND", "ROMO")) %>% st_centroid(), col_name = "UNIT_CODE")
# test3 <- get_climate_historic_frac_overlap(sf = getParkBoundary(c("BLCA", "SAND", "ROMO")) %>% st_centroid() %>% st_buffer(30), col_name = "UNIT_CODE")
# mapview(getParkBoundary(c("BLCA", "SAND", "ROMO")), col.region = "lightgrey") +
# mapview(test1 %>% dplyr::group_by(x, y, area_weight) %>% summarize() %>% st_as_sf(coords = c("x", "y"), crs = 4269), zcol = "area_weight")
# mapview(getParkBoundary(c("BLCA", "SAND", "ROMO")), col.region = "lightgrey") +
# mapview(test2 %>% dplyr::group_by(x, y, area_weight) %>% summarize() %>% st_as_sf(coords = c("x", "y"), crs = 4269), zcol = "area_weight")
# mapview(getParkBoundary(c("BLCA", "SAND", "ROMO")), col.region = "lightgrey") +
# mapview(test3 %>% dplyr::group_by(x, y, area_weight) %>% summarize() %>% st_as_sf(coords = c("x", "y"), crs = 4269), zcol = "area_weight")