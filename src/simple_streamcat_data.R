simple_streamcat_data <- function(df){
  
  selected_vars <- c("clay", "sand", "rckdep", "wtdep", "perm")
  
  
  streamcat_vars <- StreamCatTools::sc_get_data(metric = paste(selected_vars, collapse = ","),
                                                aoi = 'watershed',
                                                showAreaSqKm = TRUE,
                                                comid = df$comid) %>%
    # remove variables we don't particularly care about that get returned:
    select(-contains("areasqkm"))
  
  streamcat_list_simple <- df %>%
    left_join(., streamcat_vars, by = "comid")
  
  return(streamcat_list_simple)
}
# colorado <- tigris::states() %>% filter(STUSPS == "CO")
# colorado_buffer <- colorado %>% st_buffer(.25)
# comids <- nhdplusTools::get_nhdplus(AOI = colorado_buffer, realization = "catchment") %>% st_drop_geometry()
# df <- comids %>% rename(comid = featureid)
# simple_streamcat_data <- function(df) {
#   streamcat_vars <- StreamCatTools::sc_get_data(metric = paste(selected_vars, collapse = ","),
#                                                 aoi = 'watershed',
#                                                 showAreaSqKm = TRUE,
#                                                 comid = df$comid) %>%
#     # remove variables we don't particularly care about that get returned:
#     select(-contains("areasqkm"))
#   
#   streamcat_list_simple <- df %>%
#     left_join(., streamcat_vars, by = "comid")
#   
#   return(streamcat_list_simple)
# }
# 
# # Function to split dataframe into chunks and process them
# process_streamcat_in_chunks <- function(df, chunk_size = 500) {
#   # Split the dataframe into chunks of 500 rows
#   total_rows <- nrow(df)
#   num_chunks <- ceiling(total_rows / chunk_size)
#   
#   # Create a list of dataframe chunks
#   df_chunks <- lapply(1:num_chunks, function(i) {
#     start_row <- (i - 1) * chunk_size + 1
#     end_row <- min(i * chunk_size, total_rows)
#     df[start_row:end_row, ]
#   })
#   
#   # Apply simple_streamcat_data to each chunk
#   processed_chunks <- lapply(df_chunks, function(chunk) {
#     result <- simple_streamcat_data(chunk)
#     return(result)
#   })
#   
#   # Combine all chunks back into a single dataframe
#   final_df <- do.call(rbind, processed_chunks)
#   
#   return(final_df)
# }
# 
# # Usage example:
# final_streamcat_data <- process_streamcat_in_chunks(df)
# write_csv(final_streamcat_data, "data/katie/COLORADO_SOIL_STREAMCAT.csv")
