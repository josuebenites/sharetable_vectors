data_wrap <- function(list_data, cols) {
  data_processed <- lapply(list_data, function(item) {
    # Process if the item is a list, using map and then reduce
    if(is.list(item)) {
      item_processed <- map(item, ~ .x %>%
                              bind_rows() %>%
                              as_tibble() %>%
                              select(all_of(cols)))
      # Merge the processed items
      return(reduce(item_processed, left_join, by = 'var'))
    } else {
      # Directly process if the item is not a list
      return(item %>%
               bind_rows() %>%
               as_tibble() %>%
               select(all_of(cols)))
      # The right_join part is omitted due to unclear definition and intent
    }
  })
  
  # Combine all processed data frames into one
  return(reduce(data_processed, left_join, by = 'var'))
}