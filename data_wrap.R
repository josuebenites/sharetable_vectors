

data_wrap <- function(list_data, cols) {
  library(dplyr)
  library(purrr)
  data_processed <- lapply(list_data, function(item) {
    # Si el item es una lista, lo procesamos con map y luego usamos reduce
    if(is.list(item)) {
      item_processed <- map(item, ~ .x %>%
                              bind_rows() %>%
                              as_tibble() %>%
                              select(all_of(cols)))
      return(reduce(item_processed, left_join, by = 'var'))
    } else {
      # Si el item no es una lista, lo procesamos directamente
      return(item %>%
               bind_rows() %>%
               as_tibble() %>%
               select(all_of(cols)))
    }
  })
  
  # Finalmente, combinamos todos los data frames procesados en uno solo
  return(reduce(data_processed, left_join, by = 'var'))
}
