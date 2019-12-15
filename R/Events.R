#' Events
#'
#' Reads and uploads txt events file.
#'
#' @param events_path Select path of events txt file.

Events <- function(events_path){
  read.table(events_path) %>%
    as_tibble(.) %>%
    rename(X = V1,
           Y = V2) %>%
    select(-V3) %>%
    st_as_sf(., coords = c("X", "Y"), crs = crs, remove = F)
}
