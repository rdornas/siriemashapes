#' Events
#'
#' Reads and uploads txt events file.
#'
#' @param events_path Select path of events txt file.
#' @param crs Coordinate system used. Strongly recommended the use of EPSG.

Events <- function(events_path, crs){
  read.table(Events_path, stringsAsFactors = F) %>%
    as_tibble(.) %>%
    tidyr::unite(., col = "Sp", sep = " ", 4:ncol(.)) %>%
    mutate_if(is.character, stringr::str_squish) %>%
    rename(X = V1,
           Y = V2) %>%
    select(-V3) %>%
    st_as_sf(., coords = c("X", "Y"), crs = crs, remove = F) %>%
    rowid_to_column(., "ID_event")
}
