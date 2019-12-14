#' Line
#'
#' Reads line from txt used in Siriema
#'
#' @param line_path Line used in Siriema - txt file.
#' @param crs Coordinate system used. Preferably EPSG.

Line <- function(line_path, crs){
  read.table(line_path) %>%
    as_tibble(.) %>%
    st_as_sf(., coords = c("V1", "V2"), remove = F, crs = crs) %>%
    summarise(do_union = FALSE) %>%
    st_cast(., "LINESTRING")
}
