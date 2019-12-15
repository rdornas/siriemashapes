#' Line
#'
#' Reads and uploads txt road file.
#'
#' @param line_path Line used in Siriema - txt file.
#' @param crs Coordinate system used. Preferably EPSG.
#'
#' @import sf
#' @import dplyr

Line <- function(line_path, crs){
  read.table(line_path) %>%
    as_tibble(.) %>%
    st_as_sf(., coords = c("V1", "V2"), remove = F, crs = crs) %>%
    summarise(do_union = FALSE) %>%
    st_cast(., "LINESTRING")
}
