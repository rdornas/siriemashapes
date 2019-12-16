#' Line
#'
#' Reads and uploads txt road file.
#'
#' @param line_path Line used in Siriema - txt file.
#' @param crs Coordinate system used. Preferably EPSG.
#'
#' @importFrom utils "read.table"


Line <- function(line_path, crs){
  read.table(line_path) %>%
    dplyr::as_tibble(.) %>%
    sf::st_as_sf(., coords = c("V1", "V2"), remove = F, crs = crs) %>%
    dplyr::summarise(do_union = FALSE) %>%
    sf::st_cast(., "LINESTRING")
}
