#' Events
#'
#' Reads and uploads txt events file.
#'
#' @param events_path Select path of events txt file.
#' @param crs Coordinate system used. Strongly recommended the use of EPSG.
#'
#' @importFrom utils "read.delim"


Events <- function(events_path, crs) {
  read.delim(
    events_path,
    stringsAsFactors = F,
    header = F
  ) %>%
    dplyr::as_tibble(.) %>%
    tidyr::unite(., col = "Sp", sep = " ", 4:ncol(.)) %>%
    dplyr::mutate_if(is.character, stringr::str_squish) %>%
    dplyr::rename(X = V1,
                  Y = V2) %>%
    dplyr::select(-V3) %>%
    sf::st_as_sf(
      .,
      coords = c("X", "Y"),
      crs = crs,
      remove = F
    ) %>%
    tibble::rowid_to_column(., "ID_event")
}
