#' siriemashapes
#'
#' Creates complete dataframe/shapefile from Hotspots results obtained from \href{http://www.ufrgs.br/siriema/}{Siriema} software.
#'
#' @param line_path Line used in Siriema - txt file path.
#' @param events_path Events used in Siriema - txt file path.
#' @param hotspot_path Hotspot results obtained from Siriema - txt or dat file path.
#' @param crs Coordinate reference system. Strongly recommended the use of EPSG number.
#'
#' @author Rubem Dornas \email{(rapdornas@@gmail.com)}
#'
#' @details
#' **WARNING**: For best (and accurate) results, it is strongly recommended that the Hotspot analysis is performed taking into account the number of divisions set to double of the radius.
#'
#' The function offers a large list object containing two elements: a spatial data frame with improvements of Siriema's Hotspot Analysis results and a summary of the species found in each sample unit.
#'
#' The spatial data frame is called `shapefile` and it is composed by the following attributes:
#'
#' * **ID**: ID of the sample unit.
#' * **length**: Length of the sample unit. It is supposed to be very similar to the diameter (double of the radius) of the original Hotspot Analysis.
#' * **km_center**: Central km mark of the sample unit.
#' * **km_ini**: Initial km mark of the sample unit.
#' * **km_end**: Final km mark of the sample unit.
#' * **HS**: Aggregation intensity.
#' * **UCL**: Upper confidence limit.
#' * **LCL**: Lower confidence limit.
#' * **HS-UCL**: Subtraction of aggregation intensity by the upper limit of confidence interval. This is the "real" value of aggregation intensity as it already takes into account what is above the confidence interval.
#' * **Hotspot**: Information if the sample unit is a hotspot. Y means that \eqn{HS-UCL > 0} and therefore it is already a hotspot.
#' * **NEvents**: Number of events in the sample unit.
#' * **Rank**: Rank of aggregation intensity based on _HS-UCL_.
#' * **FisherJenks3**: Fisher-Jenks classification in three classes (text) based on _HS-UCL_.
#' * **FJ3num**: Fisher-Jenks classification in three classes (numeric) based on _HS-UCL_.
#' * **FisherJenks5**: Fisher-Jenks classification in five classes (text) based on _HS-UCL_.
#' * **FJ5num**: Fisher-Jenks classification in five classes (numeric) based on _HS-UCL_.
#'
#' The summary of the species found in each sample unit is called `species_df` and it is composed by the following attributes:
#'
#' * **ID**: ID of the sample unit.
#' * **Sp**: Species name (as from the Events file)
#' * **NEvents**: Number of events in the sample unit.
#'
#' @importFrom magrittr "%>%"
#'
#' @export

siriemashapes <- function(line_path,
                          events_path,
                          hotspot_path,
                          crs
){
  message("Sit down comfortably. This may take a while...")

  suppressMessages({Road <- Line(line_path = line_path, crs = crs)

  # staking the line feature ----
  Stake <- Milepost(Road, 1) %>%
    dplyr::mutate(km = as.character(m/1000))

  # reading events feature (same fashion as from Siriema) ----
  Events <- Events(events_path = events_path, crs = crs)

  # establishing first data frame from files uploaded ----
  suppressWarnings({df_hotspot <- data.table::fread(hotspot_path,
                                                    encoding = "Latin-1",
                                                    check.names = T,
                                                    #fill = T,
                                                    data.table = F) %>%
    dplyr::select_if(is.numeric) %>%
    `colnames<-`(c("km", "X", "Y", "HS", "UCL", "LCL")) %>%
    dplyr::mutate(`HS-UCL` = HS - UCL,
                  km_round = dplyr::if_else(duplicated(km), round(km, 3), km)) %>%
    dplyr::select(km_round, X, Y, HS, UCL, LCL, `HS-UCL`) %>%
    dplyr::mutate_if(is.double, round, 3) %>%
    dplyr::as_tibble(.) %>%
    dplyr::mutate(km_char = as.character(km_round),
                  km_med_ini = as.character(cumsum(km_round - dplyr::lag(km_round, default = .$km_round[1])))) %>%
    dplyr::left_join(., dplyr::select(Stake, X, Y, km),
                     by = c("km_med_ini" = "km"),
                     suffix = c("", "_iniline")) %>%
    dplyr::left_join(., dplyr::select(Stake, X, Y, km),
                     by = c("km_char" = "km"),
                     suffix = c("", "_orig")) %>%
    tibble::rowid_to_column(., "ID") %>%
    dplyr::select(ID, km_round, km_char, X, Y, X_orig, Y_orig, dplyr::everything(.))
  })

  # cutting df_hotspots ----
  cut <- df_hotspot %>%
    dplyr::select(ID, X_iniline, Y_iniline) %>%
    dplyr::filter(!is.na(X_iniline)) %>%
    sf::st_as_sf(., coords = c("X_iniline", "Y_iniline"), remove = F, crs = crs) %>%
    sf::st_buffer(., dist = 0.00001)

  # creating shape from files ----
  Shape <- Road %>%
    lwgeom::st_split(., cut) %>%
    sf::st_collection_extract(., "LINESTRING") %>%
    dplyr::mutate(length = as.numeric(round(sf::st_length(.), digits = 3))) %>%
    dplyr::filter(length > .001) %>%
    tibble::rowid_to_column(., "ID") %>%
    dplyr::left_join(., df_hotspot, by = "ID") %>%
    dplyr::mutate(Hotspot = dplyr::case_when(`HS-UCL` <= 0 ~ "N",
                                             TRUE ~ "Y"))

  Shape2 <- Shape %>%
    sf::st_buffer(., dist = 250, endCapStyle = "FLAT") %>%
    sf::st_join(., dplyr::select(Events, ID_event)) %>%
    sf::st_drop_geometry(.) %>%
    dplyr::filter(!is.na(ID_event)) %>%
    dplyr::count(ID, name = "NEvents") %>%
    dplyr::left_join(Shape, ., by = "ID") %>%
    dplyr::mutate(NEvents = tidyr::replace_na(NEvents, 0))

  shapefile <- FJenks(Shape2)

  species_df <- Shape %>%
    sf::st_buffer(., dist = 250, endCapStyle = "FLAT") %>%
    dplyr::select(ID, geometry) %>%
    sf::st_join(., Events) %>%
    sf::st_drop_geometry(.) %>%
    dplyr::filter(!is.na(Sp)) %>%
    dplyr::count(ID, Sp, name = "NEvents")
})

  message("Done!")

  list(shapefile = shapefile, species_df = species_df)
}
