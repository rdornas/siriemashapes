#' siriemashapes
#'
#' Creates complete dataframe/shapefile from Hotspots results obtained from Siriema software
#'
#' @param line_path Line used in Siriema - txt file.
#' @param events_path Events used in Siriema - txt file.
#' @param hotspot_path Hotspot results obtained from Siriema - txt or dat file.
#' @param crs Coordinate system used. Strongly recommended the use of EPSG.
#'
#' @export

siriemashapes <- function(line_path,
                          events_path,
                          hotspot_path,
                          crs
){
  # suppressPackageStartupMessages({
  #   require(classInt)
  #   require(sf)
  #   require(tibble)
  #   require(dplyr)
  # })

  message("Sit down comfortably. This may take a while...")

  Road <- Line(line_path = line_path, crs = crs)

  # staking the line feature ----
  Stake <- Milepost(Road, 1) %>%
    mutate(km = as.character(m/1000))

  # reading events feature (same fashion as from Siriema) ----
  Events <- Events(events_path = events_path, crs = crs)

  # establishing first data frame from files uploaded ----
  suppressWarnings({df_hotspot <- data.table::fread(hotspot_path,
                                                    encoding = "Latin-1",
                                                    check.names = T,
                                                    #fill = T,
                                                    data.table = F) %>%
    select_if(is.numeric) %>%
    `colnames<-`(c("km", "X", "Y", "HS", "UCL", "LCL")) %>%
    mutate(`HS-UCL` = HS - UCL,
           km_round = if_else(duplicated(km), round(km, 3), km)) %>%
    select(km_round, X, Y, HS, UCL, LCL, `HS-UCL`) %>%
    mutate_if(is.double, round, 3) %>%
    as_tibble(.) %>%
    mutate(km_char = as.character(km_round),
           km_med_ini = as.character(cumsum(km_round - lag(km_round, default = .$km_round[1])))) %>%
    left_join(., select(Stake, X, Y, km), by = c("km_med_ini" = "km"), suffix = c("", "_iniline")) %>%
    left_join(., select(Stake, X, Y, km), by = c("km_char" = "km"), suffix = c("", "_orig")) %>%
    rowid_to_column(., "ID") %>%
    select(ID, km_round, km_char, X, Y, X_orig, Y_orig, everything(.))
  })
  # cutting df_hotspots ----
  cut <- df_hotspot %>%
    select(ID, X_iniline, Y_iniline) %>%
    filter(!is.na(X_iniline)) %>%
    st_as_sf(., coords = c("X_iniline", "Y_iniline"), remove = F, crs = crs) %>%
    st_buffer(., dist = 0.00001)

  # creating shape from files ----
  Shape <- Road %>%
    lwgeom::st_split(., cut) %>%
    st_collection_extract(., "LINESTRING") %>%
    mutate(length = as.numeric(round(st_length(.), digits = 3))) %>%
    filter(length > .001) %>%
    rowid_to_column(., "ID") %>%
    left_join(., df_hotspot, by = "ID") %>%
    mutate(Hot = case_when(`HS-UCL` <= 0 ~ "N",
                           TRUE ~ "S"))

  Shape2 <- Shape %>%
    st_buffer(., dist = 250, endCapStyle = "FLAT") %>%
    st_join(., Events) %>%
    st_drop_geometry(.) %>%
    count(ID, name = "NEvents") %>%
    left_join(Shape, ., by = "ID")

  shapefile <- FJenks(Shape2)

  species_df <- Shape %>%
    st_buffer(., dist = 250, endCapStyle = "FLAT") %>%
    select(ID, geometry) %>%
    st_join(., Events) %>%
    st_drop_geometry(.) %>%
    count(ID, Sp, sort = T) %>%
    filter(!is.na(Sp))

  message("Done!")

  list(shapefile = shapefile, species_df = species_df)
}
