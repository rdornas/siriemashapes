#' siriemashapes
#'
#' Creates complete dataframe/shapefile from Hotspots results obtained from Siriema software
#'
#' @param line_path Line used in Siriema - txt file.
#' @param crs Coordinate system used. Preferably EPSG.
#' @param events_path Events used in Siriema - txt file.
#' @param hotspots_path Hotspot results obtained from Siriema - txt or dat file.
#'
#' @export

siriemashapes <- function(line_path,
                          crs,
                          events_path,
                          hotspots_path
)
{
  require(data.table, quietly = T, warn.conflicts = F)
  require(sf, quietly = T, warn.conflicts = F)
  require(lwgeom, quietly = T, warn.conflicts = F)
  require(magrittr, quietly = T, warn.conflicts = F)
  require(dplyr, quietly = T, warn.conflicts = F)
  require(tibble, quietly = T, warn.conflicts = F)


  # reading line feature (same fashion as from Siriema) ----
  # Line <- function(line_path, crs){
  #   read.table(line_path) %>%
  #     as_tibble(.) %>%
  #     st_as_sf(., coords = c("V1", "V2"), remove = F, crs = crs) %>%
  #     summarise(do_union = FALSE) %>%
  #     st_cast(., "LINESTRING")
  # }

  Road <- siriemashapes:::Line(line_path = line_path, crs = crs)

  # staking the line feature ----
    Stake <- siriemashapes::Milepost(Road, 1) %>%
    mutate(km = as.character(m/1000))

  # reading events feature (same fashion as from Siriema) ----
    Events <- siriemashapes:::Events(events_path)

  # establishing first data frame from files uploaded ----
  df_hotspot <- fread(hotspot_path,
                      encoding = "Latin-1",
                      check.names = T,
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

  # cutting df_hotspots ----

  cut <- df_hotspot %>%
    select(ID, X_iniline, Y_iniline) %>%
    filter(!is.na(X_iniline)) %>%
    st_as_sf(., coords = c("X_iniline", "Y_iniline"), remove = F, crs = 31982) %>%
    st_buffer(., dist = 0.001)

  # creating shape from files ----

  Shape <- Road %>%
    st_split(., cut) %>%
    st_collection_extract(., "LINESTRING") %>%
    mutate(length = as.numeric(round(st_length(.), digits = 3))) %>%
    filter(length > .001) %>%
    rowid_to_column(., "ID") %>%
    left_join(., df_hotspot, by = "ID") %>%
    mutate(Hot = case_when(`HS-UCL` <= 0 ~ "N",
                           TRUE ~ "S"))

  Shape %<>%
    st_buffer(., dist = 1, endCapStyle = "FLAT") %>%
    st_join(., Events) %>%
    st_drop_geometry(.) %>%
    count(ID, name = "NEvents") %>%
    left_join(Shape, ., by = "ID")

  FJenks(Shape)
}
