#' FJenks
#'
#' Calculates two number of classes of Fisher-Jenks algorithm based on HS-UCL
#'
#' @param t Siriema's data frame.
#'
#' @importFrom classInt "classIntervals"

FJenks <- function(t){

  t_hot <- t %>%
    dplyr::filter(`HS-UCL` > 0)

  Fisher5 <- classInt::classIntervals(
    var = t_hot$`HS-UCL`,
    n = 5,
    style = "fisher",
    dataPrecision = 6
  )

  Fisher3 <-  classInt::classIntervals(
    var = t_hot$`HS-UCL`,
    n = 3,
    style = "fisher",
    dataPrecision = 6
  )

  t %>%
    dplyr::mutate(
      Rank = dplyr::min_rank(dplyr::desc(`HS-UCL`)),
      km_ini = km_round - length/1000/2,
      km_end = km_round + length/1000/2,
      FisherJenks3 = dplyr::case_when(
        `HS-UCL` >= Fisher3$brks[1] & `HS-UCL` <= Fisher3$brks[2] ~ "Low",
        `HS-UCL` >= Fisher3$brks[2] & `HS-UCL` <= Fisher3$brks[3] ~ "Medium",
        `HS-UCL` >= Fisher3$brks[3] & `HS-UCL` <= Fisher3$brks[4] ~ "High",
        TRUE ~ NA_character_
      ),
      FJ3num = dplyr::case_when(
        FisherJenks3 == "Low" ~ 1L,
        FisherJenks3 == "Medium" ~ 2L,
        FisherJenks3 == "High" ~ 3L,
        TRUE ~ NA_integer_
      ),
      FisherJenks5 = dplyr::case_when(
        `HS-UCL` >= Fisher5$brks[1] & `HS-UCL` <= Fisher5$brks[2] ~ "Very Low",
        `HS-UCL` >= Fisher5$brks[2] & `HS-UCL` <= Fisher5$brks[3] ~ "Low",
        `HS-UCL` >= Fisher5$brks[3] & `HS-UCL` <= Fisher5$brks[4] ~ "Medium",
        `HS-UCL` >= Fisher5$brks[4] & `HS-UCL` <= Fisher5$brks[5] ~ "High",
        `HS-UCL` >= Fisher5$brks[5] & `HS-UCL` <= Fisher5$brks[6] ~ "Very High",
        TRUE ~ NA_character_
      ),
      FJ5num = dplyr::case_when(
        FisherJenks5 == "Very Low" ~ 1L,
        FisherJenks5 == "Low" ~ 2L,
        FisherJenks5 == "Medium" ~ 3L,
        FisherJenks5 == "High" ~ 4L,
        FisherJenks5 == "Very High" ~ 5L,
        TRUE ~ NA_integer_
      )
    ) %>%
    dplyr::mutate_if(is.double, round, digits = 3) %>%
    dplyr::select(ID:km_round, km_ini, km_end, HS:`HS-UCL`, Hotspot, NEvents, Rank, FisherJenks3, FJ3num, FisherJenks5, FJ5num, geometry) %>%
    dplyr::rename(km_center = km_round) %>%
    dplyr::select(-geometry, dplyr::everything(.)) %>%
    dplyr::arrange(Rank, NEvents, km_ini)
}
