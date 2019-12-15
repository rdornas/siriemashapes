#' FJenks
#'
#' Calculates two number of classes of Fisher-Jenks algorithm
#'
#' @param t Siriema's data frame.
#'
#' @import classInt
#' @import dplyr

FJenks <- function(t){

  Fisher5 <- classIntervals(t$`HS-UCL`,
                            n = 5,
                            style = "fisher",
                            dataPrecision = 6)

  Fisher3 <- classIntervals(t$`HS-UCL`,
                            n = 3,
                            style = "fisher",
                            dataPrecision = 6)

  t %>%
    mutate(Rank = min_rank(desc(`HS-UCL`)),
           km_ini = km_round - length/1000/2,
           km_end = km_round + length/1000/2,
           FisherJenks3 = case_when(`HS-UCL` >= Fisher3$brks[1] & `HS-UCL` <= Fisher3$brks[2] ~ "Low",
                                    `HS-UCL` >= Fisher3$brks[2] & `HS-UCL` <= Fisher3$brks[3] ~ "Medium",
                                    `HS-UCL` >= Fisher3$brks[3] & `HS-UCL` <= Fisher3$brks[4] ~ "High"),
           FJ3num = case_when(FisherJenks3 == "Low" ~ 1L,
                              FisherJenks3 == "Medium" ~ 2L,
                              FisherJenks3 == "High" ~ 3L),
           FisherJenks5 = case_when(`HS-UCL` >= Fisher5$brks[1] & `HS-UCL` <= Fisher5$brks[2] ~ "Very Low",
                                    `HS-UCL` >= Fisher5$brks[2] & `HS-UCL` <= Fisher5$brks[3] ~ "Low",
                                    `HS-UCL` >= Fisher5$brks[3] & `HS-UCL` <= Fisher5$brks[4] ~ "Medium",
                                    `HS-UCL` >= Fisher5$brks[4] & `HS-UCL` <= Fisher5$brks[5] ~ "High",
                                    `HS-UCL` >= Fisher5$brks[5] & `HS-UCL` <= Fisher5$brks[6] ~ "Very High"),
           FJ5num = case_when(FisherJenks5 == "Very Low" ~ 1L,
                              FisherJenks5 == "Low" ~ 2L,
                              FisherJenks5 == "Medium" ~ 3L,
                              FisherJenks5 == "High" ~ 4L,
                              FisherJenks5 == "Very High" ~ 5L)) %>%
    mutate_if(is.double, round, digits = 3) %>%
    select(ID:km_round, km_ini, km_end, HS:`HS-UCL`, NEvents, Rank, FisherJenks3, FJ3num, FisherJenks5, FJ5num, geometry) %>%
    rename(km_center = km_round) %>%
    select(-geometry, everything(.)) %>%
    arrange(Rank, NEvents, km_ini)
}
