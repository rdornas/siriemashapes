#' FJenks
#'
#' Calculates two number of classes of Fisher-Jenks algorithm
#'
#' @param t Siriema data frame.

FJenks <- function(t){
  require(classInt)

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
           FisherJenks3 = case_when(`HS-UCL` >= Fisher3$brks[1] & `HS-UCL` <= Fisher3$brks[2] ~ "Baixa",
                                    `HS-UCL` >= Fisher3$brks[2] & `HS-UCL` <= Fisher3$brks[3] ~ "Média",
                                    `HS-UCL` >= Fisher3$brks[3] & `HS-UCL` <= Fisher3$brks[4] ~ "Alta"),
           FJ3num = case_when(FisherJenks3 == "Baixa" ~ 1L,
                              FisherJenks3 == "Média" ~ 2L,
                              FisherJenks3 == "Alta" ~ 3L),
           FisherJenks5 = case_when(`HS-UCL` >= Fisher5$brks[1] & `HS-UCL` <= Fisher5$brks[2] ~ "Muito Baixa",
                                    `HS-UCL` >= Fisher5$brks[2] & `HS-UCL` <= Fisher5$brks[3] ~ "Baixa",
                                    `HS-UCL` >= Fisher5$brks[3] & `HS-UCL` <= Fisher5$brks[4] ~ "Média",
                                    `HS-UCL` >= Fisher5$brks[4] & `HS-UCL` <= Fisher5$brks[5] ~ "Alta",
                                    `HS-UCL` >= Fisher5$brks[5] & `HS-UCL` <= Fisher5$brks[6] ~ "Muito Alta"),
           FJ5num = case_when(FisherJenks5 == "Muito Baixa" ~ 1L,
                              FisherJenks5 == "Baixa" ~ 2L,
                              FisherJenks5 == "Média" ~ 3L,
                              FisherJenks5 == "Alta" ~ 4L,
                              FisherJenks5 == "Muito Alta" ~ 5L)) %>%
    mutate_if(is.double, round, digits = 3) %>%
    select(ID:km_round, km_ini, km_end, HS:`HS-UCL`, NEvents, Rank, FisherJenks3, FJ3num, FisherJenks5, FJ5num, geometry) %>%
    rename(km_center = km_round) %>%
    select(-geometry, everything(.)) %>%
    arrange(Rank, NEvents, km_ini)
}
