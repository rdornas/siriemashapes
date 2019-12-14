#' Milepost
#'
#' Calculates two number of classes of Fisher-Jenks algorithm
#'
#' @param road Road shapefile.
#' @param distance Distance of stakes.
#' @param dist_or_n Selects distance of stakes or number of stakes
#'
#' @export


Milepost <- function(road, distance, dist_or_n = "dist") {
  # Calculations on multiplying factors for generating mileposts ----
  dist_or_n_opt <- c("n", "dist") #options available for dist_or_n

  if(!dist_or_n %in% dist_or_n_opt){
    stop("Option not valid for dist_or_n argument.") #check if dist_or_n options are valid.
  }
  else if(dist_or_n == "n"){
    len <- distance #if "n" set in dist_or_n, len is equal to distance
  }
  else{len <- round(as.double(st_length(road)/distance)) #if "dist" set in dist_or_n, calculates o road length and divides it by the distance
  }

  post <- seq(from = 0, to = 1, by = 1/len) #calculates multiplying factor based on distance or n

  # Generating mileposts ----
  if(dist_or_n == "dist"){
    st_line_sample(x = road, sample = post) %>% #cria estacas de acordo com as porcentagens calculadas
      st_sf(.) %>%
      st_cast(., to = "POINT") %>%
      st_coordinates(.) %>% #extract posts coordinates
      as_tibble(.) %>%
      mutate(V1 = as.integer(rownames(.)), #cria uma coluna com o ID da linha
             m = round(post*distance*len, 3), #cria uma coluna com a metragem da estaca
             L1 = 0L) %>%  #cria uma coluna com a metragem da estaca
      arrange(m) #organiza os dados pela coluna km_calc
  }
  else{
    st_line_sample(x = road, sample = post) %>% #cria estacas de acordo com as porcentagens calculadas
      st_sf(.) %>%
      st_cast(., to = "POINT") %>%
      st_coordinates(.) %>% #extract posts coordinates
      as_tibble(.) %>%
      mutate(V1 = as.integer(rownames(.)), #cria uma coluna com o ID da linha
             m = round(post*as.double(st_length(road)), 3), #cria uma coluna com a metragem da estaca
             L1 = 0L) %>%
      arrange(m) #organiza os dados pela coluna km_calc
  }
}
