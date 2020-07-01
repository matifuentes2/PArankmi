#' gen_taxonomy Function
#'
#' This function allows you quickly classify people into generations based on their birthdate
#'
#' @param df a data frame.
#' @param birthdate string with the name of the df column that contains the birthdates.
#' @param output_colname string with the name which will contain the classification.
#' @examples
#' gen_taxonomy(base_personas)
#' gen_taxonomy(base_personas, fecha_nacimiento,
#' generation)
#' @export

gen_taxonomy  <- function(df, birthdate_colname = "birthdate", output_colname = "generacion"){

  if(!birthdate_colname %in% colnames(df)) stop("Your input column is not contained in the df. Please change your column's name or modify the parameter birthdate_colname")

  if(min(df %>% select(!!birthdate_colname) %>%
         mutate(!!birthdate_colname := lubridate::year(rlang::UQ(rlang::sym(birthdate_colname)))) %>%
         .[[1]], na.rm = T) <= 1920) warning('One of your participants is an alien (was born before 1920)')

  df %>%
    mutate(!!output_colname := case_when(lubridate::year(rlang::UQ(rlang::sym(birthdate_colname))) >= 1994 ~ "Generación Z",
                                         lubridate::year(rlang::UQ(rlang::sym(birthdate_colname))) >= 1981 ~ "Millennials",
                                         lubridate::year(rlang::UQ(rlang::sym(birthdate_colname))) >= 1969 ~ "Generación X",
                                         lubridate::year(rlang::UQ(rlang::sym(birthdate_colname))) >= 1949 ~ "Baby Boomer",
                                         lubridate::year(rlang::UQ(rlang::sym(birthdate_colname))) >= 1930 ~ "Silent Generation"

    ))
}

