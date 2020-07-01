#' gen_taxonomy Function
#'
#' This function allows you quickly classify people into generations based on their birthdate
#' @keywords backslash
#' @export
#' @examples



gen_taxonomy  <- function(df, birthdate_colname = "birthdate", output_colname = "generacion"){

  if(!birthdate_colname %in% colnames(df)) stop("Your input column is not contained in the df. Please change your columns name or modify the parameter")

  if(min(df %>% select(!!birthdate_colname) %>% .[[1]], na.rm = T) <= 1920) warning('One of your participants is an alien (was born before 1920)')

  df %>%
    mutate(!!output_colname := case_when(lubridate::year(rlang::UQ(rlang::sym(birthdate_colname))) >= 1994 ~ "Generación Z",
                                         lubridate::year(rlang::UQ(rlang::sym(birthdate_colname))) >= 1981 ~ "Millennials",
                                         lubridate::year(rlang::UQ(rlang::sym(birthdate_colname))) >= 1969 ~ "Generación X",
                                         lubridate::year(rlang::UQ(rlang::sym(birthdate_colname))) >= 1949 ~ "Baby Boomer",
                                         lubridate::year(rlang::UQ(rlang::sym(birthdate_colname))) >= 1930 ~ "Silent Generation"

    ))
}

