#' trash_surveys Function
#'
#' Filters test surveys from Rankmi Database
#' @param df Data frame to filter
#' @param var Name of the df var to filter as string
#' @export

trash_surveys <- function(df, var){

  output <- df %>%
  filter(str_length(!!sym(var)) > 1) %>%
  filter(!str_detect(!!sym(var), regex("zergey|marian|test|piloto|prueba|archivar|copia|eliminar|demo|toma de|no func|malo|dado de baja|carolina|clima danna|clima jesus", ignore_case = T)))

  return(output)
}
