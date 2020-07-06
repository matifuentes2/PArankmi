#' Limpieza base
#'
#' This function cleans and filter base results
#' @keywords clean_results
#' @export
#' @examples clean_results(base_resultado)



clean_results <- function(x){

  janitor::clean_names(x) %>%
    filter(evaluador != is.na(evaluador))

}


