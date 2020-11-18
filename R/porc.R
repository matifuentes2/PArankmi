#' porc Function
#'
#' Typical count + mutate pipeline to get percentage values
#' @keywords porc
#' @export

porc <- function(df, var){
  var <- enquo(var)
  df %>%
    count(!!var) %>%
    mutate(porc = round(n*100/sum(n),1)) %>%
    arrange(-porc)
}


