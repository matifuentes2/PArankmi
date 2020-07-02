#' df_ninebox Function
#'
#' Downloads and prepares the data required to run the ninebox function
#'
#' @param process_id The id of a process that features desempeño y potencial xd
#' @param connection Name of the connection to the database
#' @examples
#' test <- df_ninebox(3659)
#' test2 <- df_ninebox(1129, base_rkmi)
#' test3 <- df_ninebox(2581)
#'
#' @export


df_ninebox <- function(process_id, connection = con){

  xd <- DBI::dbGetQuery(connection, glue::glue("SELECT us.identifier, upr.dimension_type, dom.name, upr.average
FROM user_process_results upr
JOIN users us on(us.id=upr.user_id)
JOIN dimensions dom on(dom.id=upr.domain_id)
WHERE upr.enterprise_process_id= {process_id}
AND upr.dimension_type IN ('evaluation', 'evaluation_domain')"))

  if(nrow(xd) == 0) stop("Process_id was not found. Check it and try again lol")

  xd2 <- xd %>%
    pivot_wider(id_cols = identifier, names_from = name, values_from = average) %>% select(-identifier)

  colnames(xd2) <- str_to_lower(colnames(xd2))

  min_max <- DBI::dbGetQuery(connection, glue::glue("select dimensions.name, percentage_value from convertion_enterprises ce
left join dimensions on ce.dimension_id = dimensions.id
where enterprise_process_id = {process_id} and dimension_id in (SELECT dimension_id
FROM user_process_results upr
WHERE upr.enterprise_process_id in ({process_id}) --ID PROCESO
AND upr.dimension_type='evaluation')")) %>%
    group_by(name) %>%
    summarise(min = min(percentage_value, na.rm = T), max = max(percentage_value, na.rm = T)) %>% mutate(name = str_to_lower(name))

  consolideichon = list(xd2, min_max)

  return(consolideichon)
}
