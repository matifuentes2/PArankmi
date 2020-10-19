#' trash_surveys Function
#'
#' Filters test surveys from Rankmi Database
#' @param df Data frame to filter
#' @param var Name of the df var to filter as string
#' @param query Boolean. Are you filtering inside the query?
#' @examples
#'
#' # query = F
#' trash_surveys(base, "title")
#'
#' # query = T
#' DBI::dbGetQuery(con, glue::glue("select s.title
#'from survey_dimensions sd
#'left join dimensions d on d.id = sd.dimension_id
#'left join surveys s on s.id = sd.survey_id
#'left join enterprise_processes ep on ep.id = s.enterprise_process_id
#'left join dimension_levels dl on dl.dimension_id = d.id
#'where ep.rankmi_module_id = 3
#'and d.dimension_type = 'item'
#'and dl.evaluation_type_label_id is not null
#'and s.survey_type = 0
#'{trash_surveys(var = 's.title', query = T)}
#'"))
#' @export

trash_surveys <- function(df=NULL, var, query = F){

  omisiones <-   c("zergey","marian","test","piloto","prueba","archivar","copia",
                   "eliminar","demo","toma de","no func","malo","dado de baja"
                   ,"carolina","clima danna","clima jesus")


  if(query == F){

  output <- df %>%
  filter(str_length(!!sym(var)) > 1) %>%
  filter(!str_detect(!!sym(var), regex(paste(omisiones,collapse = "|"), ignore_case = T)))
  }

  else{
    output = glue::glue("and length({var}) > 1")

    for (i in 1:length(omisiones)){
      output <- paste(output, glue::glue("and {var} not ilike '%{omisiones[[i]]}%'"))
    }

  }

  return(output)
}




