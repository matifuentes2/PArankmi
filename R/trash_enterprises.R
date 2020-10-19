#' trash_enterprises Function
#'
#' Returns test enterprises from Rankmi Database (MANUELITA QUÉ ES ESA WEÁ BRO)
#' @param query Boolean. Are you filtering inside the query?
#' @examples
#'
#' query = F
#' base %>%
#' filter(!enterprise_name %in% PArankmi::trash_enterprises())
#'
#' #query = T
#' base <- DBI::dbGetQuery(con, glue::glue("select s.title, e.name
#'from survey_dimensions sd
#'left join dimensions d on d.id = sd.dimension_id
#'left join surveys s on s.id = sd.survey_id
#'left join enterprise_processes ep on ep.id = s.enterprise_process_id
#'left join dimension_levels dl on dl.dimension_id = d.id
#'left join enterprises e on e.id = ep.enterprise_id
#'where ep.rankmi_module_id = 3
#'and d.dimension_type = 'item'
#'and dl.evaluation_type_label_id is not null
#'and s.survey_type = 0
#'and e.name not in ({PArankmi::trash_enterprises(query = T)})
#'"))
#' @export

trash_enterprises <- function(query = F){

  enterprises <- c("Acme Corp", "Avengers","CompuMundo Hiper Mega Red", "Wayne Enterprises","Empresa de prueba", "EmpresaPrueba", "Empresa test", "Enaex DEMO", "Stark Industries", "Customer Success Demos", "Llevepan","BBVAdemo","BCP Demo","Clientes Rankmi","Co op","Empresas ACME", "Fortuna Silver Mines", "Fortuna Silver Mines 2","Manuelita", "Only app", "prueba", "Red", "Rankmi Test", "Sales Manager","test creacion","Test intercom 1","test org chart","tyrell","Tyrell Corp.")

  if(query == F){
  output <- sort(enterprises)}

  else{
  output <- str_c("'",paste(enterprises, collapse = "','"),"'")
  }

  return(output)
}
