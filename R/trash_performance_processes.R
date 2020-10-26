#' trash_performance_processes Function
#'
#' Filters test performance enterprise_process names from Rankmi Database
#' @param df Data frame to filter
#' @param var Name of the df var to filter as string
#' @param query Boolean. Are you filtering inside the query?
#' @examples
#'
#' # query = F
#' trash_performance_processes(df, "process_name")
#'
#' # query = T
#' DBI::dbGetQuery(con, glue::glue("select ep.name_translate ->> 'es' as name_translate,
#' ep.name_old
#' from enterprise_processes ep
#' left join enterprises e on e.id = ep.enterprise_id
#' where rankmi_module_id = 1
#' {trash_performance_processes(var = 'ep.name_translate::text', query = T)}"))
#' @export

trash_performance_processes <- function(df = NULL, var, query = F) {
  omisiones <- c(
    "test", "demo", "prueba", "ficticio", "tutorial", "piloto", "capacitac", "antiguo", "eliminar",
    "(old)", "copia", "carga", "klsak", "no ingresar", "fijac", "metas", "inactivo", "no valido",
    "dado de baja", "para obtener cadena", "curriculum", "csi", "3","4","asd","archivar","borrar",
    "clima","dar de baja","contraseña","clone","no ocupar","clonado","toma datos","encuesta",
    "(no)","malo","no aplica","no funciona","desactivado","descartado","oculto","falso",
    "fallido","fotos","gptw","holi","jh,fmh","Mapeo Ade","Mi perfil","Mentorías que debo",
    "new","no usar","placeholder","pilot","cualitati","subir cargos","aaa",
    "Acá podrás revisar las definiciones y el detalle","constraseñas")

  if (query == F) {
    output <- df %>%
      filter(str_length(!!sym(var)) > 1) %>%
      filter(!str_detect(!!sym(var), regex(paste(omisiones, collapse = "|"), ignore_case = T)))
  }

  else {
    output <- glue::glue("and length({var}) > 1")

    for (i in 1:length(omisiones)) {
      output <- paste(output, glue::glue("and {var} not ilike '%{omisiones[[i]]}%'"))
    }
  }

  return(output)
}





