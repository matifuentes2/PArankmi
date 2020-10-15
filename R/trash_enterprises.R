#' trash_enterprises Function
#'
#' Returns test enterprises from Rankmi Database (MANUELITA QUÉ ES ESA WEÁ BRO)
#' @export

trash_enterprises <- function(){

  enterprises <- c("Acme Corp", "Avengers","CompuMundo Hiper Mega Red", "Wayne Enterprises","Empresa de prueba", "EmpresaPrueba", "Empresa test", "Enaex DEMO", "Stark Industries", "Customer Success Demos", "Llevepan","BBVAdemo","BCP Demo","Clientes Rankmi","Co op","Empresas ACME", "Fortuna Silver Mines", "Fortuna Silver Mines 2","Manuelita", "Only app", "prueba", "Red", "Rankmi Test", "Sales Manager","test creacion","Test intercom 1","test org chart","tyrell","Tyrell Corp.")
  output <- sort(enterprises)
  return(output)
}
