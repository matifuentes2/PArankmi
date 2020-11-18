#' ssh Function
#'
#' Enable ssh
#' @keywords ssh
#' @export

ssh <- function(location = "/home/matias/Documents/Credentials/rankmi_office.pem",
                port = "5556"){

  if ((system("ps -fC ssh", intern = T) %>% length()) == 1){
    system(glue::glue("ssh -f -i {location} -L {port}:rankmi-prod-cluster.cluster-ro-crtmr3w7usfm.us-west-2.rds.amazonaws.com:5432 -p 2233 ubuntu@54.148.6.196 -N "))
  }

}



