#' Backslasher Function
#'
#' This function allows you to replace the backslashes of the string in your clipboard by normal slashes
#' @keywords backslash
#' @export

backslasher <- function(){

  if(!require("clipr", character.only = TRUE)){
    install.packages("clipr")
  }

  x <- clipr::read_clip()
  y <- gsub("\\\\", "/", x)
  write_clip(y)
  return(y)
}



