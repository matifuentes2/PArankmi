#' Backslasher Function
#'
#' This function allows you to replace the backslashes of the string in your clipboard by normal slashes
#' @keywords backslash
#' @export
#' @examples


backslasher <- function(){

  x <- readClipboard()
  y <- gsub("\\\\", "/", x)
  return(y)
}

