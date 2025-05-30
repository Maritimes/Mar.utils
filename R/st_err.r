#' @title st_err
#' @description Calculates the standard error 
#' @param x  The default value is \code{NULL}.  
#' @param na.rm The default value is \code{FALSE}. 
#' @family util
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
st_err <- function(x = NULL, na.rm = FALSE) {
  if (na.rm) x <- stats::na.omit(x)
  if (length(x) < 2) return(NA)
  stats::sd(x)/sqrt(length(x))
}

