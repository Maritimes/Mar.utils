#' @title st_err
#' @description Calculates the standard error 
#' @param x  The default value is \code{NULL}.  
#' @family util
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
st_err <- function(x = NULL) {
  stats::sd(x)/sqrt(length(x))
}

