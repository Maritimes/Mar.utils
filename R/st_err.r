#' @title st_err
#' @description Calculates the standard error 
#' @param x  The default value is \code{NULL}.  
#' @family util
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom stats sd
#' @export
st_err <- function(x = NULL) {
  sd(x)/sqrt(length(x))
}

