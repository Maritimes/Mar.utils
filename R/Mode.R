#' @title Mode
#' @description This function returns the mode of a vector, preserving data type.
#' If several values share the mode, all will be returned. 
#' @param x Vector
#' @param na.rm The default value is \code{FALSE}. 
#' @family util
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note stolen from https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
#' @export
Mode <- function(x, na.rm = FALSE) {
  if (na.rm) x <- stats::na.omit(x)
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  res <- ux[tab == max(tab)]
  return(res)
}