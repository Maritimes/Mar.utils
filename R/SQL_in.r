#' @title SQL_in
#' @description Converts a vector of characters into something that an "IN" statement can use 
#' @param x  The default value is \code{NULL}.  
#' @family util
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}l
#' @export
SQL_in <- function(x) {
  paste(unlist(gsub("(.*)","'\\1'",x)),sep="",collapse=",")
}