#' @title SQL_in
#' @description Converts a vector of characters into something that an "IN" statement can use 
#' @param x  The default value is \code{NULL}. 
#' @param apos The default value is \code{TRUE}. The default of \code{TRUE} 
#' results in a series of values where each is surrounded by apostrophes, which 
#' is appropriate for character values (e.g. ('Archer', 'Lana', 'Krieger')). 
#' Setting it to \code{FALSE} omits the apostrophes, making it appropriate for 
#' numeric values (e.g. (1,2,3,4,5)) 
#' @family util
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}l
#' @export
SQL_in <- function(x, apos=TRUE) {
  if (apos == TRUE){
    paste(unlist(gsub("(.*)","'\\1'",x)),sep="",collapse=",")
  }else{
    paste(unlist(gsub("(.*)","\\1",x)),sep="",collapse=",")
    }
}