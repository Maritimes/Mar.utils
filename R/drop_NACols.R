#' @title drop_NACols
#' @description This function takes a dataframe and removes any columns that are
#' entirely populated with NAs
#' @param df default is \code{NULL}. This is the dataframe you want to check
#' @return dataframe
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
drop_NACols <- function(df){
  df[sapply(df, function(x) all(is.na(x)))] <- NULL
  return(invisible(df))
}