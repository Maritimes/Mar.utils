#' @title simple_date
#' @description This function converts specified datetime fields into date fields.
#' @param df default is \code{NULL}. This is the dataframe with datetime fields 
#' you want to modify. 
#' @param datefields default is \code{NULL}. This is a vector of datetime fields
#' that you want to overwrite with simpler, date fields (i.e. 'YYYY-MM-DD').
#' @return returns a data.frame with the updated fields.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note The number of items in oldFields and newFields must be identical, and 
#' the order matters in that newFields[3] will replace oldFields[3].
#' @export 
simple_date <- function(df = NULL, datefields =NULL){
  #function converting 1 or more datetime fields into date fields
  for (i in 1:length(datefields)){
    df[,datefields[i]] <- as.Date(paste0(lubridate::year(df[,datefields[i]]),"-",lubridate::month(df[,datefields[i]]),"-",lubridate::day(df[,datefields[i]])))
  }
  return(df)
}