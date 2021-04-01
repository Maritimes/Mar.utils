#' @title big_in
#' @description In Oracle, IN statements are limited to 1000 elements.  When dynamically extracting data based on values a dataframe,
#' it's pretty easy to exceed this limit.  This function converts huge vectors into a format that Oracle can handle, returning many more than 1000 
#' results. 
#' @param vec  The default value is \code{NULL}.  
#' @param vec.field  The default value is \code{NULL}.  This is the name of the field that the resultant IN statement will search for the values 
#' of \code{vec} 
#' @param isStrings The default value is \code{FALSE}. This determines whether the values in the resultant IN statement are surrounded
#' by apostrophes.  For numeric vectors, it should be \code{FALSE}, which will generate a statement like \code{field IN (1,2,3)}.  For 
#' characters/strings, it should be set to \code{TRUE}, so it will surround the values with apostrophes and generate a statement like 
#' \code{field IN ('Archer', 'Lana', 'Krieger')}. 
#' @return This returns a (potentially really long) valid IN string that can be included in a SQL query as a WHERE condition.  It must be 
#' prefaced by "WHERE", or "AND" depending on whether other conditions are present.  If \code{vec} has no valid values, the function will return 
#' \code{"1=1"} which will allow SQL statements to run.
#' @examples \dontrun{
#' bigVector <- c(1:2000)
#' raw <-  big_in(vec=c(1:2000), vec.field = "field2", isStrings = F)
#' raw
#' [1]  "('_dOh_', field2) IN (('_dOh_',1),('_dOh_',2),...('_dOh_',2000))
#'
#' qry <- paste0("SELECT field1, field2 from TABLE where field1 = 'value' AND ",big_in(vec=c(1:2000), vec.field = "field2", isStrings = F))
#' qry
#' [1] "SELECT field1, field2 from TABLE where field1 = 'value' AND ('_dOh_', field2) IN (('_dOh_',1),('_dOh_',2),<...>('_dOh_',2000))
#' }
#' @family util
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note The logic for this was stolen from our friends at StackOverflow.com.  Specifically, Sergey11g's response to 
#' https://stackoverflow.com/questions/400255/how-to-put-more-than-1000-values-into-an-oracle-in-clause
#' @export
big_in <- function(vec = NULL, vec.field = NULL, isStrings = FALSE){
  vec <- vec[-NA]
  if (any(is.null(vec) || is.na(vec) || length(vec)<1 || all(length(vec)==1 && vec==""))){
    message("Your vec value doesn't have any values")
    return("1=1")
  }
  allres <- NA
  for (i in 1:length(vec)){
    thisres <- ifelse(isStrings, paste0("('_dOh_','", vec[i],"')"), paste0("('_dOh_',", vec[i],")"))
    allres <- c(allres, thisres)
  }
  allres <- allres[!is.na(allres)]
  res <- paste0("('_dOh_', ",vec.field,") IN (",paste0(allres, collapse=","),")")
  return(res)
}