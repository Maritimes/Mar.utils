#' @title combine_lists
#' @description This function assembles 2 lists into a single list.  When
#' elements from both lists have the same name, the values from the primary list
#' will be retained.
#' @param primary default is \code{NULL}
#' @param ancilliary default is \code{NULL}
#' @param quietly default is \code{TRUE}.  If FALSE, this function will notify the
#' user that the lists have similarly named elements, and will indicate which
#' values will be retained (i.e. those from the primary list)
#' @return list of all of the arguments
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
combine_lists <- function(primary = NULL, ancilliary = NULL, quietly= T){
  dups <- ancilliary[intersect(names(ancilliary),names(primary))]  #
  kept <- primary[intersect(names(ancilliary),names(primary))]
  ignored <- dups[!(dups %in% kept)]
    if (!quietly & length(setdiff(dups, kept))>0){
    cat("Ambiguous parameter(s) detected","\n")
    for(c in 1:length(kept)){
      this = names(ignored)[c]
      cat(paste0('The parameter ',this,' will use the value(s) of "', paste0(kept[[this]], collapse=","),'", not "',paste0(dups[[this]], collapse=','),'"'),"\n")
    }
  }
  primary <- append(primary, ancilliary[!(names(ancilliary) %in% names(ignored))])
  return(primary)
}
