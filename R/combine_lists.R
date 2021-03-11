#' @title combine_lists
#' @description This function assembles 2 lists into a single list.  When
#' elements from both lists have the same name, the values from the primary list
#' will be retained.
#' @param primary default is \code{NULL}
#' @param ancilliary default is \code{NULL}
#' @return list of all of the arguments
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
combine_lists <- function(primary = NULL, ancilliary = NULL){
  kept <- primary[intersect(names(ancilliary),names(primary))]
    # if (!quietly & length(setdiff(dups, kept))>0){
    # cat("Ambiguous parameter(s) detected","\n")
    # for(c in 1:length(kept)){
    #   this = names(ignored)[c]
    #   cat(paste0('The parameter ',this,' will use the value(s) of "', paste0(kept[[this]], collapse=","),'", not "',paste0(dups[[this]], collapse=','),'"'),"\n")
    # }
    # }

  final <- append(kept, ancilliary[!(names(ancilliary) %in% names(kept))])
  return(final)
}
