#' @title combine_lists
#' @description This function assembles 2 lists into a single list.  When
#' elements from both lists have the same name, the values from the primary list
#' will be retained.
#' @param primary default is \code{NULL}
#' @param ancilliary default is \code{NULL}
#' @param quietly default is \code{FALSE}.  By default, this function will indicate the names and values of duplicated (i.e. ancilliary) list elements that will be dropped 
#' @return list of all of the arguments
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
combine_lists <- function(primary = NULL, ancilliary = NULL, quietly=FALSE){ 
  new <- ancilliary[setdiff(names(ancilliary),names(primary))]
  discarded <- ancilliary[intersect(names(ancilliary),names(primary))]
  kept <- c(primary, new)
  if (!quietly & length(discarded)>0){
    message("Ambiguous parameter(s) detected","\n")
    for(c in 1:length(discarded)){
      this = names(discarded)[c]
      wantthis <- kept[[this]]
      if (inherits(wantthis,"data.frame")) wantthis <-"<the data.frame>"
      that <- discarded[[this]] 
      if (inherits(that,"data.frame")) that <-"<the data.frame>"
      message(paste0('The parameter "',this,'" will use the value(s) of "', paste0(wantthis, collapse=","),'", not "',paste0(that, collapse=','),'"'),"\n")
    }
  }
  return(kept)
}
