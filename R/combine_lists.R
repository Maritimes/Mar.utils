#' @title combine_lists
#' @description This function assembles 2 lists into a single list.  When
#' elements from both lists have the same name, the values from the primary list
#' will be retained.
#' @param ancilliary default is \code{NULL}
#' @param primary default is \code{NULL}
#' @param quiet default is \code{TRUE}.  If FALSE, this function will notify the
#' user that the lists have similarly named elements, and will indicate which
#' values will be retained (i.e. those from the primary list)
#' @return list of all of the arguments
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
combine_lists <- function(ancilliary = NULL, primary = NULL, quiet= T){
  sent <- ancilliary[intersect(names(ancilliary),names(primary))]
  used <- primary[intersect(names(ancilliary),names(primary))]
  if (!quiet & length(setdiff(sent, used))>0){
    cat("Ambiguous parameter(s) detected","\n")
    unused <- sent[!(sent %in% used)]
    for(c in 1:length(used)){
      this = names(unused)[c]
      cat(paste0('The parameter ',this,' will use the value(s) of "', paste0(used[[this]], collapse=","),'", not "',paste0(sent[[this]], collapse=','),'"'),"\n")
    }
  }
  primary[names(ancilliary)]<-ancilliary
  return(primary)
}
