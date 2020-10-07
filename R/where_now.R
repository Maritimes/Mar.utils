#' @title where_now
#' @description This function indicates which function has been reached, and the 
#' parameters it is using
#' @param inf the default is \code{NULL}.  WOrks with \code{as.character(sys.calls()[[sys.nframe() - 1]])}. 
#' This is a method for extracting the function information.
#' @param lvl the default is \code{1}. This controls how many tabs are used to 
#' display the results.  It can be useful when functions call subfunctions
#' @return NULL - it just prints to the screen
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
where_now <- function(inf=NULL, lvl=1){
  inf1 <- inf[!(inf %in% c("do.call"))]
  prefix <- paste0(rep("\t",lvl), collapse="")
  cat(prefix, inf1[1],'(',paste0(inf1[2:length(inf1)], collapse = ","),')\n', sep='')
}

