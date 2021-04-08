#' @title changeDetector
#' @description This function compares a dataframe before and after something has been done to it.  It will indicate the number of rows (before and after), and 
#' if \code{fields} are specified, it will also indicate how many unique values of each of the specified fields were present before and after the change.
#' @param pre_ default is \code{NA} This is dataframe prior to the change
#' @param post_ default  is \code{NULL} TThis is dataframe after the change
#' @param fields default  is \code{NULL} This is a vector of field names for which you are interested in the number of unique values before and after the change
#' @param flagTxt default is \code{NULL} This is text you would like to be displayed before listing the results.  For debugging, it's useful to set this text as 
#' a description of the processing step that was just done.
#' @return nothing, it just writes messages to the screen
#' @family debugging
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
changeDetector <- function(pre_ = NULL, post_ = NULL, fields = NULL, flagTxt = NULL){
  if (!is.null(flagTxt)) message(flagTxt)
  if (is.data.frame(pre_) & is.data.frame(post_)) {
    message(paste0("\t",deparse(substitute(pre_))," (nrow change): ", nrow(pre_)," --> ", nrow(post_)))
    for (i in 1:length(fields)){
      message(paste0("\tunique records in ", fields[i],": ", length(unique(pre_[,fields[i]]))," --> ", length(unique(post_[,fields[i]]))))
    }
  }
}
