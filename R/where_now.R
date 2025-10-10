#' @title where_now
#' @description **DEPRECATED:** This function indicates which function has been reached.
#' 
#' **This function is deprecated and will be removed in a future version.** 
#' Please use \code{\link{debugMode}} instead, which provides similar functionality without 
#' requiring code changes to your functions.
#' 
#' To replace \code{where_now()} calls:
#' \itemize{
#'   \item Remove all \code{where_now()} calls from your functions
#'   \item Use \code{debugMode(TRUE)} to enable debugging for all package functions
#'   \item Use \code{debugMode(FALSE)} to disable debugging
#' }
#' 
#' The new approach provides automatic entry/exit messages with timing for all functions,
#' eliminating the need to manually add debugging code.
#' 
#' @param returnTime default is \code{FALSE}.  If TRUE, this function will return a value for 
#' proc.time, facilitating timing of functions
#' @param callstack default is \code{sys.calls()}.  Not meant to be changed.
#' @return nothing, it just writes messages to the screen
#' @family debugging
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @seealso \code{\link{debugMode}} for the replacement debugging approach
#' @export
where_now <- function(returnTime = FALSE, callstack=sys.calls()){
  
  # Issue deprecation warning
  .Deprecated(
    new = "debugMode",
    package = "Mar.utils",  # Replace with your actual package name
    msg = paste(
      "where_now() is deprecated and will be removed in a future version.",
      "Please use debugMode(TRUE) instead, which provides automatic debugging",
      "for all functions without requiring code changes.",
      "See ?debugMode for details."
    )
  )
  
  #entirely stolen from https://stackoverflow.com/questions/7307987/logging-current-function-name
  clean_where <- function(x){
    val <- sapply(x, function(xt){
      z <- strsplit(paste(xt, collapse="\t"), "\t")[[1]]
      switch(z[1],
             "lapply" = z[3],
             "sapply" = z[3],
             "do.call" = z[2],
             "function" = "FUN",
             "source" = "###",
             "eval.with.vis" = "###",
             z[1]
      )
    })
    val[grepl("\\<function\\>", val)] <- "FUN"
    val <- val[!grepl("(###|FUN)", val)]
    val <- utils::head(val, -1)
    paste(val, collapse="|")
  }
  
  cs <- callstack
  cs <- clean_where(cs)
  message(cs)
  if(returnTime)return(proc.time())
}
