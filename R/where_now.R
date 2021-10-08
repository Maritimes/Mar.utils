#' @title where_now
#' @description This function indicates which function has been reached.
#' @param returnTime default is \code{FALSE}.  If TRUE, this function will return a value for 
#' proc.time, facilitating timing of functions
#' @param callstack default is \code{sys.calls()}.  Not meant to be changed.
#' @return nothing, it just writes messages to the screen
#' @family debugging
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
where_now <- function(returnTime = FALSE, callstack=sys.calls()){
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
