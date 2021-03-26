#' @title updateExpected
#' @description This function was created to identify the point at which certain expected values are dropped from a vector.  For example, a number of records 
#' might "go missing" while performing a series of filtering steps, and you want to identify which step is resulting in their loss.  By calling this function 
#' each step, it will report which of the expected values were lost at each point.  Further, 'expected' values are removed from what is looked for after they 
#' have been lost.  In this way, the function can be called numerous times successively, and it will only report the values that were lost at each step (not 
#' continue to report all of the lost values at each step).  
#' @param expected default is \code{NA} This is a vector of values that should be looked for
#' @param known default  is \code{NULL} This is the large vector that will be searched for the \code{expected} values
#' @param flagTxt default is \code{NULL} This is text you would like to be displayed before listing the results.  For debugging, it's useful to set this text as 
#' a description of the filtering step that was just done.
#' @param allOutput default is \code{FALSE} Normally, the script only provides messages when expected values are missing. If \code{TRUE}, the script will also 
#' output messages when 1) no expected values were lost, and 2) when none of the expected values remain. 
#' @param env default is \code{.GlobalEnv} For this script to work well on successive calls (e.g multiple filtering steps), information about your expected 
#' values must be stored somewhere outside of the function.  By default, a vector is created in your global environment.  Should you not want it there, you can 
#' create another environment, and have the information stored there instead.
#' @return nothing, it just writes messages to the screen
#' @family debugging
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note This function creates a vector (in the specified environment) based on the name of your "expected" vector.  For example, if your expected vector is 
#' called "myVector", than the created vector will be called "myVector__". This new vector will be "whittled down" as values are lost in the known vector.
#' @export
updateExpected<-function(expected = NA, known = NULL, flagTxt = NULL, allOutput = FALSE, env =.GlobalEnv){
  if (all(is.na(expected))) return(NA)
  if (!exists(deparse(substitute(env)), mode = "environment"))stop("Specified environment doesn't exist")
  
  expObjName <- paste0(deparse(substitute(expected)),"__")
  
  if(!exists(as.character(expObjName), envir = env)){
    assign(as.character(expObjName),expected, envir = env)
    missing <- expected
  }else{
    missing <- get(expObjName, envir = env)
  }
  
  theseMissing = sort(missing[!(missing %in% known) ])
  stillMissing <- sort(missing[!(missing %in% theseMissing)])
  if (length(theseMissing)>0){
    if (!is.null(flagTxt)) message(flagTxt)
    message(paste("\tLost: ",paste0(theseMissing, collapse=", ")))
  }else{
    if (allOutput)message("\tNone of the expected records were lost")
  }
  assign(as.character(expObjName), value = stillMissing, envir = env)
  if (length(stillMissing)==0 & allOutput) message("\tNo expected records remain")
}

