#' @title rename_fields
#' @description This function facilitates the renaming of fields in a dataframe.
#' @param df default is \code{NULL}. This is the dataframe you want to modify 
#' column names in.
#' @param oldFields default is \code{NULL}. This is a vector of fieldnames you 
#' want to rename.
#' @param newFields default is \code{NULL}. This is a vector of the new fieldnames 
#' you want to use. 
#' @return returns a data.frame with the new names.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note The number of items in oldFields and newFields must be identical, and 
#' the order matters in that newFields[3] will replace oldFields[3].
#' @export 
rename_fields <- function(df = NULL, oldFields = NULL, newFields = NULL){
  if ((length(oldFields) != length(newFields)) | (length(oldFields) + length(newFields)<2) ){
    message("oldFields and newFields vectors need to be the same length")
    return(df)
  }else{
    for (i in 1:length(oldFields)){
      colnames(df)[colnames(df)==oldFields[i]] <- newFields[i]
    }
    return(df)
  }
}