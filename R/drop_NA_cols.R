#' @title drop_NA_cols
#' @description This function takes a dataframe and removes any columns that are
#' entirely populated with NAs
#' @param df default is \code{NULL}. This is the dataframe you want to check
#' @param dropUninformative default is \code{F}.  This can be set to TRUE to drop
#' all columns that have a single unique value throughout the entuire dataframe. 
#' @param keepers default is \code{NULL}.This is a vector of columns you 
#' definitely want to retain, even all values are identical
#' @return dataframe
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
drop_NA_cols<-function(df = NULL, dropUninformative =F, keepers = NULL){
  
  if (dropUninformative){
    intIndex <- rownames(df)
    dropCols = NA
    for (i in 1:length(names(df))) {
      these = c(with(df, tapply(intIndex, df[names(df)[i]], FUN = function(x) length(unique(x)))))
      if (length(these) ==1) dropCols =c(dropCols, names(df)[i])
    }
    dropCols = dropCols[!is.na(dropCols) & !(dropCols %in% keepers)]
    df = df[, !names(df) %in% dropCols]   
  }
  df[sapply(df, function(x) all(is.na(x)))] <- NULL
  
  return(invisible(df))
}