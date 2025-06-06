#' @title drop_cols
#' @description This function takes a dataframe and drops or retains fields
#' based on the uniformity of their values.
#' @param df default is \code{NULL}. This is the dataframe you want to handle.
#' @param justDropNAs default is \code{TRUE}.  If this value is TRUE, the script 
#' will only drop fields that are solely populated with NAs.  If FALSE, it
#' will drop/retain iother fields depending on the following parameters. 
#' @param uniformFields default is \code{'retain'}.  This determines how the script
#' will handle uniform fields.  It can either retain them or drop them.  Valid
#' values are \code{'retain'} or \code{'drop'}.   
#' \code{'drop'} specifies that any field that has a single value for all 
#' entries will be dropped.  This is useful for identifying the fields that can 
#' be used to differentiate records.
#' \code{'retain'} specifies that only those fields which have a single 
#' value for all entries will be retained.  This is useful for identifying the 
#' fields with values that are shared by all records in a dataframe.
#' @param keepFields default is \code{NULL}.This is a vector of column names you 
#' definitely want to retain, regardless of the values.
#' @param quietly default is \code{FALSE}.  By default, this function will not output messages,
#' but they can be turned on by setting this to TRUE
#' @return dataframe
#' @family data_manipulation
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
drop_cols<-function(df = NULL, justDropNAs = TRUE, uniformFields = 'retain', keepFields = NULL, quietly=T){
  if (justDropNAs) {
    if (!quietly) message("\n","Just dropping NA columns")
    return(df[!sapply(df, function(x) all(is.na(x)))])
  }

  tempdf =df
  #NAs tend to get ignored by the uniqueness checks so we copy the df and replace NAs with -666
  #can't replace logicals or dates with -666, so convert these field types to numeric first

  tt<- lapply(tempdf,class)
  if (any("Date" %in% tt)) tempdf[, sapply(tempdf, class) %in% c('Date')]<-as.numeric(unlist(tempdf[, sapply(tempdf, class) %in% c('Date')]))
  if (any("logical" %in% tt)) tempdf[, sapply(tempdf, class) %in% c('logical')]<-as.numeric(tempdf[, sapply(tempdf, class) %in% c('logical')])
  if (any(c('character', 'numeric','integer') %in% tt)) tempdf[, sapply(tempdf, class) %in% c('character', 'numeric','integer')][is.na(tempdf[, sapply(tempdf, class) %in% c('character', 'numeric','integer')])]<--666

  uniformFields <- tolower(uniformFields)

    intIndex <- rownames(tempdf)
    samies = NA
    for (i in 1:length(names(tempdf))) {
      these = c(with(tempdf, tapply(intIndex, tempdf[names(tempdf)[i]], FUN = function(x) length(unique(x)))))
      if (length(these) ==1) samies =c(samies, names(tempdf)[i])
    }
    samies = c(samies, names(tempdf[sapply(tempdf, function(x) all(is.na(x)))]))
    samies = samies[!is.na(samies)]
    if (uniformFields=="drop"){
      finalFields = names(tempdf)[!(names(tempdf) %in% samies)]
    } else if (uniformFields=="retain"){
      finalFields = names(tempdf)[names(tempdf) %in% samies]
    }

    finalFields = c(finalFields,keepFields)
    if (length(finalFields)==1)df = df[names(df) %in% finalFields]
    if (length(finalFields)>1)df = df[,names(df) %in% finalFields]

  return(df)
  # return(invisible(df))
}
