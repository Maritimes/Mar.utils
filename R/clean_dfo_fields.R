#' @title clean_dfo_fields
#' @description This function facilitates the dropping of fields from a dataframe.
#' It was built for specific fields in MARFIS that I never wanted (i.e. CUSER, 
#' CDATE, UUSER, UDATE), but can also drop other fields easily.
#' @param df default is \code{NULL}. This is the dataframe you want to drop
#' fields from.
#' @param customCrap default is \code{NULL}. This is a vector of fields you 
#' want removed from your dataframe.
#' @return returns a data.frame without the columns CUSER, CDATE, UUSER, UDATE,
#' or any of the ones specified in customCrap.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
clean_dfo_fields<- function(df = NULL, customCrap = NULL){
  knownCrap = c('CUSER','CDATE','UUSER','UDATE','LAST_UPDATE','LAST_UPDATE_BY',
                'LAST_UPDATE_DATE','CREATED_BY','CREATED_DATE','OWNER_GROUP','MON_DOC_CUSER')
  df = df[,!names(df) %in% knownCrap]
  df = df[,!names(df) %in% customCrap]
  return(df)
}