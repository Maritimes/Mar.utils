#' @title vali_dates
#' @description This function ensures that functions have valid start and end dates. If only\code{dateStart}
#' is provided, \code{endDate}  will be for 1 year later. If \code{year} is provided, \code{dateStart}  and
#' \code{dateEnd} will be Jan 1 and Dec 31 of that year, respectively.
#' @param dateStart default is \code{'NULL'}.  This can be a date in the format of 'YYYY-MM-DD' or
#' 'YYYY' (which will then become Jan 1st).
#' @param dateEnd default is \code{'NULL'}.  This can be a date in the format of 'YYYY-MM-DD' or
#' 'YYYY' (which will then become Dec 31st).
#' @param quietly default is \code{TRUE}. When TRUE, no output will be shown.
#' @param year default is \code{'NULL'}.
#' @family datesAndTimes
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
vali_dates <- function(dateStart = NULL, dateEnd = NULL, year = NULL, quietly = TRUE){
  if (!is.null(dateStart) & !is.null(year)){
    year <- NULL
    if (!quietly) cat('Both "dateStart" and "year" were supplied as parameters.  dateStart will be used.','\n')      
  }
  
  fourDigToDate <- function(xxxx = NULL){
    if (is.null(xxxx)) xxxx <- format(Sys.Date(), "%Y")
    dateStart<-as.Date(ISOdate(xxxx,1,1))
    dateEnd <- as.Date(ISOdate(xxxx,12,31))
    res= list()
    res[["dateStart"]]<- dateStart
    res[["dateEnd"]]<- dateEnd
    return(res)
  }
  if (!is.null(dateStart)){
    if (nchar(dateStart)==4) {
      if (!quietly) cat("dateStart looks like a year: using calendar year, ignoring dateEnd\n")
      res <- fourDigToDate(dateStart)
    } else {
      dateStart <- as.Date(dateStart, "%Y-%m-%d")
      if (!is.null(dateEnd)) {
        if (!quietly) cat("valid dateStart and dateEnd\n")
        dateEnd <- as.Date(dateEnd, "%Y-%m-%d")
      }else{
        if (!quietly) cat("valid dateStart, missing dateEnd - defaulting to 1 year\n")
        dateEnd <- as.POSIXlt(dateStart)
        dateEnd$year <- dateEnd$year + 1
        dateEnd$mday <- dateEnd$mday -1
        dateEnd <- as.Date(dateEnd)
      }
      res<-list()
      res[["dateStart"]]<-dateStart
      res[["dateEnd"]]<- dateEnd
    }
  }else if(!is.null(year)){
    if (nchar(year)!=4) {
      if (!quietly) cat("year supplied, but appears to be a date - just using the year portion, defaulting to 1 year\n")
      year = format(as.Date(as.POSIXlt(year)),"%Y")
    }
    res <- fourDigToDate(year)
  }else{
    if (!quietly) ccat("no dates provided, using current year\n")
    res <- fourDigToDate()
  }
  return(res)
}
