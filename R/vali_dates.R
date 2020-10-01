#' @title vali_dates
#' @description This function ensures that functions have valid start and end dates. If only\code{startDate}
#' is provided, \code{endDate}  will be for 1 year later. If \code{year} is provided, \code{startDate}  and
#' \code{endDate} will be Jan 1 and Dec 31 of that year, respectively.
#' @param dateStart default is \code{'NULL'}.  This can be a date in the format of 'YYYY-MM-DD' or
#' 'YYYY' (which will then become Jan 1st).
#' @param dateEnd default is \code{'NULL'}.  This can be a date in the format of 'YYYY-MM-DD' or
#' 'YYYY' (which will then become Dec 31st).
#' @param quietly default is \code{FALSE}. When TRUE, no output will be shown.
#' @param year default is \code{'NULL'}.
#' @family datesAndTimes
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
vali_dates <- function(dateStart = NULL, dateEnd = NULL, year = NULL, quietly = FALSE){
  if (!is.null(dateStart) & !is.null(year)){
    year <- NULL
    if (!quietly) cat('Both "dateStart" and "year" were supplied as parameters.  dateStart will be used.','\n')      
  }
  if(!is.null(year)){
    dateStart<-year
    dateEnd <- NULL
    year<-NULL  
  }

  if(!is.null(dateStart)){
    if (nchar(dateStart)==4){
      dateStart <- try( as.Date( paste0(as.character(dateStart),"-01-01"), format= "%Y-%m-%d", origin="1970-01-01" ) )
    }else{
      dateStart <- try( as.Date( as.character(dateStart), format= "%Y-%m-%d", origin="1970-01-01" ) )
    }
    if( class( dateStart ) == "try-error" || is.na( dateStart ) ){
      stop("\n","The value for dateStart was not a valid year (YYYY) or date (YYYY-MM-DD)")
    }else{
      dateStart <- as.POSIXct(format(as.Date(dateStart), "%Y-%m-%d"), origin = "1970-01-01")
    }
  }else{
    dateStart <- as.POSIXct(format(as.Date(paste0(as.numeric(format(Sys.Date(), "%Y")),"-01-01")), origin = "1970-01-01"))
  }

  if(!is.null(dateEnd)){
    if (nchar(dateEnd)==4){
      dateEnd <- try( as.Date( paste0(as.character(dateEnd),"-12-31"), format= "%Y-%m-%d", origin="1970-01-01" ) )
    }else{
      dateEnd <- try( as.Date( as.character(dateEnd), format= "%Y-%m-%d", origin="1970-01-01" ) )
    }
    if( class( dateEnd ) == "try-error" || is.na( dateEnd ) ){
      stop("\n","The value for dateEnd was not a valid year (YYYY) or date (YYYY-MM-DD)")
    }else{
      dateEnd <- as.POSIXct(format(as.Date(dateEnd), "%Y-%m-%d"), origin = "1970-01-01")
    }
  }else{
    dateEnd <- as.POSIXlt(dateStart)
    dateEnd$year <- dateEnd$year + 1
    dateEnd <- as.POSIXct(format(as.Date(dateEnd), "%Y-%m-%d"), origin = "1970-01-01")
    if (!quietly) cat("No endDate was supplied, so endDate was set to be 1 yr after startDate","\n")
  }

  if (dateStart>dateEnd){
    stop("\n","dateStart occurs after dateEnd.  This is not possible.")
  }
  res= list()
  res[["dateStart"]]<- dateStart
  res[["dateEnd"]]<- dateEnd
  return(res)
}