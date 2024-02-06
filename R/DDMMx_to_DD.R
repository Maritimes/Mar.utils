#' @title DDMMx_to_DD
#' @description This utility will convert coordinates from various DDMM formats to Decimal degrees.
#' The new decimal degrees fields will be added as new columns called "LAT_DD" and "LON_DD". 
#' @param format the default is \code{"DDMMMM"}, but \code{"DDMMSS"} is also valid.  \code{'DDMMMM'} 
#' values are typically cases where the degrees are followed by decimal minutes (with or without the decimal), 
#' such as 44°4.7' is written as "44047", "4404.7", "440470", or "4404.70".  \code{"DDMMSS"} values can look identical, 
#' but the last four digits represent minutes and seconds.  For example, the value of 44°4'42" 
#' would be written as "440442". 
#' @param df a dataframe to be analyzed. If left \code{NULL}, a value for \code{db} should be provided
#' @param lat.field the default is \code{"LATITUDE"}. This is the name of the 
#' field holding latitude values (in decimal degrees)
#' @param lon.field the default is \code{"LONGITUDE"}.  This is the name of the 
#' field holding longitudevalues (in decimal degrees)
#' @param WestHemisphere the default is \code{TRUE}.  It true, this ensure that all returned 
#' Longitudes are negative.  If false, the original sign is retained.
#' @return the original dataframe is returned, but it will have 2 additional fields  - LAT_DD and 
#' LON_DD
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
DDMMx_to_DD <- function(df=NULL, format="DDMMMM", lat.field=NULL, lon.field=NULL, WestHemisphere = T){

  #this function will fail west of Manitoba - it can't handle 3 digit longitudes) (e.g.-127MM.MMM)
  makeDD <- function(DDMM= NULL, WestHemisphere =FALSE){
    #capture original sign, then remove sign
    osigns<-substr(DDMM, 1, 1)=="-"
    osigns[is.na(osigns)]<- FALSE
    DDMM <- as.numeric(gsub("^-", "", DDMM))
    DDMM <- as.numeric(gsub("\\.", "", DDMM))
    
    #get value to 6 digits long (without turning to character)
    n <- 6 - nchar(DDMM)
    tt<- data.frame(DDMM=DDMM, nchar=n)
    tt[!is.na(tt$DDMM), "DDMM"]<- tt[!is.na(tt$DDMM), "DDMM"]*10^tt[!is.na(tt$DDMM), "nchar"]
    DDMM <- tt$DDMM
    if(format=="DDMMMM"){
      
      #add decimal in correct spot (i.e. DDMM(.)MM); convert to dd (ie DD+MM.MM/60)
      pre_dd <- paste0(substr(DDMM, 1, 4), ".", substr(DDMM, 5, nchar(DDMM)))
      pre_dd[pre_dd=="NA.NA"]<-NA
      dd <- round(as.numeric(substr(pre_dd, 1, 2))+ (as.numeric(substr(pre_dd, 3, nchar(pre_dd)))/60),6)
    } else if(format=="DDMMSS"){
      dd <- round(as.numeric(substr(DDMM, 1, 2)) + as.numeric(substr(DDMM, 3, 4))/60 + as.numeric(substr(DDMM, 5, 6))/3600 ,6)
    }
    
    # if requested, make all values negative; otherwise, reapply original sign
    if (WestHemisphere){
      dd <- dd*-1
    }else if (length(dd[osigns]>0)){
      dd[osigns] <- dd[osigns]*-1
    }
    return(dd)
  }
  df$LAT_DD <- makeDD(df[,lat.field], WestHemisphere = FALSE)
  df$LON_DD <- makeDD(df[,lon.field], WestHemisphere )
  return(df)
}