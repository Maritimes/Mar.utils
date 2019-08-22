#' @title VMS_clean_recs
#' @description This function takes raw VMS data (i.e. a dataframe having 
#' sequential coordinates and times) and cleans it.  
#' @param df default is \code{NULL}.  This is the dataframe to be processed.  It 
#' should have coordinates in decimal degrees and they should be in fields 
#' called "LATITUDE" and "LONGITUDE".  It also needs a field with the time 
#' associated with each position, i.e. the  \code{timeField}.
#' @param objField default is \code{VR_NUMBER}. This is a field identifying which 
#' points are associated withe eachother. 
#' @param timeField default is \code{POSITION_UTC_DATE}. This is the field which 
#' will be used to calculate the time differences between records.  
#' @param lat.field the default is \code{"LATITUDE"}. the name of the field holding latitude values 
#' (in decimal degrees)
#' @param lon.field the default is \code{"LONGITUDE"}.  the name of the field holding longitude 
#' values (in decimal degrees)
#' @param minDist_m the default is \code{50}. This is the minimum distance (m) a vessel
#' must move from it's last position in order for the record to be kept.  This 
#' should be greater than zero to avoid records for vessels sitting in port. 
#' @param maxBreak_mins the default is \code{1440}.  This is the maximum time (in mins)
#' that is allowed between positions before a new "trek" is created. 
#' @return a dataframe with an additional "trek" column identifying a number of
#' discrete paths for each unique value of \code{objField}.
#' @family vms
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#' @note The resultant df will likely have less records than the source.
#' During the cleaning process, values for Latitude and Longitude are rounded 
#' from 5 decimal places to 4 (~1m --> 11m resolution), and times are rounded
#' to the nearest 5 minutes.  If a field called  "UPDATE_DATE" exists, it will
#' be used to "break ties" in the case of duplicate records (i.e. the more recent 
#' record will is retained.
#' Additionally,  data is grouped into "treks" which can be thought of as discrete
#' forays by a vessel.  A new trek occurs when the time between subsequent points 
#' for a vessel exceeds \code{maxBreak_mins}.  If no value is provided, 24 hours
#' (i.e. 1440 minutes) will be used.
VMS_clean_recs <-function(df=NULL,lat.field= "LATITUDE",lon.field="LONGITUDE",
                          objField = "VR_NUMBER", timeField ="POSITION_UTC_DATE",
                          minDist_m = 50, maxBreak_mins = 1440){
  #following are vars that will be created by data.table, and build errors
  #appear if we don't define them
  distCalc <- time_min <- elapsedDist_m <- elapsedTime_min <- .SD <- NULL
  `:=` <- function (x, value) value
  vmsdf=df
  n1 = nrow(vmsdf)
  # cat("initial no:",n1,"\n")
  #round values to remove near-duplicates:
  #1) dd coords to 4 decimals (~10m resolution);
  #2) time to nearest 5 minutes
  vmsdf[,lat.field]<-round( vmsdf[,lat.field],4)
  vmsdf[,lon.field]<-round( vmsdf[,lon.field],4)
  vmsdf[,timeField] <- as.POSIXct(round(as.numeric(vmsdf[,timeField])/(300))*(300),origin='1970-01-01')
  #remove the recs that our rounding has turned into duplicates
  vmsdf= unique(vmsdf)
  if ("UPDATE_DATE" %in% names(vmsdf)){
    #For cases where a vessel has multiple positions at a single time, I use 
    #UPDATE_DATE to get only the most recently updated position
    vmsdf = vmsdf[order(vmsdf[objField],vmsdf[timeField],vmsdf$UPDATE_DATE),] 
    vmsdf = data.table::setDT(vmsdf)
    vmsdf = vmsdf[,utils::tail(.SD,1),by=list("newObjField" = get(objField),"newTimeField" = get(timeField))]
    vmsdf = as.data.frame(vmsdf)
    vmsdf[objField]<-NULL
    vmsdf[timeField]<-NULL
    colnames(vmsdf)[colnames(vmsdf)=="newObjField"] <- objField
    colnames(vmsdf)[colnames(vmsdf)=="newTimeField"] <- timeField
  }
  
  vmsdf = data.table::setDT(vmsdf)
  vmsdf[,distCalc:=round(geosphere::distGeo(cbind(get(lon.field), get(lat.field)))),by=get(objField)]
  vmsdf[,time_min:=difftime(get(timeField), data.table::shift(get(timeField), fill = get(timeField)[1L]), units = "min"),by=get(objField)]
  vmsdf <- as.data.frame(vmsdf)
  vmsdf$time_min<-as.numeric(vmsdf$time_min)
  # pingRate = median(vmsdf$time_min) 
  # if (is.null(maxBreak_mins))  maxBreak_mins = 10*pingRate
  
  #distCalc above associates distance with the record *before* the movement 
  #occurred.  The following bumps all of the distance calculations down to the 
  #next record.
  vmsdf['distCalc'] <- c(-1, utils::head(vmsdf['distCalc'], dim(vmsdf)[1] - 1)[[1]])
  #if the position has not changed since the last one, then it's probably 
  #non-informative.  However, if enough time has passed (i.e. 10*pingRate), maybe
  #it's the start of a new trip? 
  vmsdf$KEEP<-NA
  vmsdf[which(vmsdf$distCalc >= minDist_m | vmsdf$distCalc ==-1 | vmsdf$time_min > maxBreak_mins), "KEEP"] <- TRUE
  vmsdf <- vmsdf[which(vmsdf$KEEP==TRUE),]
  # cat("post-redundants:",nrow(vmsdf),"\n")
  vmsdf$KEEP<-NULL
  vmsdf = data.table::setDT(vmsdf)
  vmsdf[,elapsedDist_m:=round(geosphere::distGeo(cbind(get(lon.field), get(lat.field)))),by=get(objField)]
  vmsdf[,elapsedTime_min:=difftime(get(timeField), data.table::shift(get(timeField), fill = get(timeField)[1L]), units = "min"),by=get(objField)]
  vmsdf <- as.data.frame(vmsdf)
  vmsdf$elapsedTime_min<-as.numeric(vmsdf$elapsedTime_min)
  vmsdf['elapsedDist_m'] <- c(NA, utils::head(vmsdf['elapsedDist_m'], dim(vmsdf)[1] - 1)[[1]])
  vmsdf$time_min<-NULL
  vmsdf$distCalc <- NULL
  
  #try to find treks (i.e. groups of positions for a vessel that are not interrupted
  #for more than the maximum allowable break (e.g. 10* the ping rate) at any 
  #point).  For example, if a vessel with a ping rate of 60 stops for 10 hrs, a 
  #new trek is started. The first appearance of a vessel will start with a case 
  #of elapsedDist_m of NA.  
  vmsdf$trek<-NA
  vmsdf[is.na(vmsdf$elapsedDist_m) | vmsdf$elapsedTime_min > maxBreak_mins,"trek"] <- seq.int(nrow(vmsdf[is.na(vmsdf$elapsedDist_m) | vmsdf$elapsedTime_min > maxBreak_mins,]))
  #Carry over the identified changes in trek to subsequent points until new trek
  na.locf <- function(x) {
    v <- !is.na(x)
    c(NA, x[v])[cumsum(v)+1]
  }
  vmsdf$trek <- na.locf(vmsdf$trek)
  trekpts <- stats::aggregate(
    x = list(cnt = vmsdf[,objField]),
    by = list(grp = vmsdf$trek
    ),
    length
  )
  vmsdf <- vmsdf[vmsdf$trek %in% trekpts[trekpts$cnt>1,"grp"],]
  # cat("post-single point trek removal:",nrow(vmsdf),"\n")
  # cat("Final no:", nrow(vmsdf),"\n")
  # cat( n1- nrow(vmsdf),"total records removed (", (nrow(vmsdf)/n1)*100,"%)\n" )
  # cat("assumed ping rate of",pingRate,"min\n") 
  return(vmsdf)
}