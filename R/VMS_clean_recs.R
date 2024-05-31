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
#' @param minKnots  default is \code{NULL}.  This is the minimum vessel speed that should be include 
#' in the output
#' @param maxKnots default is \code{NULL}.  This is the maximum vessel speed that should be include 
#' in the output
#' @import data.table
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
                          minDist_m = 50, maxBreak_mins = 1440,
                          minKnots = NULL, maxKnots = NULL){
  LATITUDE__ <- LONGITUDE__ <- objField__ <- timeField__ <- NA
  colnames(df)[colnames(df)==lat.field] <- "LATITUDE__"
  colnames(df)[colnames(df)==lon.field] <- "LONGITUDE__"
  colnames(df)[colnames(df)==objField] <- "objField__"
  colnames(df)[colnames(df)==timeField] <- "timeField__"
  e <- new.env()
  e$loopagain <- TRUE
  
  addDistTime <- function(df=NULL){
    dfn_0 <- nrow(df)
    df = data.table::setDT(df)
    df[,distCalc:=round(geosphere::distGeo(cbind(LONGITUDE__, LATITUDE__))),by=objField__]
    df[,time_min:=as.numeric(difftime(timeField__, data.table::shift(timeField__, fill = timeField__[1L]), units = "min")),by=objField__]
    df <- data.table::setDF(df)
    #distCalc above associates distance with the record *before* the movement 
    #occurred.  The following bumps all of the distance calculations down to the 
    #next record.
    df['distCalc'] <- c(-1, utils::head(df['distCalc'], dim(df)[1] - 1)[[1]])
    df[is.na(df$distCalc),"distCalc"] <- -1
    df$KEEP<-FALSE
    df[df$distCalc == -1, "KEEP"] <- TRUE
    df[df$time_min <= maxBreak_mins,"KEEP"] <- TRUE
    df[df$distCalc >= minDist_m ,"KEEP"] <- TRUE
    # df[df$distCalc == 0, "KEEP"] <- FALSE
    df <- df[which(df$KEEP==TRUE),]
    df$KEEP<-NULL
    dfn_1 <- nrow(df)
    if (dfn_0 ==dfn_1){
      e$loopagain = FALSE
    }
    return(df)
  }
  
  #following are vars that will be created by data.table, and build errors
  #appear if we don't define them
  `:=` <- function (x, value) value
  distCalc <- time_min <- elapsedDist_m <- elapsedTime_min <- .SD <- UPDATE_DATE <- NULL
  n1 = nrow(df)
  #round values to remove near-duplicates:
  #1) dd coords to 4 decimals (~10m resolution);
  #2) time to nearest 5 minutes
  df$LATITUDE__<-round( df$LATITUDE__,4)
  df$LONGITUDE__<-round( df$LONGITUDE__,4)
  df <- df[!(df$LONGITUDE__ == 0 & df$LATITUDE__ == 0) & df$LONGITUDE__ >= -180 & df$LONGITUDE__ <= 180 & df$LATITUDE__ >= -90 & df$LATITUDE__ <= 90, ]
  df$timeField__ <- as.POSIXct(round(as.numeric(df$timeField__)/(300))*(300),origin='1970-01-01')
  #remove the recs that our rounding has turned into duplicates
  df= unique(df)
  if ("UPDATE_DATE" %in% names(df)){
    df = data.table::setorder(df, objField__, timeField__, UPDATE_DATE)
    df = data.table::setDT(df)
    df = df[,utils::tail(.SD,1),by=list("objField__" = objField__,"timeField__" = timeField__)]
  }
  
  while (e$loopagain == TRUE) {
    df<- addDistTime(df)
  }

  #calculate the instantaneous speed for each position (i.e. the distance from the previous position
  #in the amount of time it took)
  df$KNOTS_CALC <- 0
  df$KNOTS_CALC <- (df$distCalc/df$time_min)*0.0323974
  if(!is.null(minKnots))df<-df[df$KNOTS_CALC>=minKnots,]
  if(!is.null(maxKnots))df<-df[df$KNOTS_CALC<=maxKnots,]
  
  #try to find treks (i.e. groups of positions for a vessel that are not interrupted
  #for more than the maximum allowable break (e.g. 10* the ping rate) at any 
  #point).  For example, if a vessel with a ping rate of 60 stops for 10 hrs, a 
  #new trek is started. The first appearance of a vessel will start with a case 
  #of distCalc of NA.  
  df$trek<-NA
  df[is.na(df$distCalc) | df$distCalc < 0 | df$time_min > maxBreak_mins,"trek"] <- seq.int(nrow(df[is.na(df$distCalc) | df$distCalc < 0 | df$time_min > maxBreak_mins,]))
  #Carry over the identified changes in trek to subsequent points until new trek
  na.locf <- function(x) {
    v <- !is.na(x)
    c(NA, x[v])[cumsum(v)+1]
  }
  df$trek <- na.locf(df$trek)
  trekpts <- stats::aggregate(
    x = list(cnt = df$objField__),
    by = list(grp = df$trek
    ),
    length
  )
  df <- df[df$trek %in% trekpts[trekpts$cnt>1,"grp"],]
  
  colnames(df)[colnames(df)=="LATITUDE__"] <- lat.field
  colnames(df)[colnames(df)=="LONGITUDE__"] <- lon.field
  colnames(df)[colnames(df)=="objField__"] <- objField
  colnames(df)[colnames(df)=="timeField__"] <- timeField
  
  return(df)
}