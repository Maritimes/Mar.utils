#' @title make_segments_isdb
#' @description This function takes a df of the isdb data, and creates line segments for each 
#' distinct FISHSET_ID. If specified, it will generate spatial layers within a gpkg file.
#' Each line will have the following columns in the resulting data frame:
##' \itemize{
##'  \item \code{FISHSET_ID} This uniquely identifies a set
##'  \item \code{LEN_KM} This field shows the calculated distance of the 
##'  resultant line in kms
##'  \item \code{SET_NO} This uniquely identifies a set
##'  \item \code{N_VALID_VERT} This field shows how many vertices appear 
##'  correct after NAs, 0s and other problematic values have been dropped
##'  \item \code{SETSTART} This is the earliest time associated with a set
##'  \item \code{SETEND}  This is the last time associated with a set
##'  \item \code{SET_DUR} This is the difference in time (in hours) between the first and last 
##'  position of a set
##'  }
#' @param isdb.df This is the dataframe you want to plot.  isdb dataframes are
#' characterized by the existence of one of the following sets of fields:
#' ISSETPROFILE_WIDE - based:
##' \itemize{
##'  \item \code{FISHSET_ID}
##'  \item \code{SET_NO}
##'  \item \code{LAT1}, \code{LONG1} and \code{DATE_TIME1}
##'  \item \code{LAT2}, \code{LONG2} and \code{DATE_TIME2}
##'  \item \code{LAT3}, \code{LONG3} and \code{DATE_TIME3}
##'  \item \code{LAT4}, \code{LONG4} and \code{DATE_TIME4}
##' }
##' or
##' ISSETPROFILE - based
##' \itemize{
##' \item \code{FISHSET_ID}
##'  \item \code{SET_NO}
##'  \item \code{LATITUDE}, \code{LONGITUDE}, \code{DATETIME} and \code{PNTCD_ID}
##' }
##' 
#' @param filename default is \code{NULL}.  If you are outputting shapefiles, 
#' you can specify a name for them here.  They will also get a timestamp. 
#' @param create.spatial default is \code{TRUE}.  This indicates whether or not to create a gpkg 
#' file in your working directory.
#' @family spatial
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
make_segments_isdb <- function(isdb.df, 
                               filename = NULL, 
                               create.spatial=TRUE){
  FISHSET_ID <- SET_NO <- DATETIME <- NA
  name=""
  ts = format(Sys.time(), "%Y%m%d_%H%M")
  if (is.null(filename)) {
    name = ts
  }else{
    name = filename
    name = gsub('()','',name)
    name = gsub('\\.','',name)
    name = paste(name,"_",ts,sep="" )
  }
  
  #ISSETPROFILE & ISSETPROFILE_WIDE contain the same, but differently formatted data
  #this created a common format for use by the rest of the function
  if (any(names(isdb.df) %in% "LAT1")){
    p1=cbind(isdb.df[c("FISHSET_ID","SET_NO", "LAT1","LONG1", "DATE_TIME1")],"1")
    colnames(p1)<-c("FISHSET_ID","SET_NO", "LATITUDE", "LONGITUDE", "DATETIME","PNTCD_ID")
    p2=cbind(isdb.df[c("FISHSET_ID","SET_NO","LAT2","LONG2", "DATE_TIME2")],"2")
    colnames(p2)<-c("FISHSET_ID","SET_NO", "LATITUDE", "LONGITUDE", "DATETIME","PNTCD_ID")
    p3=cbind(isdb.df[c("FISHSET_ID","SET_NO","LAT3","LONG3", "DATE_TIME3")],"3")
    colnames(p3)<-c("FISHSET_ID","SET_NO", "LATITUDE", "LONGITUDE", "DATETIME","PNTCD_ID")
    p4=cbind(isdb.df[c("FISHSET_ID","SET_NO","LAT4","LONG4", "DATE_TIME4")],"4")
    colnames(p4)<-c("FISHSET_ID","SET_NO", "LATITUDE", "LONGITUDE", "DATETIME","PNTCD_ID")
    isdbPos <- rbind(p1,p2,p3,p4)
    rm(p1)
    rm(p2)
    rm(p3)
    rm(p4)
  }else if (any(names(isdb.df) %in% "SETTIME")){
    isdb.df$DATETIME <- as.POSIXct(as.character(paste(lubridate::date(isdb.df$SETDATE), 
                                                      format(strptime(sprintf("%04d", isdb.df$SETTIME), format="%H%M"), format = "%H:%M"))), 
                                   format="%Y-%m-%d %H:%M", tz="America/Halifax")
    
    isdbPos <- isdb.df[order(isdb.df$FISHSET_ID, isdb.df$PNTCD_ID), c("FISHSET_ID","SET_NO", "LATITUDE", "LONGITUDE", "DATETIME", "PNTCD_ID")]
    isdbPos$SETTIME <- NULL
    isdbPos$SETDATE <- NULL
  }else{
    stop("Can't detect format of incoming ISDB data.  It should be in the format of ISSETPROFILE or ISSETPROFILE_WIDE")
  }
  
  isdbPos <- isdbPos[order(isdbPos$FISHSET_ID, isdbPos$PNTCD_ID),]
  isdbPos[isdbPos==0] <- NA
  isdbPos <- isdbPos[stats::complete.cases(isdbPos),] 
  isdbData <- isdbPos |> 
    dplyr::group_by(FISHSET_ID, SET_NO) |> 
    dplyr::summarise(
      SETSTART = min(DATETIME),
      SETEND = max(DATETIME),
      N_VALID_VERT = length(DATETIME), 
      .groups = "keep"
    )
  isdbPts <- as.vector(isdbData[isdbData$N_VALID_VERT<2,"FISHSET_ID"])
  if(length(isdbPts)>0){
    isdbDataPts <- isdbPos[isdbPos$FISHSET_ID %in% isdbPts,]
    isdbData <- isdbData[!isdbData$FISHSET_ID %in% isdbPts,]
    isdbPts_sf <- df_to_sf(isdbDataPts, lat.field = "LATITUDE", lon.field = "LONGITUDE", type = "points")
    message(length(isdbPts), " instances were detected where a set only had a single valid position.
These will be added to the gpkg file if create.spatial = TRUE")
  }
  isdbData$SET_DUR_HRS <- round(as.numeric(difftime(isdbData$SETEND,isdbData$SETSTART, units="hours")),2)
  lines_sf <- df_to_sf(isdbPos, lat.field = "LATITUDE", lon.field = "LONGITUDE", primary.object.field = "FISHSET_ID", order.field = "PNTCD_ID",type = "lines")
  lines_sf$LEN_KM <- round(as.numeric(sf::st_length(lines_sf)/1000),2)
  lines_sf<- merge(lines_sf, isdbData)
  
  if (create.spatial) {
    df_sf_to_gpkg(lines_sf, layerName = paste0(name,"_lines"), gpkgName = "make_segments_isdb.gpkg")
    if(length(isdbPts)>0) df_sf_to_gpkg(isdbDataPts, layerName = paste0(name,"_points"), gpkgName = "make_segments_isdb.gpkg")
  }
  return(lines_sf)
}
