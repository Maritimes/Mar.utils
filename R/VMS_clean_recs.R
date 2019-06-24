#' @title VMS_clean_recs
#' @description This function takes raw VMS data (i.e. a dataframe having 
#' sequential coordinates and times), and removes duplicate positions.
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
#' @note Your resultant df will likely have less records than your source df.
#' During the cleaning process, values for Latitude and Longitude will be 
#' rounded to 4 decimals (~11m resolution), and values in your timeFields will be rounded 
#' to the nearest 5 minutes.  If a field called "UPDATE_DATE" exists, it will be 
#' used to grab only the most recent record in cases where multiple records have
#' the same values for both objField and timeField (e.g. vessel, and time of position)
#' @importFrom data.table :=
#' @importFrom data.table .SD
#' @importFrom data.table setDT
#' @importFrom utils tail
#' @family vms
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
VMS_clean_recs <-function(df=NULL,lat.field= "LATITUDE",lon.field="LONGITUDE",
                          objField = "VR_NUMBER", timeField ="POSITION_UTC_DATE"){
  #round dd coords to 4 decimals (~10m resolution)
  vmsdf=df
  n1 = nrow(vmsdf)
  vmsdf[,lat.field]<-round( vmsdf[,lat.field],4)
  vmsdf[,lon.field]<-round( vmsdf[,lon.field],4)
  #round time to nearest 5 minutes
  vmsdf[,timeField] <- as.POSIXct(round(as.numeric(vmsdf$POSITION_UTC_DATE)/(300))*(300),origin='1970-01-01')
  vmsdf= unique(vmsdf)
  
  if ("UPDATE_DATE" %in% names(vmsdf)){
    #For cases where a vessel has multiple positions at a single time, I use 
    #UPDATE_DATE to get only the most recently updated position
    vmsdf = vmsdf[order(vmsdf[objField],vmsdf[timeField],vmsdf$UPDATE_DATE),] 
    vmsdf = setDT(vmsdf)
    vmsdf = vmsdf[,tail(.SD,1),by=list("newObjField" = get(objField),"newTimeField" = get(timeField))]
    vmsdf = as.data.frame(vmsdf)
    vmsdf[objField]<-NULL
    vmsdf[timeField]<-NULL
    colnames(vmsdf)[colnames(vmsdf)=="newObjField"] <- objField
    colnames(vmsdf)[colnames(vmsdf)=="newTimeField"] <- timeField
  }
  cat( n1- nrow(vmsdf),"redundant records removed")
  return(vmsdf)
}