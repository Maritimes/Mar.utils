#' @title VMS_get_recs
#' @description This function extracts VMS data for a given timespan and area.  
#' A time buffer can be added returns other points that are not within the area
#' of interest, but give context to the path.
#' @param usepkg default is \code{'roracle'}. This indicates whether the 
#' connection to Oracle should use \code{'rodbc'} or \code{'roracle'} to 
#' connect.  rodbc is slightly easier to setup, but roracle will extract data 
#' ~ 5x faster.
#' @param shp default is \code{NULL}.  This is the shapefile that has 
#' polygons that should be checked for existence of VMS data.
#' @param shp.field default is \code{NULL}.  This is the field in the shapefile
#' that should attached to the reurned data.  This will typically be a field 
#' that identifies the area(s).
#' @param hrBuffer default is \code{4}.  This is the number of hours worth of 
#' VMS data you would like to pad your area search by.  For example, if a vessel
#' has a single VMS position in the results, padding it will add additional 
#' points before and after the intrusion which can illustrate what it was doing 
#' at the time. 
#' @param dateStart default is \code{NULL}. This is the start date ('YYYY-MM-DD') 
#' for your VMS data search
#' @param dateEnd default is \code{NULL}. This is the end date ('YYYY-MM-DD') 
#' for your VMS data search
#' @param vrnList default is \code{NULL}.  A vector or VRNs can be added so 
#' that the only positions returned match particular vessel(s).
#' @param rowNum default is \code{50000}.  In the event of populating a table 
#' with VMS data, no end date is entered.  If this is the case, this value will 
#' limit how many VMS records are pulled on each function call.  It is in place l
#' to prevent crashing your application.
#' @return a DataFrame with the column \code{agg.poly.field} added (if value for 
#' \code{shp} is supplied)
# @importFrom sp CRS
# @importFrom sp spTransform
# @importFrom sp coordinates
# @importFrom sp proj4string
# @importFrom rgdal readOGR
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
VMS_get_recs <- function(usepkg = 'roracle', dateStart = NULL, dateEnd = NULL, 
                       vrnList = NULL, hrBuffer = 4,  shp = NULL, shp.field=NULL, 
                       rowNum = 50000){
  if (!is.null(dateEnd)) {
    whereDateEnd = paste0("AND POSITION_UTC_DATE < to_date('",dateEnd,"','YYYY-MM-DD HH24:MI:SS')") 
  }else{
    whereDateEnd = paste0("AND ROWNUM <= ",rowNum)
  }
  if (!is.null(vrnList)) {
    whereVRN = paste0("AND VR_NUMBER IN (",SQL_in(vrnList, apos = FALSE),")") 
  }else{
    whereVRN = ""
  }
  
  run_ts = as.integer(Sys.time())
  # go to VMS source, and retrieve records
  recSQL = paste0("select VR_NUMBER,LATITUDE,LONGITUDE,POSITION_UTC_DATE,SPEED_KNOTS,UPDATE_DATE
              from MFD_OBFMI.VMS_ALL 
              WHERE POSITION_UTC_DATE> to_date('",dateStart,"','YYYY-MM-DD HH24:MI:SS') ",whereDateEnd, 
                  " ",whereVRN
  )
  oracle_cxn = make_oracle_cxn(usepkg)
  
  allRecs=oracle_cxn$thecmd(oracle_cxn$channel,recSQL)
  #set up something to hold the ones we'll keep
  if (!is.null(shp)){
    
    saveRecs = allRecs[FALSE,]
    saveRecs = cbind(saveRecs,data.frame(SEGMID=character(), shp.field=character()))
    names(saveRecs)[names(saveRecs) == "shp.field"] <- shp.field
    if (nrow(allRecs)==0) stop("No records returned")
    areaRecs = identify_area(allRecs, agg.poly.shp = shp, agg.poly.field = shp.field)
    areaRecs = areaRecs[which(!is.na(areaRecs[shp.field]) & areaRecs[shp.field] != "Bad coordinate"),] 
    if (nrow(areaRecs)==0) stop("No records in area detected")
    #maybe this is where we detect old segmid that we need to append to?
    allVess<-unique(areaRecs$VR_NUMBER)
    for (i in 1:length(allVess)){
      # if (i==1)browser()
      thisVessRecs = areaRecs[areaRecs$VR_NUMBER == allVess[i],]
      thisVessRecs = thisVessRecs[order(thisVessRecs$POSITION_UTC_DATE),]
      if (nrow(thisVessRecs)>1){
        #find the time difference between successive recs
        thisVessRecs$LAG <-c(0,difftime(thisVessRecs[2:nrow(thisVessRecs),"POSITION_UTC_DATE"],thisVessRecs[1:(nrow(thisVessRecs)-1),"POSITION_UTC_DATE"],units = "hours"))     
        
        cnt = 1
        thisVessRecs$cnt <-NA
        for (j in 1:nrow(thisVessRecs)){
          #look at date range - anything with a gap larger than hrBuff will get 
          #broken into multiple "SEGMID"
          if (thisVessRecs[j,"LAG"]>hrBuffer)cnt=cnt+1
          thisVessRecs[j,"cnt"]=cnt
        }
        for (k in 1:cnt){
          #we've figured out how many trips there are so now we can get the non-area
          #records for this trip
          thisTrip = thisVessRecs[thisVessRecs$cnt==k,]
          thisTrip$LAG <- NULL
          thisTrip$SEGMID <- paste0(run_ts,"_",unique(thisVessRecs$VR_NUMBER),"_",k)
          
          minTime = min(range(thisTrip$POSITION_UTC_DATE))
          preContextThisTrip = allRecs[allRecs$VR_NUMBER == allVess[i] & 
                                         allRecs$POSITION_UTC_DATE >= (minTime - (hrBuffer *3600)) &
                                         allRecs$POSITION_UTC_DATE <= minTime,]
          maxTime = max(range(thisTrip$POSITION_UTC_DATE))
          postContextThisTrip = allRecs[allRecs$VR_NUMBER == allVess[i] & 
                                          allRecs$POSITION_UTC_DATE >= maxTime &
                                          allRecs$POSITION_UTC_DATE <= (maxTime + (hrBuffer *3600)),]
          allContextthisTrip = rbind(preContextThisTrip,postContextThisTrip)
          allContextthisTrip[shp.field] = NA
          allContextthisTrip$cnt = k
          allContextthisTrip$SEGMID = unique(thisTrip$SEGMID)
          allContextthisTrip = rbind(allContextthisTrip,thisTrip)
          allContextthisTrip$cnt<-NULL
          saveRecs = rbind(saveRecs,allContextthisTrip)
        }
      }
      
    }
    saveRecs= saveRecs[order(c(saveRecs$VR_NUMER, saveRecs$POSITION_UTC_DATE)),]
  }else{
    saveRecs = allRecs[order(c(allRecs$VR_NUMER, allRecs$POSITION_UTC_DATE)),]
  }
  return(saveRecs)
}