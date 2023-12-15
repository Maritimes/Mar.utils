#' @title VMS_get_recs
#' @description This function extracts VMS data for a given timespan and area.  
#' A time buffer can be added returns other points that are not within the area
#' of interest, but give context to the path.
#' @param fn.oracle.username default is \code{'_none_'} This is your username for
#' accessing oracle objects. If you have a value for \code{oracle.username} 
#' stored in your environment (e.g. from an rprofile file), this can be left out
#' and that value will be used.  If a value for this is provided, it will take 
#' priority over your existing value.
#' @param fn.oracle.password default is \code{'_none_'} This is your password for
#' accessing oracle objects. If you have a value for \code{oracle.password}  
#' stored in your environment (e.g. from an rprofile file), this can be left out
#' and that value will be used.  If a value for this is provided, it will take 
#' priority over your existing value.
#' @param fn.oracle.dsn default is \code{'_none_'} This is your dsn/ODBC
#' identifier for accessing oracle objects. If you have a value for 
#' \code{oracle.dsn} stored in your environment (e.g. from an rprofile file), 
#' this can be left and that value will be used.  If a value for this is 
#' provided, it will take priority over your existing value.
#' @param usepkg default is \code{'rodbc'}. This indicates whether the 
#' connection to Oracle should use \code{'rodbc'} or \code{'roracle'} to 
#' connect.  rodbc is slightly easier to setup, but roracle will extract data 
#' ~ 5x faster.
#' @param hrBuffer default is \code{4}.  This is the number of hours worth of 
#' VMS data you would like to pad your area search by.  For example, if a vessel
#' has a single VMS position in the results, padding it will add additional 
#' points before and after the intrusion which can illustrate what it was doing 
#' at the time. 
#' @param minLon default is \code{NULL}. This can be used to set a minimum Longitude
#' @param maxLon default is \code{NULL}. This can be used to set a maximum Longitude
#' @param minLat default is \code{NULL}. This can be used to set a minimum Latitude
#' @param maxLat default is \code{NULL}. This can be used to set a maximum Latitude
#' @param dateStart default is \code{NULL}. This is the start date ('YYYY-MM-DD') 
#' for your VMS data search
#' @param dateEnd default is \code{NULL}. This is the end date ('YYYY-MM-DD') 
#' for your VMS data search. If this is left NULL, one year of data will be
#' returned 
#' @param vrnList default is \code{NULL}.  A vector or VRNs can be added so 
#' that the only positions returned match particular vessel(s).
#' @param rowNum default is \code{50000}.  This is the maximum number of VMS 
#' records that can be extracted. It is in place to prevent crashing your 
#' application.
#' @param quietly default is \code{FALSE}.  This indicates whether or not
#' information about the process should be shown.
#' @return a dataFrame
#' @family vms
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
VMS_get_recs <- function(fn.oracle.username = "_none_", 
                         fn.oracle.password = "_none_", 
                         fn.oracle.dsn = "_none_",
                         usepkg = 'rodbc', dateStart = NULL, dateEnd = NULL, 
                         vrnList = NULL, hrBuffer = 4,   
                         minLon = NULL, maxLon = NULL,
                         minLat = NULL, maxLat = NULL, rowNum = 50000,
                       quietly = F){
  if (is.null(dateEnd)) dateEnd = as.Date(dateStart) + lubridate::years(1)
  whereDateEnd = paste0("AND POSITION_UTC_DATE <= to_date('",dateEnd,"','YYYY-MM-DD')") 
  
  if (!is.null(vrnList)) {
    whereVRN = paste0("AND ",Mar.utils::big_in(vec=unique(vrnList), vec.field="VR_NUMBER", isStrings = T))
  }else{
    whereVRN = ""
  }
  sqlLimit = paste0(" AND ROWNUM <= ",rowNum)
  if (!is.null(minLon)){
    W_minX <- paste0("LONGITUDE >=",minLon)
  }else{
    W_minX <- " 1 = 1"
  }
  if (!is.null(maxLon)){
    W_maxX <- paste0("LONGITUDE <=",maxLon)
  }else{
    W_maxX <- "1 = 1"
  }
  if (!is.null(minLat)){
    W_minY <- paste0("LATITUDE >=",minLat)
  }else{
    W_minY <- "1 = 1"
  }
  if (!is.null(maxLat)){
    W_maxY <- paste0("LATITUDE <=",maxLat)
  }else{
    W_maxY <- "1 = 1"
  }
  run_ts = as.integer(Sys.time())
  # go to VMS source, and retrieve records
  recSQL = paste0("select VR_NUMBER,LATITUDE,LONGITUDE,POSITION_UTC_DATE,SPEED_KNOTS,UPDATE_DATE
              from MFD_OBFMI.VMS_ALL 
              WHERE POSITION_UTC_DATE>= to_date('",dateStart,"','YYYY-MM-DD') ",whereDateEnd, 
                  " AND (",W_minX, " AND ",
                          W_maxX, " AND ", 
                          W_minY,  " AND ",
                          W_maxY,") ",whereVRN, sqlLimit)
  oracle_cxn = make_oracle_cxn(fn.oracle.username =fn.oracle.username, 
                               fn.oracle.password = fn.oracle.password, 
                               fn.oracle.dsn = fn.oracle.dsn,
                               usepkg = usepkg, quietly = quietly)
  if (!is.list(oracle_cxn)) {
    cat("\nCan't do this without a DB connection.  Aborting.\n")
    return(NULL)
  }
  allRecs=oracle_cxn$thecmd(oracle_cxn$channel,recSQL)

  if (nrow(allRecs)<1){
    if (!quietly) cat(paste0("\n","No records returned"))
    return(NULL)
  }
  # if (ptsOnly) return(allRecs)
  # #set up something to hold the ones we'll keep
  # if (!is.null(shp)){
  #   saveRecs = allRecs[FALSE,]
  #   saveRecs = cbind(saveRecs,data.frame(SEGMID=character(), shp.field=character()))
  #   names(saveRecs)[names(saveRecs) == "shp.field"] <- shp.field
  # 
  #   areaRecs = identify_area(allRecs, agg.poly.shp = shp, agg.poly.field = shp.field)
  # 
  #   areaRecs = areaRecs[which(!is.na(areaRecs[shp.field]) & areaRecs[shp.field] != "Bad coordinate"),]
  #   if (nrow(areaRecs)<1){
  #     if (!quietly) cat(paste0("\n","No records in area detected"))
  #     return(NULL)
  #   }
  #   #maybe this is where we detect old segmid that we need to append to?
  #   allVess<-unique(areaRecs$VR_NUMBER)
  # 
  #   for (i in 1:length(allVess)){
  # 
  #     cat("\n","working on",allVess[i])
  #     # if (i==1)browser()
  #     thisVessRecs = areaRecs[areaRecs$VR_NUMBER == allVess[i],]
  #     thisVessRecs = thisVessRecs[order(thisVessRecs$POSITION_UTC_DATE),]
  #     if (nrow(thisVessRecs)>1){
  #       #find the time difference between successive recs
  #       thisVessRecs$LAG <-c(0,difftime(thisVessRecs[2:nrow(thisVessRecs),"POSITION_UTC_DATE"],thisVessRecs[1:(nrow(thisVessRecs)-1),"POSITION_UTC_DATE"],units = "hours"))     
  #       
  #       cnt = 1
  #       thisVessRecs$cnt <-NA
  #       for (j in 1:nrow(thisVessRecs)){
  #         #look at date range - anything with a gap larger than hrBuff will get 
  #         #broken into multiple "SEGMID"
  #         if (thisVessRecs[j,"LAG"]>hrBuffer)cnt=cnt+1
  #         thisVessRecs[j,"cnt"]=cnt
  #       }
  #       for (k in 1:cnt){
  #         #we've figured out how many trips there are so now we can get the non-area
  #         #records for this trip
  #         thisTrip = thisVessRecs[thisVessRecs$cnt==k,]
  #         thisTrip$LAG <- NULL
  #         thisTrip$SEGMID <- paste0(run_ts,"_",unique(thisVessRecs$VR_NUMBER),"_",k)
  #         
  #         minTime = min(range(thisTrip$POSITION_UTC_DATE))
  #         preContextThisTrip = allRecs[allRecs$VR_NUMBER == allVess[i] & 
  #                                        allRecs$POSITION_UTC_DATE >= (minTime - (hrBuffer *3600)) &
  #                                        allRecs$POSITION_UTC_DATE <= minTime,]
  #         maxTime = max(range(thisTrip$POSITION_UTC_DATE))
  #         postContextThisTrip = allRecs[allRecs$VR_NUMBER == allVess[i] & 
  #                                         allRecs$POSITION_UTC_DATE >= maxTime &
  #                                         allRecs$POSITION_UTC_DATE <= (maxTime + (hrBuffer *3600)),]
  #         allContextthisTrip = rbind(preContextThisTrip,postContextThisTrip)
  #         allContextthisTrip[shp.field] = NA
  #         allContextthisTrip$cnt = k
  #         allContextthisTrip$SEGMID = unique(thisTrip$SEGMID)
  #         allContextthisTrip = rbind(allContextthisTrip,thisTrip)
  #         allContextthisTrip$cnt<-NULL
  #         saveRecs = rbind(saveRecs,allContextthisTrip)
  #       }
  #     }
  #     cat("\n","finished ",allVess[i])
  #   }
  #   saveRecs = saveRecs[with(saveRecs,order(VR_NUMBER, POSITION_UTC_DATE)),]
  # }else{
  #   saveRecs = allRecs[with(allRecs,order(VR_NUMBER, POSITION_UTC_DATE)),]
  # }
  # 
  # if (simpleQC){
  # saveRecs = saveRecs[which(abs(round(saveRecs$LATITUDE,0))!=0 & 
  #                           abs(round(saveRecs$LONGITUDE,0)) !=0 & 
  #                           abs(round(saveRecs$LATITUDE,0))!=90 & 
  #                           abs(round(saveRecs$LONGITUDE,0))!=180),]
  # }
  
  return(allRecs)
}