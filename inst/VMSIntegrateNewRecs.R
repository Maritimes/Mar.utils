VMSIntegrateNewRecs <-function(newVMSRecs = NULL, allRecentRecs = NULL, shp.field= NULL){ 
  
  split2df <- function(vector=NULL, split_char=NULL){
    out <- strsplit(as.character(vector),split_char) 
    res = data.frame(vector,do.call(rbind, out))
    return(res)
  }
  
  #get the unique segments both both new and existing, and break them into df cols 
  
  #for the existing recs - we only want to update the segmids for records that are 
  #within the hrBuffer of an intersection with an area - otherwise, we'll
  #get all VMS points for vessels that have ever crossed a boundary

  segmIDExist = split2df(unique(allRecentRecs[!is.na(allRecentRecs[shp.field]),"SEGMID"]),"_")
  #segmIDExist = split2df(unique(allRecentRecs$SEGMID),"_")
  segmIDNew= split2df(unique(newVMSRecs$SEGMID),"_")
  
  #identify potential overlaps
  overLappers = newVMSRecs[newVMSRecs$POSITION_UTC_DATE >=  min(allRecentRecs$POSITION_UTC_DATE),]
  
  changed = overLappers[FALSE,]
  for (j in 1:nrow(segmIDNew)){
    #these are the new records that share a segmid
    changers = overLappers[overLappers$SEGMID==segmIDNew[j,1],]
    #for this segmid, see if we match an existing VR
    changers = changers[changers$VR_NUMBER %in% segmIDExist[,3],]
    if (nrow(changers)>0){
      
      theSeg = segmIDExist[segmIDExist[,3] %in% changers$VR_NUMBER,"vector"]
      changers$SEGMID = theSeg
      changed = rbind(changed,changers)
    }
  }
  unchangedRecs=anti_join(newVMSRecs,changed, by = c("VR_NUMBER", "LATITUDE", "LONGITUDE", "POSITION_UTC_DATE","SPEED_KNOTS","UPDATE_DATE",shp.field))
  allRecs = rbind(unchangedRecs, changed) 
  return(allRecs)
}