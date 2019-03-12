VMSCombineSegments <-function(segmIDAdd = NULL){ 
  #compare segmentID with 
  if (!is.null(segmIDAdd)){  
    #HERE - must ensure that only new records are added!
    browser()
    seg= unique(areaRecs$SEGMID)
    addRecs = areaRecs[FALSE,]
    for (j in 1:length(seg)){
      changers = areaRecs[areaRecs$SEGMID==seg[j],]
      changers = changers[changers$VR_NUMBER %in% substr(segmIDAdd[,1], 12, 17),]
      for (l in 1:nrow(segmIDAdd)){
        changers = changers[as.integer(changers$POSITION_UTC_DATE) < substr(segmIDAdd[l,1], 1, 10),]
      }
      #changers are records where the segment current segment should be changed to whats
      #in the db so that they are shown to be clearly related
      if (nrow(changers)>0){
        theSeg = segmIDAdd[substr(segmIDAdd[,1], 12, 17) %in% changers$VR_NUMBER,"SEGMID", ]
        if (length(theSeg)>1)stop("error - too many matching segments")
        changers$SEGMID = theSeg
      }else{
        changers = areaRecs[areaRecs$SEGMID==seg[j],]
      }
      addRecs = rbind(addRecs,changers)
    }
    updatedRecs=anti_join(areaRecs,addRecs, by = c("VR_NUMBER", "LATITUDE", "LONGITUDE", "POSITION_UTC_DATE","SPEED_KNOTS","UPDATE_DATE",shp.field))
    
    allRecs=rbind(updatedRecs,addRecs)
  }else{
    allRecs=areaRecs
  }
  return(allRecs)
}