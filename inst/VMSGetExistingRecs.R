# create common functions ####
VMSGetExistingRecs <- function(thistblName = tblName, recStartDateSQL = NULL, 
                           recTime = "newest", recInArea = TRUE){
  # go to the potentially existing table, and retrieve record(s)
  
    if (recTime == "newest") {
      recTime = "MAX"
    } else if (recTime=="oldest"){
      recTime = "MIN"
    }
    if (recInArea == TRUE) {
      whereClause = "length(FULL_DESCR)>0"
    }else{
      whereClause = "1=1"
    }
  if (is.null(recStartDateSQL)){
    recSQL =  paste0("select DISTINCT VR_NUMBER,LATITUDE,LONGITUDE,POSITION_UTC_DATE,SPEED_KNOTS,FULL_DESCR, UPDATE_DATE, SEGMID
              FROM ",thistblName," 
              WHERE ",whereClause," AND POSITION_UTC_DATE = (select ",recTime,"(POSITION_UTC_DATE) from 
              ",thistblName," WHERE ",whereClause,")")
  } else{
    recSQL =  paste0("SELECT DISTINCT VR_NUMBER, LATITUDE, LONGITUDE, POSITION_UTC_DATE, SPEED_KNOTS, FULL_DESCR, UPDATE_DATE, SEGMID
              FROM  ",thistblName,"
              WHERE ",whereClause,"
              AND POSITION_UTC_DATE > to_date('",recStartDateSQL,"','YYYY-MM-DD HH24:MI:SS')")
  }
  rec <- tryCatch({
    rec = oracle_cxn$thecmd(oracle_cxn$channel,recSQL)
  },error=function(cond1){
    browser()
    if (grep("does not exist",cond1$message)>0){
      return("Nope")
    }
  })
  if(all(is.na(rec))) rec = "Empty"
  return(rec)
}
