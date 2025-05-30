#' @title VMS_get_recs
#' @description This function extracts VMS data for a given timespan and area.  
#' A time buffer can be added returns other points that are not within the area
#' of interest, but give context to the path.
#' @param cxn A valid Oracle connection object. This parameter allows you to 
#' pass an existing connection, reducing the need to establish a new connection 
#' within the function. If provided, it takes precedence over the connection-
#' related parameters.
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
VMS_get_recs <- function(cxn = NULL, 
                         dateStart = NULL, dateEnd = NULL, 
                         vrnList = NULL, hrBuffer = 4,   
                         minLon = NULL, maxLon = NULL,
                         minLat = NULL, maxLat = NULL, rowNum = 50000,
                       quietly = F){

  deprecationCheck(fn.oracle.username = fn.oracle.username, 
                   fn.oracle.password = fn.oracle.password, 
                   fn.oracle.dsn = fn.oracle.dsn, 
                   usepkg = usepkg)
  
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
    thecmd <- Mar.utils::connectionCheck(cxn)

  allRecs = thecmd(cxn, recSQL)

  if (nrow(allRecs)<1){
    if (!quietly) cat(paste0("\n","No records returned"))
    return(NULL)
  }
  
  return(allRecs)
}