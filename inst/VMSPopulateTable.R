#' @title VMSPopulateTable
#' @description This function takes a polygon shapefile and a connection to an 
#' Oracle schema (with access to MFD_OBFMI.VMS_ALL) and generates a new table 
#' within that schema populated by VMS data that falls within any of the 
#' shapefile's polygons.
#'   
#' A buffer can be specified (in hours) that gives context to the vessel tracks, 
#' so that activities such as transitting can be distinguished from other 
#' activities.  
#' 
#' @param usepkg The default value is \code{'roracle'}, but 
#' \code{'rodbc'} works as well. This describes the R package you use to connect 
#' to Oracle.  
#' @param tblName The default value is \code{NULL}.  This is the name of the 
#' table that will be created/appended in the Oracle schema.
#' @param hrBuffer default is \code{6}.  This is how much context you would
#' like to capture around the positions that actually lie within your 
#' polygons (in hours).  For example, if a vessel has 1 ping within a polygon, 
#' the default is to grab the other VMS data for that vessel within 6 hours on 
#' either side of the event.    
#' The VMS system does not inherently have any way to describe a "trip" since it 
#' is just GPS timestamped positions and vessel identifiers.  The buffer is also 
#' used to identify breaks in the VMS data so that positions for a vessel on one 
#' day are not accidentally linked to positions for that same vessel several 
#' days later. If a vessel passes through a polygon twice, but the length of 
#' time between the two events is longer than hrBuffer, the positions for each 
#' event will be identified with different "SEGMIDs".
#' @param shp default is \code{NULL}.  This is the shapefile that has 
#' polygons for which you want to catch VMS data within.
#' @param shp.field default is \code{NULL}.  This identifies the field within 
#' the shapefile which you would like appended to each VMS position that lands 
#' within the polygon.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom Mar.utils make_oracle_cxn
#' @importFrom Mar.utils identifyArea
#' @importFrom dplyr anti_join
VMSPopulateTable <- function(usepkg = 'roracle', tblName = "VMSTEMP", 
                             hrBuffer = 6, 
                             shp = NULL, shp.field=NULL,
                             latRange =c(30,60), longRange = c(-75,-45) #, startDate = "2018-03-01"
                             ){
  
  # setup functions ####
  Sys.setenv(TZ="UTC")
  Sys.setenv(ORA_SDTZ = "UTC")
  VMSDataStart = as.POSIXct('1999-01-01', origin = "1970-01-01")
  # lastProcessedDate =  as.POSIXct(defaultStartDate, origin = "1970-01-01")
  startTime=Sys.time()
  run_ts = as.integer(startTime)
  runTime=format(startTime,"%Y-%m-%d %H:%M:%S")
  overwriteAction = FALSE
  appendAction = TRUE
  assign("oracle_cxn", Mar.utils::make_oracle_cxn(usepkg), envir = .GlobalEnv )
  runDur = 0
  overwriteAction = TRUE
  appendAction = FALSE
  ####   
  
 #STEP 1) Get the new data for the current time period (startDate - buffer)
  mostRecentAreaRec = VMSGetExistingRecs(thistblName = tblName, recTime = "newest", recInArea = TRUE)

  if (class(mostRecentAreaRec)=="character"){
    newVMSRecs <- tryCatch(
      {
        VMSGetNewRecs(dateStart = VMSDataStart, hrBuffer = hrBuffer)
      },error=function(cond1){
        print(cond1)
        if (grep("does not exist",cond1$message)>0){
          return("failed")
        }
      }
    )
  }else{    
    overwriteAction = FALSE
    appendAction = TRUE
    #STEP 2) Get the existing data for the current time period (startDate - buffer) 
    #have time overlap, so existing records within time window and check them
    timeWindowStart = as.POSIXct(mostRecentAreaRec$POSITION_UTC_DATE -(hrBuffer *3600), origin = '1970-01-01')
   
    allRecentRecs = VMSGetExistingRecs(thistblName = tblName, recStartDateSQL = timeWindowStart, recInArea = FALSE)
    
    newVMSRecs <- tryCatch(
      {
        VMSGetNewRecs(timeStart = timeWindowStart, hrBuffer = hrBuffer)
      },error=function(cond2){
        if (grep("does not exist",cond2$message)>0){
          return("failed")
        }
      }
    )
    #find the new vms records that overlap our recent stored records
    newVMSRecs = VMSIntegrateNewRecs(newVMSRecs, allRecentRecs, shp.field)
    #the line below should prevent dups from being added
    newVMSRecs = anti_join(newVMSRecs,allRecentRecs, by = c("VR_NUMBER", "LATITUDE", "LONGITUDE", "POSITION_UTC_DATE","SPEED_KNOTS","UPDATE_DATE","FULL_DESCR"))
    #QC
    newVMSRecs= newVMSRecs[(newVMSRecs$LATITUDE > min(latRange) & newVMSRecs$LATITUDE < max(latRange)) &
                             (newVMSRecs$LONGITUDE > min(longRange) & newVMSRecs$LONGITUDE < max(longRange)),]
  }

  if (nrow(newVMSRecs)>0){
      cutAndPrint <- tryCatch(
      {
        #write the data
        newVMSRecs$VMS_SAVED = as.POSIXct(startTime, origin = "1970-01-01")
        ROracle::dbWriteTable(oracle_cxn$channel, tblName, newVMSRecs, row.names = FALSE, overwrite = overwriteAction, append = appendAction)
        print(paste0("added ", length(newVMSRecs$VR_NUMBER)," relevant records. Most recent entry in database is: ",as.POSIXct(max(newVMSRecs$POSITION_UTC_DATE), origin="1970-01-01")))
      },
      error=function(cond){
        print(paste("Data not written:  ",cond))
      }
    )
  } else{
    if (class(mostRecentAreaRec)=="character"){
      print(paste0("No new records found."))
    }else{
      print(paste0("No new records.  Most recent entry in database is: ",as.POSIXct(mostRecentAreaRec$POSITION_UTC_DATE, origin="1970-01-01")))
    }
  }
  return(invisible(newVMSRecs))
}
