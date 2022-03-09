#' @title prepare_shape_fields
#' @description Shapefiles are limited in the naming conventions of the fields - they can only
#' be 10 characters, and must be devoid of special characters that are allowed in R.  This function
#' attempts to ensure that the shortened names that result from calls to writeOGR() can still be 
#' understood and are not simply truncated.
#' @param shape a dataframe or spatial object whose fieldnames are to be processed
#' @return a dataframe - spatial or otherwise
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
prepare_shape_fields <- function(shape){
  updateCnt<-function(df){
    df$CNT<- nchar(df$NEW)
    return(df)
  }
  
  #big ol' df of stuff that we should always replace
  univRepl <- data.frame("VERBOSE" = character(), "SUCCINCT" = character())
  univRepl <- as.data.frame(rbind(as.matrix(univRepl), c('COMBINED','CMB')))
  univRepl <- as.data.frame(rbind(as.matrix(univRepl), c('LICENSE','LIC')))
  univRepl <- as.data.frame(rbind(as.matrix(univRepl), c('VESSEL','VES')))
  univRepl <- as.data.frame(rbind(as.matrix(univRepl), c('TEMPERATURE','TMP')))
  univRepl <- as.data.frame(rbind(as.matrix(univRepl), c('BOTTOM','BOT')))
  univRepl <- as.data.frame(rbind(as.matrix(univRepl), c('SURFACE','SUR')))
  univRepl <- as.data.frame(rbind(as.matrix(univRepl), c('SALINITY','SAL')))
  univRepl <- as.data.frame(rbind(as.matrix(univRepl), c('WEIGHT','WT')))
  univRepl <- as.data.frame(rbind(as.matrix(univRepl), c('DEPTH','DEP')))
  univRepl <- as.data.frame(rbind(as.matrix(univRepl), c('EST_KEPT_WT','KPT')))
  univRepl <- as.data.frame(rbind(as.matrix(univRepl), c('KEPT_WT','KPT')))
  univRepl <- as.data.frame(rbind(as.matrix(univRepl), c('EST_DISCARD_WT','DSC')))
  univRepl <- as.data.frame(rbind(as.matrix(univRepl), c('DISCARD_WT','DSC')))
  univRepl <- as.data.frame(rbind(as.matrix(univRepl), c('EST_NUM_CAUGHT','NUM')))
  univRepl <- as.data.frame(rbind(as.matrix(univRepl), c('MEAN','MN')))
  univRepl <- as.data.frame(rbind(as.matrix(univRepl), c('COUNT','CT')))
  univRepl <- as.data.frame(rbind(as.matrix(univRepl), c('CNT','CT')))
  univRepl <- as.data.frame(rbind(as.matrix(univRepl), c('SUM','SU')))
  univRepl <- as.data.frame(rbind(as.matrix(univRepl), c('SPEC','SP')))
  univRepl <- as.data.frame(rbind(as.matrix(univRepl), c('GEAR','GR')))
  univRepl <- as.data.frame(rbind(as.matrix(univRepl), c('LANDING','LND')))
  univRepl <- as.data.frame(rbind(as.matrix(univRepl), c('BOARD','BRD')))
  univRepl <- as.data.frame(rbind(as.matrix(univRepl), c('TRIP','TRP')))
  univRepl <- as.data.frame(rbind(as.matrix(univRepl), c('YEAR','YR')))
  univRepl <- as.data.frame(rbind(as.matrix(univRepl), c('MONTH','MO')))
  univRepl <- as.data.frame(rbind(as.matrix(univRepl), c('DATE','DT')))
  univRepl <- as.data.frame(rbind(as.matrix(univRepl), c('SOUGHT','SGT')))
  univRepl <- as.data.frame(rbind(as.matrix(univRepl), c('CAUGHT','CGT')))
  univRepl <- as.data.frame(rbind(as.matrix(univRepl), c('DAY','DA')))
  univRepl <- as.data.frame(rbind(as.matrix(univRepl), c('SOURCE','SRC')))
  
  #grab all of the original names, capture their order, length, and prepare a place for their replacements
  allNames = data.frame(ID = seq(length(names(shape))), ORIG = names(shape), CNT = nchar(names(shape)), NEW = names(shape))

  #replace periods with underscores
  allNames$NEW = gsub('\\.','_', allNames$NEW)
  
  #do the universal replacements
  for (r in 1:nrow(univRepl)){
    allNames[grepl(x=allNames$NEW, ignore.case = T, pattern=univRepl[r,"VERBOSE"]),"NEW"]<-gsub(x = allNames[grepl(x = allNames$NEW,ignore.case = T,pattern = univRepl[r,"VERBOSE"]),"NEW"], pattern = univRepl[r,"VERBOSE"],replacement = univRepl[r,"SUCCINCT"])
  }
  
  allNames <-updateCnt(allNames)
  #do succesively more harsh actions to get char down to 10
  if (max(range(allNames$CNT))>10){
    allNames[allNames$CNT>10,"NEW"]<-gsub("_","", allNames[allNames$CNT>10,"NEW"])
    allNames <-updateCnt(allNames)
  }

  if (max(range(allNames$CNT))>10){
    allNames[grepl(pattern = "EST", x = allNames$NEW),"NEW"] <- gsub(pattern = "EST", replacement = "", x= allNames[grepl(pattern = "EST", x = allNames$NEW),"NEW"])
    allNames <-updateCnt(allNames)
  }
  
  if (max(range(allNames$CNT))>10){
    # allNames[allNames$CNT>10,"NEW"]<-substr(allNames[allNames$CNT>10,"NEW"], 1, 10) 
    message("\n!!Despite efforts to shorten them, rgdal will impose some aggressive measures to reduce the field names to the 10 chars allowed by ArcGIS","\n")
    #cat(allNames$NEW)
    #allNames <-updateCnt(allNames)
  }
  names(shape) = allNames[with(allNames,order(ID)),"NEW"]
  return(shape)
}
