#' @import data.table  
NULL
# .onAttach <- function(libname, pkgname) {
#   localVer = utils::packageDescription('Mar.utils')$Version
#   packageStartupMessage(paste0("Version: ", localVer))
# }

# .onLoad <- function(libname, pkgname){
#   options(stringsAsFactors = FALSE)
#   updateCheck(gitPkg = 'Maritimes/Mar.utils')
# }
utils::globalVariables(c("AIR_TEMPERATURE", "BAR_PRESSURE", "DATE_TIME", "DEPTH", "FISHSET_ID", "LATITUDE", "LONGITUDE", "NET_TEMPERATURE", "PNTCD_ID", "SETDATE", "SETTIME", "SET_NO", "VESSEL_SPEED", "WATER_TEMPERATURE"))