#' @title DDMMSS_to_DD
#' @description This is a utility that will replace the DDMMSS columns of a dataframe with the same
#' coordinates in decimal degrees.
#' @param df a dataframe to be analyzed. If left \code{NULL}, a value for \code{db} should be provided
#' @param lat.field the default is \code{"LATITUDE"}. This is the name of the 
#' field holding latitude values (in decimal degrees)
#' @param lon.field the default is \code{"LONGITUDE"}.  This is the name of the 
#' field holding longitudevalues (in decimal degrees)
#' @param WestHemisphere the default is \code{TRUE}.  It true, this ensure that all returned 
#' Longitudes are negative.  If false, the original sign is retained.
#' @return a data.frame
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
DDMMSS_to_DD <- function(df = NULL, lat.field = "LATITUDE", lon.field="LONGITUDE", WestHemisphere = T){
  deprecate_warn("2023.12.2", "DDMMSS_to_DD()", "DDMMx_to_DD()")
  df[,lat.field][!is.na(df[,lat.field])] = round(as.numeric(substr(df[,lat.field][!is.na(df[,lat.field])], 1, 2)) + 
                                                 as.numeric(substr(df[,lat.field][!is.na(df[,lat.field])], 3, 4))/60 + 
                                                 as.numeric(substr(df[,lat.field][!is.na(df[,lat.field])], 5, 6))/3600,7)
  
  df[,lon.field][!is.na(df[,lon.field])] = round(as.numeric(substr(df[,lon.field][!is.na(df[,lon.field])], 1, 2)) + 
                                                 as.numeric(substr(df[,lon.field][!is.na(df[,lon.field])], 3, 4))/60 + 
                                                     as.numeric(substr(df[,lon.field][!is.na(df[,lon.field])], 5, 6))/3600 ,7)
  if (WestHemisphere) df[,lon.field][!is.na(df[,lon.field])] = -1 * abs(df[,lon.field][!is.na(df[,lon.field])])
  return(df)
}

