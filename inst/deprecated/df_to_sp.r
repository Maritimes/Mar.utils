#' @title df_to_sp
#' @description This function takes a dataframe and converts in to a spatialpointsdataframe
#' @param df default is \code{NULL}. This is the dataframe you want to spatialize.
#' @param lat.field the default is \code{"LATITUDE"}. the name of the field holding latitude values
#' (in decimal degrees)
#' @param lon.field the default is \code{"LONGITUDE"}.  the name of the field holding longitude
#' values (in decimal degrees)
#' @param the.crs the default is \code{EPSG:4326}.  This is the projection 
#' of the incoming data.  This is NOT what you want the projection to be.  
#' Assigning a projection, and reprojecting are different.
#' @param autoQC the default is \code{TRUE}.  This runs df_qc_spatial() 
#' automatically, ensuring that all coordinates are in the Northern hemsisphere
#' (i.e. Latitude between 0 and 90N) and the western hemisphere (i.e. Longitude
#' between -180 and 0W) 
#' @return spatialpointsdataframe
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
df_to_sp <- function(df = NULL, lat.field="LATITUDE", lon.field="LONGITUDE", the.crs = "EPSG:4326", autoQC = TRUE){
  df = df_qc_spatial(df = df, lat.field = lat.field, lon.field = lon.field, return.bad = FALSE, autoQC=autoQC)
  df.sp = sp::SpatialPointsDataFrame(
    coords = df[, c(lon.field, lat.field)],
    data = df,
    proj4string = sp::CRS(SRS_string=the.crs)
  )
  return(df.sp)
}
