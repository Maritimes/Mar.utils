#' @title identify_area
#' @description This function takes a df with coordinate fields in decimal
#' degrees, and overlays it a shapefile (agg.poly.shp).  It adds a column to the 
#' df indicating which polygon within the shapefile each point falls within.
#' If no polygon is provided, the df will be assessed against NAFO subdivisions.
#' @param df a dataframe to be analyzed. If left \code{NULL}, a value for 
#' \code{db} should be provided
#' @param lat.field the default is \code{"LATITUDE"}. the name of the field holding latitude values 
#' (in decimal degrees)
#' @param lon.field the default is \code{"LONGITUDE"}.  the name of the field holding longitude 
#' values (in decimal degrees)
#' @param agg.poly.shp default is \code{NULL}.  This is the shapefile that has 
#' polygons that should be checked for sufficient unique values of the 
#' sens.fields.  If NULL, NAFO zones will be used.  Otherwise, a path to any 
#' polygon shapefile can be provided. 
#' @param agg.poly.field default is \code{NULL}.  This identifies the field within 
#' the shapefile provided to agg.poly.shp that should be used to check for 
#' sufficient unique values of the sens.fields.
#' @return a DataFrame with the column \code{agg.poly.field} added
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
identify_area <- function(df = NULL,
                          lat.field = "LATITUDE", 
                          lon.field = "LONGITUDE",
                          agg.poly.shp = NULL,
                          agg.poly.field = NULL){  
  #retain bad coords so we can alert the user
  df_bad = df_qc_spatial(df = df, lat.field = lat.field, lon.field = lon.field, return.bad = TRUE)
  #get the good ones, and convert to spatial
  df = df_qc_spatial(df = df, lat.field = lat.field, lon.field = lon.field, return.bad = FALSE)
  df_sp = df_to_sp(df = df, lat.field = lat.field, lon.field = lon.field, the.crs = "+init=epsg:4326")
  #default to determining the NAFO_BEST column of the NAFO areas if no polygon and/or field is chosen
  if (is.null(agg.poly.shp)){
    agg.poly= Mar.data::NAFOSubunits
  
    agg.poly = sp::spTransform(x=agg.poly,CRSobj = sp::CRS("+init=epsg:4326"))
    if (is.null(agg.poly.field)){
      agg.poly.field = 'NAFO_BEST'
    }
  }else if (is.character(agg.poly.shp)){
    agg.poly <- rgdal::readOGR(dsn = agg.poly.shp, verbose = FALSE)
    if (is.na(sp::proj4string(agg.poly))) {
      cat('\nNo projection found for input shapefile - assuming geographic.')
      sp::proj4string(agg.poly) = sp::CRS("+init=epsg:4326")
    }
    #convert the shape to geographic
    agg.poly <- sp::spTransform(x=agg.poly,CRSobj = sp::CRS("+init=epsg:4326"))
  }
  #append the appropriate value from agg.poly
  df_sp@data = cbind(df_sp@data,sp::over( df_sp, agg.poly , fn = NULL)[agg.poly.field])

  
  #add an error message to the unprocessable fields
  #if numeric, add -999, otherwise a more informative msg
  if (nrow(df_bad)>0){
    if(sapply(df_sp@data[agg.poly.field], is.numeric)[[1]]){
      df_bad[agg.poly.field]<- -999
    }else{
      df_bad[agg.poly.field] <- "Bad coordinate"
    }
    combined = rbind(df_sp@data, df_bad)
  }else{
    combined = df_sp@data
  }
  
  #merge good an bad records together

  return(invisible(combined))
  
}