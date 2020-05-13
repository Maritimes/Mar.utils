#' @title df_to_shp
#' @description Super quick and dirty function for turning a dataframe with coordinates into a 
#' shapefile.  Only parameters are the Lat and Lon fields. Assumes WGS 84.
#' @param df the default is \code{NULL} a dataframe or spatial object whose fieldnames are to be processed
#' @param lat.field the default is \code{"LATITUDE"}. the name of the field holding latitude values 
#' (in decimal degrees)
#' @param lon.field the default is \code{"LONGITUDE"}.  the name of the field holding longitude 
#' values (in decimal degrees)
#' @param filename the default is \code{NULL}.  This is the name of the output shapefile.  If \code{NULL},
#' the shape will be called "newShape".
#' @return NULL - writes shapefile to current working directory
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#' @note This function is set to overwrite existing shapefiles with the same name. 
df_to_shp <- function(df = NULL, lat.field = "LATITUDE", lon.field = "LONGITUDE", filename = NULL){
  df= Mar.utils::df_to_sp(df=df, lat.field = lat.field, lon.field = lon.field)
  df= prepare_shape_fields(shape = df)
  nm = ifelse(is.null(filename),"newShape", filename)
  rgdal::writeOGR(df, dsn= getwd(),layer = nm, driver="ESRI Shapefile",overwrite_layer = TRUE)
  cat("\nFile written to ",getwd(),"/",nm,".shp\n")
  return(invisible(NULL))
}