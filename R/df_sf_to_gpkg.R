#' @title df_sf_to_gpkg
#' @description This function adds dataframes (with coordinates) and/or sf spatial objects into a 
#' gpkg file. Dataframes will be converted into sf POINT objects, but if existing sf objects are 
#' submitted, they will be added to the gpkg file.   
#' @param df the default is \code{NULL} a dataframe or sf object to be added to a gpkg file
#' @param lat.field the default is \code{"LATITUDE"}. For dataframe objects, this is the name of the 
#' field holding latitude values (in decimal degrees).  This field can be ignored if the submitted 
#' df is an sf object.
#' @param lon.field the default is \code{"LONGITUDE"}. For dataframe objects, this is the name of 
#' the field holding longitude values (in decimal degrees).  This field can be ignored if the 
#' submitted df is an sf object.
#' @param crs the default is \code{4326}.  This is the coordinate system associated with the input
#' positions, and it assumes WGS84 (i.e. collected via a GPS). This field can be ignored if the 
#' submitted df is an sf object.
#' @param makeSpatial the default is \code{TRUE}. By default, this function will attempt to convert 
#' dataframes to spatial (i.e. sf) objects.  Set this to false if you just want tabular data.
#' @param path this is the path to the gpkg file (e.g. "c:/folder/")
#' @param layerName the default is \code{NULL}.  This will be the name of the layer within the gpkg 
#' file (not the name of the generated gpkg file.) 
#' @param gpkgName the default is \code{"r_layers.gpkg"}.  This is the name of the gpkg file that 
#' will hold the spatial layers.  If this file already exists in \code{path}, the layer will be 
#' added to the existing file.  If the file does not exist, it will be created.
#' @return NULL - writes shapefile to current working directory
#' @family spatial
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
df_sf_to_gpkg<- function(df = NULL, lat.field = "LATITUDE", lon.field = "LONGITUDE", crs=4326, 
                      makeSpatial = T, path = NULL, layerName = NULL, gpkgName = "r_layers.gpkg"){
  if (is.null(path)) path<-getwd()
  if(is.null(layerName)) layerName <- deparse(substitute(df))
  if(!inherits(df,"sf") & makeSpatial) {
    df <- sf::st_as_sf(df, coords = c(lon.field, lat.field), crs = crs)
  }
  if(file.exists(paste0(path, "/", gpkgName))){
    # message("Adding ",layerName," to existing gpkg  ", gpkgName)
    doAppend = FALSE
    overwriteDSN = FALSE
  }else{
    # message("Creating ", gpkgName, " and adding ",layerName)
    doAppend = TRUE
    overwriteDSN = TRUE
  }
  sf::st_write(df, dsn = paste0(path, "/", gpkgName), layerName, append = doAppend, delete.dsn=overwriteDSN, quiet=T)
  return(df)
}
