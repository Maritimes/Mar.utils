#' @title identify_area
#' @description This function takes a dataframe (with coordinate fields in decimal
#' degrees), (optionally) a spatial object (either a shapefile, an
#' sp::SpatialPolygonsDataframe, or an sf::polygon), and a field name from the 
#' polygon object.  It then overlays the df with the polygon, and determines which
#' discrete polygon contains each point from the df, and appends a new field 
#' containing this value.  
#' If no polygon is provided, the df will be assessed against NAFO subdivisions.
#' @param df a dataframe to be analyzed. 
#' @param lat.field the default is \code{"LATITUDE"}. the name of the field holding latitude values 
#' (in decimal degrees)
#' @param lon.field the default is \code{"LONGITUDE"}.  the name of the field holding longitude 
#' values (in decimal degrees)
#' @param agg.poly.shp default is \code{NULL}.  This is either the path to the 
#' *.shp file of a shapefile, an sf spatial object; or an sp spatialpolygonsdataframe.
#'  If NULL, NAFO zones will be used.
#' @param agg.poly.field default is \code{NULL}.  This identifies the field within 
#' \code{agg.poly.shp} that contains the values that should be appended to the 
#' input dataframe. If NULL, "NAFO_BEST", will be used, which is the finest
#' resolution NAFO subdivision.
#' @return a DataFrame with the column \code{agg.poly.field} added
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
identify_area <- function(df = NULL,
                          lat.field = "LATITUDE", 
                          lon.field = "LONGITUDE",
                          agg.poly.shp = NULL,
                          agg.poly.field = NULL){  
  #flag NA coords for future id
  df_Orig <- df
  df[,"tmp"]<-NA
  df[is.na(df[,lat.field])|is.na(df[,lon.field]),"tmp" ]<- "Bad coordinate"
  df[is.na(df[,lat.field]),lat.field] <- -89.89
  df[is.na(df[,lon.field]),lon.field] <- -89.89
  df_sf <- sf::st_as_sf(x = df,  coords = c(lon.field, lat.field), crs = "+init=epsg:4326")
  
  #default to determining the NAFO_BEST column of the NAFO areas if no polygon and/or field is chosen
  if (is.null(agg.poly.shp)){
    agg.poly= Mar.data::NAFOSubunits_sf
    if (is.null(agg.poly.field)){
      agg.poly.field = 'NAFO_BEST'
    }
  }else if (is.character(agg.poly.shp)){
    agg.poly <- sf::st_read(dsn = agg.poly.shp)
  }else if (class(agg.poly.shp)=="SpatialPolygons" || class(agg.poly.shp)=="SpatialPolygonsDataFrame"){
    agg.poly = methods::as(agg.poly.shp, "sf")
  }else if (any(class(agg.poly.shp)=="sf")){
    agg.poly = agg.poly.shp
  }
  if (is.na(sf::st_crs(agg.poly))) {
    cat('\nNo projection found for input - assuming geographic.')
    attributes(agg.poly)$crs <- sf::st_crs(4326)
  }else{
    agg.poly <- sf::st_transform(agg.poly, 4326)
  }
  
  agg.poly <- suppressMessages(sf::st_join(df_sf, agg.poly))
  agg.poly[is.na(agg.poly[,agg.poly.field]),agg.poly.field] <-"Outside of Defined Areas" 

  agg.poly[!is.na(agg.poly$tmp),agg.poly.field]<-sf::st_drop_geometry(agg.poly[!is.na(agg.poly$tmp),"tmp"])
  agg.poly$tmp <- agg.poly$geometry <- NULL
  agg.poly <-  merge(agg.poly, df_Orig[,!names(df_Orig) %in% c(agg.poly.field)])
  return(invisible(agg.poly))
}