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
#' @param flag.land default is \code{TRUE}.  This will result in positions determined to be on 
#' land to return the value "<LAND>" in the resulting dataframe.  
#' but would require updating 
#' @return a DataFrame with the column \code{agg.poly.field} added
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
identify_area <- function(df = NULL,
                          lat.field = "LATITUDE", 
                          lon.field = "LONGITUDE",
                          agg.poly.shp = NULL,
                          agg.poly.field = NULL,
                          flag.land = FALSE){  
  
  #flag NA coords for future id
  df_Orig <- df
  df[,"tmp"]<-NA
  handled <- df[F,]
  
  if(nrow(df[is.na(df[,lat.field])|is.na(df[,lon.field]),])>0){
    coordMissing <- df[is.na(df[,lat.field])|is.na(df[,lon.field]),]
    coordMissing$tmp <- "<missing coord>"
    handled=rbind.data.frame(handled, coordMissing)
    df <-df[!is.na(df[,lat.field])& !is.na(df[,lon.field]),]
  }
  
  if(nrow(df[(df[,lat.field] > 90 | df[,lat.field] < -90) |(df[,lon.field] > 180 | df[,lon.field] < -180), ])>0){
    coordImpossible <- df[(df[,lat.field] > 90 | df[,lat.field] < -90) |(df[,lon.field] > 180 | df[,lon.field] < -180), ]
    coordImpossible$tmp <- "<impossible coord>"
    handled=rbind.data.frame(handled, coordImpossible)
    df <-df[!((df[,lat.field] > 90 | df[,lat.field] < -90) |(df[,lon.field] > 180 | df[,lon.field] < -180)), ]
  }
  #default to determining the NAFO_BEST column of the NAFO areas if no polygon and/or field is chosen
  if (is.null(agg.poly.shp)){
    if (flag.land){
      agg.poly= Mar.data::NAFOSubunitsLnd_sf
    }else{
      agg.poly= Mar.data::NAFOSubunits_sf
    }
    
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
    message('No projection found for input - assuming geographic.')
    attributes(agg.poly)$crs <- sf::st_crs(4326)
  }else{
    agg.poly <- sf::st_transform(agg.poly, 4326)
  }
  df_sf <- sf::st_as_sf(x = df,  coords = c(lon.field, lat.field), crs = "EPSG:4326")
  res <- suppressMessages(sf::st_join(df_sf, agg.poly))
  res[which(is.na(res[,agg.poly.field])),agg.poly.field] <- "<on boundary line>"
  res[!is.na(res$tmp),agg.poly.field]<-sf::st_drop_geometry(res[!is.na(res$tmp),"tmp"])
  res$tmp <- res$geometry <- NULL
  res <- res[,c(names(res[names(res) %in% names(df_Orig)]),agg.poly.field)]
  res <- merge(df_Orig, res)
  bbox<-as.vector(sf::st_bbox(agg.poly))
  res[which(res$LATITUDE>bbox[4] |res$LATITUDE < bbox[2]| res$LONGITUDE>bbox[3] |res$LONGITUDE < bbox[1]) ,agg.poly.field] <- "<outside known areas>"
  if(nrow(handled)>0){
    colnames(handled)[colnames(handled)=="tmp"] <- agg.poly.field
    res <- rbind.data.frame(res, handled)
  }
  
  return(invisible(res))
}