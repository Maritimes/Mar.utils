#' @title clip_by_poly
#' @description This function takes a dataframe and a polygon, and clips the dataframe using the
#' the extent of the polygon.  The polygon can be buffered as required to select nearby 
#' data as well.  
#' @param df default is \code{NULL}.  This is the dataframe/sf object that will be altered.  It 
#' can be a dataframe,  sp::spatial* object or an sf object (point, line or polygon).
#' @param clip.poly default is \code{NULL}. This is either an sf polygon, an sp:spatialPolygons
#' object, or the full path to a shapefile (including the '.shp' extension).  This is the polygon 
#' that will be used to clip the \code{input} object.
#' @param lat.field the default is \code{"LATITUDE"}. This is the name of the field holding latitude 
#' values (in decimal degrees).  This is only necessary if \code{input} is a non spatial df.
#' @param lon.field the default is \code{"LONGITUDE"}.  This is the name of the field holding 
#' longitude values (in decimal degrees).  This is only necessary if \code{input} is a non spatial 
#' df.
#' @param buffer.m default is \code{NULL}. This is the distance in meters to buffer the border of 
#' \code{clip.poly}
#' @param invert default is \code{FALSE}.  By default, \code{input} will be clipped such that 
#' \itemize{
#' \item \code{points} only those points within the \code{clip.poly} are returned 
#' \item \code{lines} only those lines that cross or are contained by the \code{clip.poly} are 
#' returned 
#' \item \code{polygons} only those polygons that cross or are contained by the \code{clip.poly} are 
#' returned 
#' }
#' However, if \code{invert} is set to \code{TRUE}, than any points/lines/polygons that intersect/
#' are contained by the \code{clip.poly} will be removed.
#' @return sf object
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#' @note If the input polygon has no projection assigned, it will be assumed to be in Geographic, 
#' WGS84. FYI, during buffering, the polygon is briefly converted to UTMZone20 since the use of 
#' distances requires projecting the data.
clip_by_poly <- function(df=NULL,
                                 lat.field = "LATITUDE", 
                                 lon.field = "LONGITUDE", 
                                 clip.poly = NULL,
                                 buffer.m = NULL,
                                 invert=F){
  orig_crs_df <- NA 
  if (!inherits(df,"sf")){
    if (inherits(df,"data.frame")){
      df=df_qc_spatial(df)
      orig_crs_df <- 4326
      df.sf <- sf::st_as_sf(df, coords = c(lon.field, lat.field), crs = 4326, agr = "constant")
    } else if(inherits(df.sf,"SpatialPointsDataFrame")|inherits(df.sf,"SpatialLinesDataFrame")|inherits(df.sf,"SpatialPolygonsDataFrame")){
      orig_crs_df <- sp::proj4string(df.sf)
      df.sf <- sf::st_as_sf(df.sf)
    }
  }else{
    df.sf<- df
    orig_crs_df <- sf::st_crs(df.sf)
  }
  
  if (inherits(clip.poly,"character")){
    clip.poly_this     <- sf::st_read(clip.poly, quiet=T)
  } else if(inherits(clip.poly,"SpatialPolygonsDataFrame")){
    clip.poly_this <- sf::st_as_sf(clip.poly)
  }
  if (is.na(sf::st_crs(clip.poly_this))){
    cat('\nNo projection found for input shapefile - assuming geographic.')
    sf::st_crs(clip.poly_this) <- 4326
  }
  
  #set both poly and df to same projection (UTM ZONE 20N)
  #this will also ensure buffered units are meters
  clip.poly_this <- sf::st_transform(clip.poly_this, crs = 2220)
  df.sf <- sf::st_transform(df.sf, crs = 2220)
  
  if (!is.null(buffer.m)){
    clip.poly_this <- sf::st_buffer( clip.poly_this, buffer.m)
  }
  
  if (NROW(df.sf[clip.poly_this, ]) ==0) {
    stop("\nNo data lies inside this polygon, aborting clip.")
  }
  
  if (!invert){
    df.sf_subset <- df.sf[clip.poly_this, ] 
  }else{

    # df.sf_subset<- sf::st_intersects(df.sf, clip.poly_this, sparse = T) %>%   
    #   as.numeric() 
    df.sf_subset<- sf::st_intersects(df.sf, clip.poly_this, sparse = T)
    is.na(df.sf_subset) <- lengths(df.sf_subset) == 0
    df.sf_subset <- suppressWarnings(as.numeric(unlist(lapply(df.sf_subset, paste0, collapse = ''))))
    
    df.sf_subset<-df.sf[is.na(df.sf_subset),] 
  }
  df.sf_subset <- sf::st_transform(df.sf_subset, crs = orig_crs_df)
  
  if (all(sf::st_geometry_type(df.sf_subset)=="POINT")){
    df.sf_subset<- cbind(df.sf_subset, sf::st_coordinates(df.sf_subset$geometry))
    colnames(df.sf_subset)[colnames(df.sf_subset)=="X"] <- lon.field
    colnames(df.sf_subset)[colnames(df.sf_subset)=="Y"] <- lat.field
  }

  return(df.sf_subset)
}