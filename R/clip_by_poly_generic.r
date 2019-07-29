#' @title clip_by_poly_generic
#' @description This function takes a dataframe and a polygon, and clips
#' the data to the extent of the polygon.  The polygon can be buffered as required to select nearby 
#' data as well.
#' @param df default is \code{NULL}.  This is the dataframe to be clipped.
#' @param lat.field the default is \code{"LATITUDE"}. the name of the field holding latitude values 
#' (in decimal degrees)
#' @param lon.field the default is \code{"LONGITUDE"}.  the name of the field holding longitude 
#' values (in decimal degrees)
#' @param clip.poly default is \code{NULL}. This is the full path to a shapefile 
#' that the data will be clipped by (including the '.shp' extension).
#' @param buffer.m default is \code{NULL}. This is the distance in meters to buffer the border of 
#' \code{clip.poly}
#' @param return.spatial default is \code{FALSE}. If this is TRUE, a 
#' SpatialPointsDataFrame will be returned. Otherwise it will return a df.
#' @param env This the the environment you want this function to work in.  The 
#' default value is \code{.GlobalEnv}.
#' @return spatialPointsDataFrame
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#' @note If the input polygon has no projection assigned, it will be assumed to be in Geographic, 
#' WGS84. FYI, during buffering, the polygon is briefly converted to UTMZone20, and back again, 
#' since the use of distances requires projecting the data.
clip_by_poly_generic <- function(df=NULL,
                         lat.field = "LATITUDE", 
                         lon.field = "LONGITUDE", 
                         clip.poly = NULL,
                         buffer.m = NULL,
                         return.spatial = FALSE,
                         env=.GlobalEnv){
  df=df_qc_spatial(df)
  df.sp = sp::SpatialPointsDataFrame(
    coords = df[, c(lon.field, lat.field)],
    data = df,
    proj4string = sp::CRS('+init=epsg:4326')
  )
  if (class(clip.poly)=="character"){
    #extract the full path and name of the shapefile 
    ogrPath = dirname(clip.poly)
    ogrLayer = sub('\\.shp$', '', basename(clip.poly))
    clip.poly_this <- rgdal::readOGR(dsn = ogrPath, layer = ogrLayer, verbose = FALSE)
  }else if(class(clip.poly)=="SpatialPolygonsDataFrame"){
    clip.poly_this = clip.poly
  }

  if (is.na(sp::proj4string(clip.poly_this))) {
    cat('\nNo projection found for input shapefile - assuming geographic.')
    sp::proj4string(clip.poly_this) = sp::CRS("+init=epsg:4326")
  } else if (sp::proj4string(clip.poly_this)!="+init=epsg:4326") {
    clip.poly_this = sp::spTransform(clip.poly_this, sp::CRS('+init=epsg:4326'))
  }
  
  if (!is.null(buffer.m)){
    #if a buffer is specified, convert poly to UTM20N, apply buffer, and convert back
    clip.poly_this = sp::spTransform(clip.poly_this, sp::CRS("+init=epsg:2220"))
    clip.poly_this = rgeos::gBuffer(clip.poly_this, width=buffer.m)
    clip.poly_this = sp::spTransform(clip.poly_this, sp::CRS('+init=epsg:4326'))
  }
  if (NROW(df.sp[clip.poly_this, ]) ==0) {
    stop("\nNo data lies inside this polygon, aborting clip.")
  }
  df.sp_subset <- df.sp[clip.poly_this, ] 
  
  if (!return.spatial){
    df.sp_subset = df.sp_subset@data
  }

  return(df.sp_subset)

}