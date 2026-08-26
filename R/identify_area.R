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
#' @family spatial
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
identify_area <- function (df = NULL, 
                           lat.field = "LATITUDE", 
                           lon.field = "LONGITUDE", 
                           agg.poly.shp = NULL, 
                           agg.poly.field = NULL, 
                           flag.land = FALSE) 
{
  df$ID_ <- seq.int(nrow(df))
  df_Orig <- df
  handled <- df[F, ]
  badNames <- names(df)[!names(df) %in% c(lat.field, lon.field, "ID_", "tmp")]
  
  if(inherits(df, "sf")){
    coord_df <-as.data.frame(st_coordinates(df))
    lat_vals <- coord_df$Y  # Now works
    lon_vals <- coord_df$X  # Now works
  }else{
    lat_vals <- df[, lat.field]
    lon_vals <- df[, lon.field]
  }
  
  df$tmp <- NA
  
  if (nrow(df[is.na(lat_vals) | is.na(lon_vals),]) > 0) {
    coordMissing <- df[is.na(lat_vals) | is.na(lon_vals), ]
    coordMissing$tmp <- "<missing coord>"
    handled=rbind.data.frame(handled, coordMissing)
    df <-df[!is.na(lat_vals) & !is.na(lon_vals), ]
  }
  if (nrow(df[(lat_vals > 90 | lat_vals < -90) | (lon_vals > 180 | lon_vals < -180), ]) > 0) {
    coordImpossible <- df[(lat_vals > 90 | lat_vals < -90) | (lon_vals > 180 | lon_vals < -180), ]
    coordImpossible$tmp <- "<impossible coord>"
    handled=rbind.data.frame(handled, coordImpossible)
    df <-df[!((lat_vals > 90 | lat_vals < -90) | (lon_vals > 180 | lon_vals < -180)), ]
  }
  if (is.null(agg.poly.shp)){
    if (flag.land) {
      agg.poly = Mar.data::NAFOSubunitsLnd_sf
    } else {
      agg.poly = Mar.data::NAFOSubunits_sf
    }
    if (is.null(agg.poly.field)){
      agg.poly.field = 'NAFO'
    }
  } else if (is.character(agg.poly.shp)) {
    agg.poly <- sf::st_read(dsn = agg.poly.shp, quiet = T)
  } else if ("SpatialPolygons" %in% class(agg.poly.shp) || "SpatialPolygonsDataFrame" %in%  class(agg.poly.shp)) {
    agg.poly = sf::st_as_sf(agg.poly.shp)
  } else if ("sf" %in% class(agg.poly.shp)) {
    agg.poly = agg.poly.shp
  }
  if (agg.poly.field %in% names(df)) {
    names(agg.poly)[names(agg.poly) == agg.poly.field] <- paste0(agg.poly.field, "_1")
    agg.poly.field <- paste0(agg.poly.field, "_1")
    warning("The value for agg.poly.field already exists in df.  The output will append '_1' to the field coming from agg_poly_shp")
  }
  if (is.na(sf::st_crs(agg.poly))) {
    message("No projection found for input - assuming geographic.")
    attributes(agg.poly)$crs <- sf::st_crs(4326)
  } else {
    agg.poly <- sf::st_transform(agg.poly, 4326)
  }
  df <-df  |>  
    mutate(lat_orig = .data[[lat.field]], 
           lon_orig = .data[[lon.field]])
  
  df_sf <- sf::st_as_sf(x = df, coords = c(lon.field, lat.field), crs = "EPSG:4326")
  sink <- utils::capture.output(sf::sf_use_s2(FALSE))
  res <- suppressMessages(sf::st_join(df_sf, agg.poly))
  sink <- utils::capture.output(sf::sf_use_s2(TRUE))
  res[which(is.na(res[, agg.poly.field])), agg.poly.field] <- "<outside known areas>"
  res[!is.na(res$tmp), agg.poly.field] <- sf::st_drop_geometry(res[!is.na(res$tmp), agg.poly.field])
  res$tmp <- res$geometry <- NULL
  res <- res[, c(names(res[names(res) %in% names(df_Orig)]), agg.poly.field)]
  res <- res[, !names(res) %in% badNames]
  res <- merge(df_Orig, res, by = "ID_")
  bbox <- st_bbox(agg.poly)
  res[which(res$lat_orig > bbox[4] | res$lat_orig < bbox[2] | 
              res$lon_orig > bbox[3] | res$lon_orig < bbox[1]), 
      agg.poly.field] <- "<outside known areas>"
  # bbox <- as.vector(sf::st_bbox(agg.poly))
  # res[which(res[, lat.field] > bbox[4] | res[, lat.field] < bbox[2] | res[, lon.field] > bbox[3] | res[, lon.field] < bbox[1]), agg.poly.field] <- "<outside known areas>"
  if (nrow(handled) > 0) {
    colnames(handled)[colnames(handled) == "tmp"] <- agg.poly.field
    res <- rbind.data.frame(res, handled)
  }
  res$ID_ <- NULL
  return(invisible(res))
}