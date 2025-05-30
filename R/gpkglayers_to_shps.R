#' @title export_gpkglayers_to_shapefiles
#' @description This function converts the layers within a gpkg file into shapefiles
#' @param gpkg_path This is a gpkg file with 1 or more layers inside that should be 
#' extracted to shapefiles
#' @param output_dir This is a path where you would like the shapefiles output to
#' @param crs the default is \code{NULL}.  This is the coordinate system  you would 
#' like the output shapefiles to be converted to (e.g. \code{4326}).  If left NULL,
#' no transformation will be attempted
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
export_gpkglayers_to_shapefiles <- function(gpkg_path = NULL, output_dir = NULL, crs = NULL) {
  # List all layer names in the GPKG file
  layer_names <- sf::st_layers(gpkg_path)$name
  
  # Iterate over each layer name
  for (layer in layer_names) {
    # Read the layer
    layer_data <- sf::st_read(gpkg_path, layer = layer)
    if (!is.null(crs))layer_data <- sf::st_transform(layer_data, crs = crs)
    layer_data <- Mar.utils::prepare_shape_fields(layer_data)
    # Create a path for the output shapefile
    output_path <- file.path(output_dir, paste0(layer, ".shp"))
    
    # Write the layer as a shapefile
    sf::st_write(layer_data, output_path, driver = "ESRI Shapefile")
    
    message("Exported layer:", layer, "to", output_path, "\n")
  }
}
