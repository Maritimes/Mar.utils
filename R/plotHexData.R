#' @title plot_hex_data
#' @description This function plots the gridded data from assess_privacy
#' @param data_sf default is \code{NULL}.  an sf version of the Grid2Min object from assess_privacy.  
#' @param plotfld default is \code{NULL}.  This is a single field within the data object that you 
#' want to plot. Leaving it NULL results in a popup box of selectable fields. 
#' @param extent  default is \code{"data"}.  This controls the extent of the output plot.  It can be one 
#' of \code{"data"}, \code{"layer"}, or a vector of coordinates corresponding with 
#' xmin, xmax, ymin, ymax (e.g. \code{bbox = c(-65,-63,43,45)}.
#' \itemize{
##'  \item \code{"data"} - results in the plot being restricted to an area where values of \code{plotfld} are 
#' non-zero
#'   \item \code{"layer"} - results in the plot showing the full extent of the gridded area
#'   \item \code{c(xmin, xmax, ymin, ymax)} - results in a plot restricted to the specified area
#'   }
#' @return an ggplot2 plot
#' @note if no  PID or POS is provided, it is assumed that the provided positions
#' are for a single polygon and the position are in the correct order.
#' @family privacy
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
plot_hex_data <- function(data_sf = NULL, plotfld=NULL, extent ="data"){
  
  if (!class(data_sf)[1]=="sf"){
    if(class(data_sf)=="list" && class(data_sf[[1]])=="SpatialPolygonsDataFrame" && names(data_sf[1])=="Grid2Min"){
      message("converting your sp into an sf object via \n\tMar.utils::convert2poly(input = data_sf[[1]], out = 'sf')\nIt would be faster to do this prior to running this script")
      data_sf <- Mar.utils::convert2poly(input = data_sf[[1]], out = "sf" )
    }else{
      stop("Please convert your input into an sf object.")
    }
  }
  # establish which are data vs system fields
  flds_sf <- c("ORD_gr",  "HEXID", "geometry")
  flds_choice <- colnames(data_sf)
  flds_choice = flds_choice[!(flds_choice %in% flds_sf)]
  
  # if plotfld provided, use it.  Otherwise, prompt for one 
  # (this could be broken into two prompts:
  # 1 for species (w spp lookup table for names), and
  # 2 for analytic (sums, counts, numbers, est_combined_wts, etc))
  if(!is.null(plotfld)){ 
    if (plotfld %in% flds_choice){
      fld <-plotfld
    }else{
      stop("selected field in not present in data.  Please choose a different one")
    }
  }else{
    fld <- NA
    while (is.na(fld)){
      fld <- select.list(choices=flds_choice,multiple = F, graphics = T)
    }
  }
  
  # got field, remove unused fields
  thisData <- data_sf[,c(flds_sf,fld)]
  # set zero values to NA, so they plot transparently
  thisData[[fld]][thisData[[fld]]==0] <- NA
  
  # determine desired map extent, using extent parameter
  if (class(extent) == "numeric"){
    thisbbox <- as.list(setNames(extent , c("xmin", "xmax","ymin","ymax")))
  } else if (extent == "data"){ 
    # default is extent on non-zero data
    thisbbox <- thisData[!is.na(thisData[[fld]]),]
    thisbbox <-  sf::st_bbox(thisbbox)
  }else if (any(extent %in% "layer")){
    # 'layer' is extent of entire sf object
    thisbbox <- sf::st_bbox(data_sf)
  }

   p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = thisData, ggplot2::aes(colour="#E6D5DBDB",fill = get(fld))) +
    ggplot2::scale_fill_viridis_c(direction = -1, na.value = NA, name =fld) +
    ggplot2::scale_colour_identity() +
    ggplot2::coord_sf(xlim = c(thisbbox$xmin, thisbbox$xmax), ylim = c(thisbbox$ymin, thisbbox$ymax))
  return(p)
}
