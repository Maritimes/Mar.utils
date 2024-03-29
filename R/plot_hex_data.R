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
#' @param hideEmptyCells default is \code{TRUE}.  By default, this parameter results in the plot 
#' only showing grid cells for which data exists (i.e. non-zero).  This offers a dramatic performance 
#' improvement.  If the entire mesh of predominantly empty cells must be shown, set this to FALSE.
#' @return an ggplot2 plot
#' @note if no  PID or POS is provided, it is assumed that the provided positions
#' are for a single polygon and the position are in the correct order.
#' @family privacy
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
plot_hex_data <- function(data_sf = NULL, plotfld=NULL, extent ="data", hideEmptyCells = TRUE){
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
      fld <- utils::select.list(choices=flds_choice,multiple = F, graphics = T)
    }
  }
  
  # got field, remove unused fields
  thisData <- data_sf[,c(flds_sf,fld)]
  
  if (hideEmptyCells){
    thisData <- thisData[thisData[[fld]] !=0, ]
  }else{
    # set zero values to NA, so they plot transparently
    thisData[[fld]][thisData[[fld]]==0] <- NA
  }
  
  # determine desired map extent, using extent parameter
  if (inherits(extent, "numeric")){
    thisbbox <- as.list(stats::setNames(extent , c("xmin", "xmax","ymin","ymax")))
  } else if (extent == "data"){ 
    # default is extent on non-zero data
    thisbbox <- thisData[!is.na(thisData[[fld]]),]
    thisbbox <-  sf::st_bbox(thisbbox)
  }else if (any(extent %in% "layer")){
    # 'layer' is extent of entire sf object
    thisbbox <- sf::st_bbox(data_sf)
  }
  
  coastline <- Mar.data::coast_lores_sf
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = coastline, ggplot2::aes(colour=NA)) +
    ggplot2::geom_sf(data = thisData, ggplot2::aes(colour="#E6D5DBDB",fill = get(fld))) +
    ggplot2::scale_fill_viridis_c(direction = -1, na.value = NA, name =fld) +
    ggplot2::scale_colour_identity() +
    ggplot2::geom_sf(data = coastline, ggplot2::aes(colour="grey16")) +
    ggplot2::coord_sf(xlim = c(thisbbox$xmin, thisbbox$xmax), ylim = c(thisbbox$ymin, thisbbox$ymax)) +
    ggplot2::theme_bw()
  return(p)
}
