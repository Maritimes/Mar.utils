#' @title pbs2sp_poly
#' @description This function will turn PBSMapping PolySets into a 
#' SpatialPolygonsDataFrame, and can generate a shapefile at teh same time, if 
#' requested.
#' @param pbs a PBSMapping PolySet to be converted.
#' @param create.shp default is \code{FALSE}.  This indicates whether or not 
#' a shapefile should be created.
#' @return a SpatialPolygonsDataFrame, and potentially a shapefile
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
pbs2sp_poly <- function(pbs=NULL, create.shp=FALSE){
.Deprecated("csv2other_poly", msg = "This functionality and more is possible in convert2poly")
  nm = deparse(substitute(pbs))
  nm = gsub(replacement = "_",x = nm, pattern = "\\.")
  this = maptools::PolySet2SpatialPolygons(pbs)
  thisdata = data.frame(PID = unique(pbs$PID))
  thisdata$ID = seq(1:nrow(thisdata))
  polySpDf <- tryCatch({
    rownames(thisdata) <- thisdata$PID
    polySpDf <- sp::SpatialPolygonsDataFrame(Sr = this, data =thisdata)
  },
  error = function(cond) {
    rownames(thisdata) <- thisdata$ID
    polySpDf <- sp::SpatialPolygonsDataFrame(Sr = this, data =thisdata)
  })
  if (create.shp){
    rgdal::writeOGR(obj = polySpDf, layer = nm, dsn = ".", driver="ESRI Shapefile", overwrite_layer=TRUE)
    print(paste0("Saved shapefile to ",getwd(),"/",nm,".shp"))
  }
  return(polySpDf)
}