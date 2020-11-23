#' @title csv2other_poly
#' @description This function will turn PBSMapping style csv into either an 
#' sf object, an sp object or a shapefile.
#' @param csv a PBSMapping PolySet to be converted.
#' @param out default is \code{'sf'}.  This indicates whether an \code{sf} object, 
#' \code{'sp'} (SpatialPolygonsDataFrame) or \code{'shp'} (shapefile) should be created.
#' @param path if \code{out ='shp'}, this is the path to where the shapefile 
#' should be saved.  sf and sp objects are just created in the environment
#' @return an sf object, a SpatialPolygonsDataFrame,or a shapefile
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
csv2other_poly <- function(csv=NULL, out='sf', path = NULL){
  out <- tolower(out)
  nm <- gsub(".csv","", basename(csv))
  
  csv1 <- read.csv(csv)
  if (!all(c("PID","POS","X","Y") %in% names(csv1))) stop("Input file needs PID, POS, X and Y columns")
  #polyset-formatted csv
  csv1 <- PBSmapping::as.PolySet(csv1, projection = "LL")
  polySp = suppressWarnings(maptools::PolySet2SpatialPolygons(csv1))
  thisdata = data.frame(PID = unique(csv1$PID))
  thisdata$ID = seq(1:nrow(thisdata))
  polySpDf <- tryCatch({
    rownames(thisdata) <- thisdata$PID
    polySpDf <- sp::SpatialPolygonsDataFrame(Sr = polySp, data =thisdata)
  },
  error = function(cond) {
    rownames(thisdata) <- thisdata$ID
    polySpDf <- sp::SpatialPolygonsDataFrame(Sr = polySp, data =thisdata)
  })
  
  if (out == 'sp'){
    return(polySpDf) 
  } else if (out=="shp"){
    thePath <- ifelse(is.null(path), getwd(), path)
    rgdal::writeOGR(obj = polySpDf, layer = nm, dsn = thePath, driver="ESRI Shapefile", overwrite_layer=TRUE)
    print(paste0("Saved shapefile to ",thePath,"/",nm,".shp"))
    return(NULL)
  }else if (out=="sf"){
    polySf <- sf::st_as_sf(polySpDf) 
    return(polySf)
  }
  
}