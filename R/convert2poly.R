#' @title convert2poly
#' @description This function will turn PBSMapping style csv into either an 
#' sf object, an sp object or a shapefile.
#' @param input a PBSMapping PolySet to be converted.
#' @param out default is \code{'sf'}.  This indicates whether an \code{sf} object, 
#' \code{'sp'} (SpatialPolygonsDataFrame) or \code{'shp'} (shapefile) should be created.
#' @param shp.path if \code{out ='shp'}, this is the path to where the shapefile 
#' should be saved.  sf and sp objects are just created in the environment
#' @param lat.field the default is \code{"LATITUDE"}. the name of the field holding latitude values
#' (in decimal degrees)
#' @param lon.field the default is \code{"LONGITUDE"}.  the name of the field holding longitude
#' values (in decimal degrees)
#' @return an sf object, a SpatialPolygonsDataFrame,or a shapefile
#' @note if no  PID or POS is provided, it is assumed that the provided positions
#' are for a single polygon and the position are in the correct order.
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
convert2poly <- function(input=NULL, 
                         header = TRUE,
                         out='sf', 
                         lat.field = 'X', lon.field = 'Y',
                         PID = "PID", POS= "POS",
                         shp.path = NULL){
  
  nm <- gsub(".csv|.dat","", basename(input))
  
  theInput <- read.csv(input, header = header)
  if(ncol(theInput)<2) {
    theInput <- read.table(input, header = header)

    if (!all(c(lat.field,lon.field) %in% names(theInput)) && all(c("V1","V2") %in% names(theInput))){
      if (mean(theInput$V1)<0){
        colnames(theInput)[colnames(theInput)=="V1"] <- "X"
        colnames(theInput)[colnames(theInput)=="V2"] <- "Y"
      }else{
        colnames(theInput)[colnames(theInput)=="V1"] <- "Y"
        colnames(theInput)[colnames(theInput)=="V2"] <- "X"
      }
    }
  }
  
  if(!PID %in% colnames(theInput)) theInput$PID <- 1
  if(!POS %in% colnames(theInput)) theInput$POS <- seq(1:nrow(theInput))
  
  colnames(theInput)[colnames(theInput)==lat.field] <- "X"
  colnames(theInput)[colnames(theInput)==lon.field] <- "Y"
  colnames(theInput)[colnames(theInput)==PID] <- "PID"
  colnames(theInput)[colnames(theInput)==POS] <- "POS"
  
  theInput <- PBSmapping::as.PolySet(theInput, projection = "LL")
  polySp = suppressWarnings(maptools::PolySet2SpatialPolygons(theInput))
  thisdata = data.frame(PID = unique(theInput$PID))
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
  } else if ( tolower(out)=="shp"){
    thePath <- ifelse(is.null(shp.path), getwd(), shp.path)
    rgdal::writeOGR(obj = polySpDf, layer = nm, dsn = thePath, driver="ESRI Shapefile", overwrite_layer=TRUE)
    print(paste0("Saved shapefile to ",thePath,"/",nm,".shp"))
    return(NULL)
  }else if ( tolower(out)=="sf"){
    polySf <- sf::st_as_sf(polySpDf) 
    return(polySf)
  }
  
}