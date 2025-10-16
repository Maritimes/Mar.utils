#' @title make_segments
#' @description This function takes a dataframe with coordinates in decimal 
#' degrees and a track identifier, and creates line segments for each distinct 
#' identifier.  If specified, it will generate spatial layers within a gpkg file.
#' @param df default is \code{NULL}.  This is the dataframe to be processed.  It 
#' should have coordinates in decimal degrees.  It should also have fields for use by 
#' \code{objField} and \code{seqField}.
#' @param objField default is \code{NULL}. This is a field identifying which 
#' points are part of the same track.
#' @param seqField default is \code{NULL}. This is a field which can be used to 
#' correctly order the positions along the track (dates are fine).
#' @param lat.field the default is \code{"LATITUDE"}. the name of the field holding latitude values 
#' (in decimal degrees)
#' @param lon.field the default is \code{"LONGITUDE"}.  the name of the field holding longitude 
#' values (in decimal degrees)
#' @param points default is \code{"orphans"}. Valid options include "orphans", "all" and "none". 
#' While this function primarily generates linestrings/tracks, it can also include the same data as 
#' points/vertices.  "none" will not include any vertices, "all" will include all vertices, and 
#' "orphans" will only include lone vertices, which are not part of any segment.
#' @param the.crs default is \code{"EPSG:4326"}. This is the projection 
#' you want any generated data to be output in.  Input data is assumed to be 
#' from a GPS and should be WGS84 (which is what the default value corresponds 
#' with).
#' @param filename default is \code{NULL}.  If you are outputting spatial objects, 
#' you can specify a name for them here.  This used to correspond with the output shapefile name,
#' but now is used to identify the layers within a gpkg file. They will also get a timestamp. 
#' @param create.spatial default is \code{TRUE}.  This indicates whether or not to create a gpkg 
#' file in your working directory.
#' @param gpkgName default is \code{"make_segments.gpkg"}.  If \code{create.spatial = TRUE}, a gpkg 
#' file will be created, and the name here will control what it is called.
#' @param path this is the path to the gpkg file (e.g. "c:/folder/")
#' @return a list containing sf objects.  Additionally, a gpkg can be generated.
#' @family spatial
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
make_segments <- function(df, objField = "SEGMID", seqField ="POSITION_UTC_DATE",
                          lat.field= "LATITUDE",lon.field="LONGITUDE",
                          points = "orphans", the.crs = "EPSG:4326", 
                          filename = NULL, create.spatial = TRUE,
                          gpkgName = "make_segments.gpkg", path=NULL){
  #following are vars that will be created by data.table, and build errors
  #appear if we don't define them
  if (is.null(path)) path<-getwd()
  
  trekMax <- trekMin <- cnt <- NULL
  `:=` <- function (x, value) value
  name=""
  ts = format(Sys.time(), "%Y%m%d_%H%M")
  if (is.null(filename)) {
    name = ts
  }else{
    name = filename
    name = gsub('()','',name)
    name = gsub('\\.','',name)
    name = paste(name,"_",ts,sep="" )
  }
  
  mergeableData<-function(df){
    intcols = lapply(df, function(x) unique(x))
    forDrop <- NULL 
    maxVals <- length(intcols[[objField]])
    for (l in 1:length(intcols)){
      if(length(intcols[[l]])> maxVals){
        forDrop <- c(forDrop, names(intcols[l]))
      }
    }
    keep.fields <- names(df)[!names(df) %in% c(forDrop,"SPEED_KNOTS","distCalc","time_min","KNOTS_CALC","UPDATE_DATE")]
    dfDets <- unique(df[,c(keep.fields)])
    return(dfDets)
  }
  # #check for trips that only have one point
  check = plyr::count(df, objField)
  names(check)[names(check)==objField]<-"objID"
  dataLines = df[df[,objField] %in% check[check$freq>1,"objID"],]
  dataPoints = df[df[,objField] %in% check[check$freq==1,"objID"],]
  res=list()
  res[["points"]]=NA
  res[["segments"]]=NA
  if (points == "all") {
    plotPoints = df_to_sf(df, lat.field = lat.field, lon.field = lon.field, type = "points") #,the.crs = the.crs
    plotPoints <- sf::st_transform(plotPoints, crs = the.crs)
    res[[1]]=plotPoints
    if (create.spatial) {
      df_sf_to_gpkg(plotPoints, layerName = paste0(name,"_pt"), gpkgName = gpkgName, path=path)
    }
  } else if (points =="none"){
    # plotPoints = NA
  } else {
    if (nrow(dataPoints)==0){
      # cat("\nNo points are orphaned")
    }else{
      plotPoints = df_to_sf(dataPoints, type = "points")
      res[["points"]]=plotPoints
      if (create.spatial) {
        df_sf_to_gpkg(plotPoints, layerName = paste0(name,"_pt"), gpkgName = gpkgName, path=path)
      }
    }
  } 
  
  if (nrow(dataLines)){
    dataLines=data.table::setDT(dataLines)
    dataLines[ , trekMin := min(get(seqField)), by = objField]
    dataLines[ , trekMax := max(get(seqField)), by = objField]
    dataLines[ , cnt := length(get(objField)), by = objField]
    dataLines = as.data.frame(dataLines)
    lineData <- mergeableData(dataLines)
    segs = unique(dataLines[,objField])
    plotLines <- df_to_sf(df = dataLines, primary.object.field = objField, order.field = seqField, type= "lines", lat.field = lat.field,  lon.field = lon.field)
    plotLines <- sf::st_transform(plotLines, crs = the.crs)
    plotLines<-merge(plotLines, lineData)
    res[["segments"]] <- plotLines
    if (create.spatial) {
      df_sf_to_gpkg(plotLines, layerName = paste0(name,"_line"), gpkgName = gpkgName, , path=path)
    }
  }else{
    message("\nNo segments could be made")
  }
  return(invisible(res))
}