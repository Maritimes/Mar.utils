#' @title make_segments
#' @description This function takes a dataframe with coordinates in decimal 
#' degrees and a track identifier, and creates line segments for each distinct 
#' identifier.  If specified, it will plot the results in R and/or create 
#' shapefiles.
#' @param df default is \code{NULL}.  This is the dataframe to be processed.  It 
#' should have coordinates in decimal degrees and they should be in fields 
#' called "LATITUDE" and "LONGITUDE".  It should also have fields for use by 
#' \code{objField} and \code{seqField}.
#' @param objField default is \code{NULL}. This is a field identifying which 
#' points are part of the same track;
#' @param seqField default is \code{NULL}. This is a field which can be used to 
#' correctly order the positions along the track (dates are fine).
#' @param lat.field the default is \code{"LATITUDE"}. the name of the field holding latitude values 
#' (in decimal degrees)
#' @param lon.field the default is \code{"LONGITUDE"}.  the name of the field holding longitude 
#' values (in decimal degrees)
#' @param points default is \code{"orphans"}. While this function generates 
#' tracks, it's possible that single records can exist from which no track can 
#' be displayed.  Setting it to "orphans" includes these lone positions in the 
#' output plots and/or shapefiles.  Setting it to "all" includes all positions
#' in the output plots and/or shapefiles. Setting it to "none" ignores points
#' and doesn't output any. 
#' @param the.crs default is \code{"+init=epsg:4326"}. This is the projection 
#' you want any generated data to be output in.  Input data is assumed to be 
#' from a GPS and should be WGS84 (which is what the default value corresponds 
#' with).
#' @param filename default is \code{NULL}.  If you are outputting shapefiles, 
#' you can specify a name for them here.  They will also get a timestamp. 
#' @param plot default is \code{TRUE}. This determines whether or not generated 
#' tracks will be plotted.
#' @param createShp default is \code{TRUE}. This determines whether or not 
#' shapefiles will be generated in your working directory.
#' @importFrom data.table setDT
#' @return a list with 2 items - a SpatialPointsDataFrame, and a 
#' SpatialLinesDataFrame.  Additionally, shapefiles can also be generated.
#' @importFrom plyr count
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
make_segments <- function(df, objField = "SEGMID", seqField ="POSITION_UTC_DATE",
                          lat.field= "LATITUDE",lon.field="LONGITUDE",
                          points = "orphans", the.crs = "+init=epsg:4326", 
                          filename = NULL, plot=TRUE, createShp = TRUE){
  #following are vars that will be created by data.table, and build errors
  #appear if we don't define them
  trekMax <- trekMin <- cnt <- NULL
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
  df = df[order(df[objField],df[seqField]),]
  # #check for trips that only have one point
  
  check = plyr::count(df, objField)
  names(check)[names(check)==objField]<-"objID"
  dataLines = df[df[,objField] %in% check[check$freq>1,"objID"],]
  dataPoints = df[df[,objField] %in% check[check$freq==1,"objID"],]
  res=list()
  shapes = NA
  res[["points"]]=NA
  res[["segments"]]=NA
  if (points == "all") {
    plotPoints = df_to_sp(df,the.crs = the.crs)
    res[[1]]=plotPoints
    if (createShp) {
      plotPoints = prepare_shape_fields(plotPoints)
      rgdal::writeOGR(obj = plotPoints, layer =paste0(name,"_pt"), dsn=getwd(), driver="ESRI Shapefile", overwrite_layer=TRUE)
      shapes = c(shapes,paste0(name,"_pt.shp"))
    }
  } else if (points =="none"){
    # plotPoints = NA
  } else {
    if (nrow(dataPoints)==0){
      # cat("\nNo points are orphaned")
    }else{
      plotPoints = df_to_sp(dataPoints,the.crs = the.crs)
      res[["points"]]=plotPoints
      if (createShp) {
        plotPoints = prepare_shape_fields(plotPoints)
        rgdal::writeOGR(obj = plotPoints, layer =paste0(name,"_pt"), dsn=getwd(), driver="ESRI Shapefile", overwrite_layer=TRUE)
        shapes = c(shapes,paste0(name,"_pt"))
      }
    }
  } 
  
  if (nrow(dataLines)){
    dataLines=data.table::setDT(dataLines)
    dataLines[ , trekMin := min(get(seqField)), by = objField]
    dataLines[ , trekMax := max(get(seqField)), by = objField]
    dataLines[ , cnt := length(get(objField)), by = objField]
    dataLines = as.data.frame(dataLines)
    
    plotLines<-list()
    segs = unique(dataLines[,objField])
    for (i in 1:length(segs)){
      li = sp::Line(dataLines[dataLines[objField]==segs[i],][c(lon.field,lat.field)])
      plotLines[[i]]<-sp::Lines(li,ID=segs[i])
    }
    plotLines = sp::SpatialLines(plotLines)
    sp::proj4string(plotLines) <- sp::CRS(the.crs)
    dets = as.data.frame(dataLines[!duplicated(dataLines[c(objField)]),]) 
    rownames(dets) <- dets[,objField]
    plotLines<-sp::SpatialLinesDataFrame(plotLines,data = dets, match.ID = TRUE)
    res[["segments"]]=plotLines
    if (createShp) {
      plotLines = prepare_shape_fields(plotLines)
      rgdal::writeOGR(obj = plotLines, layer =paste0(name,"_line"), dsn=getwd(), driver="ESRI Shapefile", overwrite_layer=TRUE)
      shapes = c(shapes,paste0(name,"_line.shp"))
    }
  }else{
    cat("\nNo segments could be made")
  }
  
  if (plot == TRUE){
    addIt = FALSE
    if (exists("plotLines")){
      sp::plot(plotLines)
      addIt = TRUE
    }
    if (exists("plotPoints"))sp::plot(plotPoints, add=addIt)
  }
  
  if (any(!is.na(shapes))) {
    cat(paste0("\nThe following shapefiles were written to ",getwd(),": "))
    shapes=shapes[!is.na(shapes)]
    print(shapes)
  }
  return(invisible(res))
}