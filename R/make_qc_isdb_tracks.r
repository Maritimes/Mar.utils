#' @title make_qc_isdb_tracks
#' @description This function takes a df of the isdb data (including fields
#' \code{FISHSET_ID} and LAT1:LAT4 and LONG1:LONG4) and makes a 
#' spatialLinesDataFrame which can be plotted.
#' Each line will have the following columns in the resulting data frame:
##' \itemize{
##'  \item \code{FISHSET_ID} This uniquely identifies a set
##'  \item \code{QC} This field identifies potential issues with the line - 
##'  including NA positions, positions that are "0", incorrect hemisphere, or 
##'  impossible coordinates
##'  \item \code{LEN_KM} This field shows the calculated distance of the 
##'  resultant line in kms
##' }
#' @param isdb.df This is the dataframe you want to plot.  isdb dataframes are
#' characterized by the existence of the following fields:
##' \itemize{
##'  \item \code{FISHSET_ID}
##'  \item \code{LAT1} and \code{LONG1}
##'  \item \code{LAT2} and \code{LONG2}
##'  \item \code{LAT3} and \code{LONG3}
##'  \item \code{LAT4} and \code{LONG4}
##' }
##' @param return.choice This determines what the function returns.  The default
##' value of \code{"lines"} returns a SpatialLinesDataFrame, where the dataframe 
##' contains the generated QC information.  Any other value for 
##' \code{return.choice} will instead return of data frame of all of the 
##' FISHSET_IDs, the QC determination of each line, the length of each line (in 
##' km), and the number of valid vertices. 
##' The QC data checks the positions of each FISHSET for NAs, Zeroes, or 
##' otherwise data.  Additionally, if the line has a length of zero, that is 
##' noted as well.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom sp CRS
#' @importFrom sp Line
#' @importFrom sp Lines
#' @importFrom sp LinesLength
#' @importFrom sp SpatialLines
#' @importFrom sp SpatialLinesDataFrame
#' @importFrom sp proj4string<-
#' @importFrom stats aggregate
#' @importFrom stats complete.cases
#' @export
make_qc_isdb_tracks <- function(isdb.df, return.choice = "lines"){
  fishsetsWUnplottablePt <- NA
  # Pull out coordinates, stack em, create and populate QC status field ----
  crs.geo <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
  rownames(isdb.df) <- isdb.df$FISHSET_ID
  p1=cbind(isdb.df[c("FISHSET_ID","LAT1","LONG1")],"1")
  colnames(p1)<-c("FISHSET_ID", "LAT", "LONG", "P")
  p2=cbind(isdb.df[c("FISHSET_ID","LAT2","LONG2")],"2")
  colnames(p2)<-c("FISHSET_ID", "LAT", "LONG", "P")
  p3=cbind(isdb.df[c("FISHSET_ID","LAT3","LONG3")],"3")
  colnames(p3)<-c("FISHSET_ID", "LAT", "LONG", "P")
  p4=cbind(isdb.df[c("FISHSET_ID","LAT4","LONG4")],"4")
  colnames(p4)<-c("FISHSET_ID", "LAT", "LONG", "P")
  isdb.df.long <- rbind(p1,p2,p3,p4)
  rm(p1)
  rm(p2)
  rm(p3)
  rm(p4)
  isdb.df.long <- isdb.df.long[order(isdb.df.long$FISHSET_ID, isdb.df.long$P),]
  isdb.df.long$QC <- 'Default Fail'
  
  if (nrow(isdb.df.long[is.na(isdb.df.long$LAT) | is.na(isdb.df.long$LONG),])>0) isdb.df.long[is.na(isdb.df.long$LAT) | is.na(isdb.df.long$LONG),]$QC <- 'pos:NA'
  if (nrow(isdb.df.long[which(isdb.df.long$LAT == 0 | isdb.df.long$LONG==0),])>0) isdb.df.long[which(isdb.df.long$LAT == 0 | isdb.df.long$LONG==0),]$QC <- 'pos:0'
  if (nrow(isdb.df.long[which(isdb.df.long$LONG>0),])>0) isdb.df.long[which(isdb.df.long$LONG>0),]$QC <- 'pos:HEMISPHERE'
  
  #These are impossible - capture for QC, but don't try to plot
  if (nrow(isdb.df.long[which(isdb.df.long$LONG<=-180 | isdb.df.long$LONG>=180),])>0){
    isdb.df.long[which(isdb.df.long$LONG<=-180 | isdb.df.long$LONG>=180),]$QC <- 'pos:impossible LONG'
  }
  if (nrow(isdb.df.long[which(isdb.df.long$LAT<=-90 | isdb.df.long$LAT>=90),])>0) {
    isdb.df.long[which(isdb.df.long$LAT<=-90 | isdb.df.long$LAT>=90),]$QC <- 'pos:impossible LAT'
  }
  
  if (nrow(isdb.df.long[which(isdb.df.long$LAT>0 & isdb.df.long$LONG<0),])>0) isdb.df.long[which(isdb.df.long$LAT>0 & isdb.df.long$LONG<0),]$QC <- 'Valid'
  
  # Roll up all the comments for each set into one col ----------------------
  isdb.qc <- aggregate(QC ~ FISHSET_ID, isdb.df.long, function(x) paste0(unique(x), collapse = ", "))
  rownames(isdb.qc) <-isdb.qc$FISHET_ID
  
  # Flag zeroes for removal ----------------------------------------------------
  fishsetsWUnplottablePt = c(fishsetsWUnplottablePt, unique(isdb.df.long[isdb.df.long$QC == "pos:0","FISHSET_ID"]))
  isdb.df.long[isdb.df.long$QC == "pos:0","LONG"] <- NA

  # Flag impossible values for removal -----------------------------------------
  if (nrow(isdb.df.long[grep(pattern = "pos:impossible", x=isdb.df.long$QC),])>0){
    fishsetsWUnplottablePt = c(fishsetsWUnplottablePt, unique(isdb.df.long[grep(pattern = "pos:impossible", x=isdb.df.long$QC),"FISHSET_ID"]))
    isdb.df.long[grep(pattern = "pos:impossible", x=isdb.df.long$QC),"LONG"]<-NA
  }
  
  # Do the removal -------------------------------------------------------------
  isdb.df.long <- isdb.df.long[complete.cases(isdb.df.long),] 
  
  # With remaining coords, create lines, capture length -------------------------
  all.sets.lines<-list()
  isdb.qc$LEN_KM<-NA
  isdb.qc$N_VALID_VERT<-NA
  for (i in 1:length(unique(isdb.df.long$FISHSET_ID))){
    this.a <-unique(isdb.df.long$FISHSET_ID)[i]
    li = Line(isdb.df.long[isdb.df.long$FISHSET_ID==this.a,][3:2])
    all.sets.lines[[i]]<-Lines(li,ID=this.a)
    this.len = LinesLength(Ls = all.sets.lines[[i]], longlat = TRUE)
    isdb.qc[isdb.qc$FISHSET_ID==this.a,"N_VALID_VERT"]<-length(all.sets.lines[[i]]@Lines[[1]]@coords)/2
    isdb.qc[isdb.qc$FISHSET_ID==this.a,"LEN_KM"]<-this.len
    if (this.len == 0) {
      fishsetsWUnplottablePt = c(fishsetsWUnplottablePt, isdb.qc[isdb.qc$FISHSET_ID==this.a,"FISHSET_ID"])
      isdb.qc[isdb.qc$FISHSET_ID==this.a,"QC"]<-paste(c(isdb.qc[isdb.qc$FISHSET_ID==this.a,"QC"], 'Zero_Len'), collapse = ", ")
    }
  }
  fishsetsWUnplottablePt = unique(fishsetsWUnplottablePt)
  all.sets.lines = SpatialLines(all.sets.lines)
  proj4string(all.sets.lines) <- crs.geo
  #Sort the qc field prior to merging into line df
  for (k in 1:nrow(isdb.qc)){
    isdb.qc$QC[k] <- paste(sort(unlist(strsplit(isdb.qc$QC[k], ", ")),decreasing = TRUE), collapse = ", ")
  }
  all.sets.lines<-SpatialLinesDataFrame(all.sets.lines, isdb.qc, match.ID = "FISHSET_ID")
  fishsetsWUnplottablePt = setdiff(fishsetsWUnplottablePt, all.sets.lines@data$FISHSET_ID)
  if (any(!is.na(fishsetsWUnplottablePt))){
    fishsetsWUnplottablePt <- fishsetsWUnplottablePt[!is.na(fishsetsWUnplottablePt)]
    cat(paste0("\nThe following ",length(fishsetsWUnplottablePt)," sets are unplottable.  :\n"))
    for (j in 1:length(fishsetsWUnplottablePt)){
      #edit qc field to identify unplottable stuff.
      isdb.qc[isdb.qc$FISHSET_ID == fishsetsWUnplottablePt[j],"QC"]<-paste0(c("Unplottable", isdb.qc[isdb.qc$FISHSET_ID == fishsetsWUnplottablePt[j],"QC"]), collapse = ", ")
      cat(paste0(fishsetsWUnplottablePt[j],": ",isdb.qc[isdb.qc$FISHSET_ID == fishsetsWUnplottablePt[j],"QC"],"\n"))
    }
  }
  
  if (return.choice == "lines"){
    return(all.sets.lines)
  }else{
    return(isdb.qc)
  }
}
