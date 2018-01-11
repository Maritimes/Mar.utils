#' @title make_isdb_tracks
#' @description This function takes a df of the isdb data (including fields
#' \code{FISHSET_ID} and LAT1:LAT4 and LONG1:LONG4) and makes a 
#' spatialLinesDataFrame which can be plotted.If selected, it can also QC them.
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
##' @param do.qc default is \code{FALSE}
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
make_isdb_tracks <- function(isdb.df, do.qc = FALSE, return.choice = "lines"){
 
  # Pull out coordinates, stack em, create and populate QC status field ----
  crs.geo <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
  rownames(isdb.df) <- isdb.df$FISHSET_ID
  nsets = nrow(isdb.df)
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

  if (do.qc) {
    isdb.df.long$QC <- ""
    fishsetsWUnplottablePt <- NA
  }
  pos0 = nrow(isdb.df.long[is.na(isdb.df.long$LAT) | is.na(isdb.df.long$LONG),])
  if (pos0>0) {
    if (do.qc) {
      isdb.df.long[is.na(isdb.df.long$LAT) | is.na(isdb.df.long$LONG),"QC"] <- 'pos:NA'
      fishsetsWUnplottablePt = c(fishsetsWUnplottablePt, unique(isdb.df.long[is.na(isdb.df.long$LAT) | is.na(isdb.df.long$LONG),"FISHSET_ID"]))
    }
    #Flag for drop
    isdb.df.long[is.na(isdb.df.long$LAT) | is.na(isdb.df.long$LONG),"LONG"] <- NA
  }
  
  posNA = nrow(isdb.df.long[which(isdb.df.long$LAT == 0 | isdb.df.long$LONG==0),])
  if (posNA>0){
    if (do.qc) {
      isdb.df.long[which(isdb.df.long$LAT == 0 | isdb.df.long$LONG==0),"QC"] <- 'pos:0'
      fishsetsWUnplottablePt = c(fishsetsWUnplottablePt, unique(isdb.df.long[which(isdb.df.long$LAT == 0 | isdb.df.long$LONG==0),"FISHSET_ID"]))
    }
    #Flag for drop
    isdb.df.long[which(isdb.df.long$LAT == 0 | isdb.df.long$LONG==0),"LONG"] <- NA
  }
  posHem = nrow(isdb.df.long[which(isdb.df.long$LONG>0),])
  if (posHem>0) {
    if (do.qc) isdb.df.long[which(isdb.df.long$LONG>0),"QC"] <- 'pos:HEMISPHERE'
    cat(paste0(posHem," coords seem to be in the wrong hemisphere, but will be plotted as-is\n"))
  }
  
  #These are impossible - capture for QC, but don't try to plot
  posLong = nrow(isdb.df.long[which(isdb.df.long$LONG<=-180 | isdb.df.long$LONG>=180),])
  if (posLong>0){
    if (do.qc) {
      isdb.df.long[which(isdb.df.long$LONG<=-180 | isdb.df.long$LONG>=180),"QC"] <- 'pos:impossible LONG'
      fishsetsWUnplottablePt = c(fishsetsWUnplottablePt, unique(isdb.df.long[which(isdb.df.long$LONG<=-180 | isdb.df.long$LONG>=180),"FISHSET_ID"]))
    }
    #Flag for drop
    isdb.df.long[which(isdb.df.long$LONG<=-180 | isdb.df.long$LONG>=180),"LONG"] <- NA
  }
  posLat = nrow(isdb.df.long[which(isdb.df.long$LAT<=-90 | isdb.df.long$LAT>=90),])
  if (posLat>0) {
    if (do.qc) {
      isdb.df.long[which(isdb.df.long$LAT<=-90 | isdb.df.long$LAT>=90),"QC"] <- 'pos:impossible LAT'
      fishsetsWUnplottablePt = c(fishsetsWUnplottablePt, unique(isdb.df.long[which(isdb.df.long$LAT<=-90 | isdb.df.long$LAT>=90),"FISHSET_ID"]))
    }
    #Flag for drop
    isdb.df.long[which(isdb.df.long$LAT<=-90 | isdb.df.long$LAT>=90),"LONG"] <- NA
  }

  # Do the removal -------------------------------------------------------------
  isdb.df.long <- isdb.df.long[complete.cases(isdb.df.long),] 
 
  all.sets.lines<-list()
  if (do.qc){
    # Roll up all the comments for each set into one col ----------------------
    isdb.qc <- aggregate(QC ~ FISHSET_ID, isdb.df.long, function(x) paste0(unique(x), collapse = ", "))
    rownames(isdb.qc) <-isdb.qc$FISHET_ID
    isdb.qc$LEN_KM<-NA
    isdb.qc$N_VALID_VERT<-NA
    
    for (i in 1:length(unique(isdb.df.long$FISHSET_ID))){
      this.a <-unique(isdb.df.long$FISHSET_ID)[i]
      if (length(unique(isdb.df.long[isdb.df.long$FISHSET_ID==this.a,][3:2][,1]))==1 &
          length(unique(isdb.df.long[isdb.df.long$FISHSET_ID==this.a,][3:2][,2]))==1){
        #these have same start and end coords
        isdb.qc[isdb.qc$FISHSET_ID==this.a,"LEN_KM"]<-0
        fishsetsWUnplottablePt = c(fishsetsWUnplottablePt, isdb.qc[isdb.qc$FISHSET_ID==this.a,"FISHSET_ID"])
        isdb.qc[isdb.qc$FISHSET_ID==this.a,"QC"]<-paste(c(isdb.qc[isdb.qc$FISHSET_ID==this.a,"QC"], 'Zero_Len'), collapse = ", ")
        all.sets.lines[[i]]<-NULL
        next
      }else{
        li = Line(isdb.df.long[isdb.df.long$FISHSET_ID==this.a,][3:2])
        all.sets.lines[[i]]<-Lines(li,ID=this.a)
      }
      this.len = LinesLength(Ls = all.sets.lines[[i]], longlat = TRUE)
      isdb.qc[isdb.qc$FISHSET_ID==this.a,"N_VALID_VERT"]<-length(all.sets.lines[[i]]@Lines[[1]]@coords)/2
      isdb.qc[isdb.qc$FISHSET_ID==this.a,"LEN_KM"]<-this.len
      if (this.len == 0) {
        fishsetsWUnplottablePt = c(fishsetsWUnplottablePt, isdb.qc[isdb.qc$FISHSET_ID==this.a,"FISHSET_ID"])
        isdb.qc[isdb.qc$FISHSET_ID==this.a,"QC"]<-paste(c(isdb.qc[isdb.qc$FISHSET_ID==this.a,"QC"], 'Zero_Len'), collapse = ", ")
      }
    }
    fishsetsWUnplottablePt = unique(fishsetsWUnplottablePt)
  }else{
    for (i in 1:length(unique(isdb.df.long$FISHSET_ID))){
      this.a <-unique(isdb.df.long$FISHSET_ID)[i]
      if (length(unique(isdb.df.long[isdb.df.long$FISHSET_ID==this.a,][3:2][,1]))==1 & 
          length(unique(isdb.df.long[isdb.df.long$FISHSET_ID==this.a,][3:2][,2]))==1){
        #these have same start and end coords
        all.sets.lines[[i]]<-NULL
        next
      }else{
        li = Line(isdb.df.long[isdb.df.long$FISHSET_ID==this.a,][3:2])
        all.sets.lines[[i]]<-Lines(li,ID=this.a)
      }
      this.len = LinesLength(Ls = all.sets.lines[[i]], longlat = TRUE)
    }
  }
  
  #drop any lines that have zero length
  all.sets.lines = all.sets.lines[!sapply(all.sets.lines, is.null)]
  all.sets.lines = SpatialLines(all.sets.lines)
  proj4string(all.sets.lines) <- crs.geo
  cat(paste0(length(all.sets.lines), " of ", nsets, " sets could be made into lines having at least 2 points.\n"))
  
  if (do.qc){
    #Sort the qc field prior to merging into line df
    for (k in 1:nrow(isdb.qc)){
      isdb.qc$QC[k] <- paste(sort(unlist(strsplit(isdb.qc$QC[k], ", ")),decreasing = TRUE), collapse = ", ")
    }
    all.sets.lines<-SpatialLinesDataFrame(all.sets.lines, isdb.qc, match.ID = "FISHSET_ID")
    fishsetsWUnplottablePt = setdiff(fishsetsWUnplottablePt, all.sets.lines@data$FISHSET_ID)
   
    if (any(!is.na(fishsetsWUnplottablePt))){
      fishsetsWUnplottablePt <- fishsetsWUnplottablePt[!is.na(fishsetsWUnplottablePt)]
      cat(paste0("The following ",length(fishsetsWUnplottablePt)," sets are unplottable, due to either insufficient valid coordinate pairs or zero-length lines:\n"))
      for (j in 1:length(fishsetsWUnplottablePt)){
        cat(paste0("\t ",fishsetsWUnplottablePt[j],": ",isdb.qc[isdb.qc$FISHSET_ID == fishsetsWUnplottablePt[j],"QC"],"\n"))
      }
    }
  }else{
    cat("For more info on the lines that failed, run this with function with 'do.qc=TRUE'")
    all.sets.lines<-SpatialLinesDataFrame(all.sets.lines, isdb.df[1], match.ID = "FISHSET_ID")
  }
  if (return.choice != "lines" & do.qc){
    return(isdb.qc)
  }else{
    return(all.sets.lines)
  }
}
