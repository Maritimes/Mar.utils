#' @title dets_defuddler
#' @description This function decodes the various marfis *_DETS table from long to wide, associating 
#' all of the data available for each discrete record.  
#' @param marfName default is \code{NULL}.  This is the name that describes the marfis data sent to 
#' \code{df}.  It MUST be one of:
#' \itemize{
#'  \item \code{"LOG_EFRT_ENTRD_DETS"}
#'  \item \code{"LOG_SPC_ENTRD_DETS"}
#'  \item \code{"MON_DOC_ENTRD_DETS"}
#'  \item \code{"SD_LOG_ENTRD_DETS"}
#'  \item \code{"SD_SLP_ENTRD_DETS"}
#'  \item \code{"SLIP_SPC_ENTRD_DETS"} 
#'  \item \code{"SUM_DOC_ENTR_DETS"}
#'  }
#' @param df default is \code{NULL}.  This is a data.frame corresponding with one of the following
#' marfis objects:
#' \itemize{
#'  \item \code{"LOG_EFRT_ENTRD_DETS"}
#'  \item \code{"LOG_SPC_ENTRD_DETS"}
#'  \item \code{"MON_DOC_ENTRD_DETS"}
#'  \item \code{"SD_LOG_ENTRD_DETS"}
#'  \item \code{"SD_SLP_ENTRD_DETS"}
#'  \item \code{"SLIP_SPC_ENTRD_DETS"} 
#'  \item \code{"SUM_DOC_ENTR_DETS"}
#'  }
#' The data must retain the formatting of the original Oracle objects (i.e. column names)
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
dets_defuddler <- function(marfName = NULL, df = NULL){
  DETS_COL_LOOKUP <- COLUMN_DEFN_ID <- SUM_DOC_DEFN_COL_ID <- NA
  dfNm <- toupper(marfName)
  getCols <- function(df = NULL, type= NULL){
    if (type == "COL"){
      col = "COLUMN_DEFN_ID"
    }else if (type == "SUM"){
      col = "SUM_DOC_DEFN_COL_ID"
    }
    
    theseDets <-  unique(DETS_COL_LOOKUP[DETS_COL_LOOKUP[,col] %in% names(df),c(col, "DESC_ENG")])
    codedColsPresent <- as.character(sort(as.numeric(names(df)[names(df) %in% theseDets[,col]])))
    otherColsPresent <- sort(names(df)[!names(df) %in% theseDets[,col]])
    
    theseDets$DESC_ENG <- paste0(theseDets$DESC_ENG,"_", theseDets$SUM_DOC_DEFN_COL_ID)
    
    if (type=="COL"){
      theseDets <- dplyr::arrange(theseDets, COLUMN_DEFN_ID)
    } else if (type == "SUM"){
      theseDets <- dplyr::arrange(theseDets, SUM_DOC_DEFN_COL_ID)
    }
    df<- df[,c(otherColsPresent, codedColsPresent)]
    names(df) <- c(otherColsPresent, theseDets$DESC_ENG)
    return(df)
  }
  doSD_LOG_ENTRD_DETS  <- function(df = NULL){
    df1<- unique(df[!is.na(df$SD_LOG_SPC_STD_INFO_ID),c("SUM_DOC_DEFN_COL_ID", "DATA_VALUE", "SD_LOG_ID", "SD_LOG_SPC_STD_INFO_ID")])
    df2<- unique(df[!is.na(df$SD_LOG_EFF_STD_INFO_ID),c("SUM_DOC_DEFN_COL_ID", "DATA_VALUE", "SD_LOG_ID", "SD_LOG_EFF_STD_INFO_ID")])
    df3<- unique(df[,c("SUM_DOC_DEFN_COL_ID", "DATA_VALUE", "SD_LOG_ID")])
    
    if(nrow(df1>0)) {
      df1_W <- reshape2::dcast(df1, SD_LOG_ID+SD_LOG_SPC_STD_INFO_ID~SUM_DOC_DEFN_COL_ID, value.var = "DATA_VALUE")
    }else{
      df1_W <- df1
    }
    if(nrow(df2>0)) {
      df2_W <- reshape2::dcast(df2, SD_LOG_ID+SD_LOG_EFF_STD_INFO_ID~SUM_DOC_DEFN_COL_ID, value.var = "DATA_VALUE")
    }else{
      df2_W <- df2
    }
    if(nrow(df3>0)) {
      df_W <- reshape2::dcast(df3, SD_LOG_ID~SUM_DOC_DEFN_COL_ID, value.var = "DATA_VALUE")
    }else{
      stop()
    }
    
    if (nrow(df2_W)>0){
      df_W <- merge(df_W, df2_W, all.x = T)
    }else{
      df_W$SD_LOG_EFF_STD_INFO_ID <- NA
    }
    if (nrow(df1_W)>0){
      df_W <- merge(df_W, df1_W, all.x = T)
    }else{
      df_W$SD_LOG_SPC_STD_INFO_ID <- NA
    }
    df_W <- getCols(df_W, type="SUM")
    return(df_W)
  }
  doSD_SLP_ENTRD_DETS  <- function(df = NULL){
    
    df1<- unique(df[!is.na(df$SD_SLP_LND_STD_INFO_ID),c("SUM_DOC_DEFN_COL_ID", "DATA_VALUE", "SD_SLP_ID", "SD_SLP_LND_STD_INFO_ID")])
    df2<- unique(df[!is.na(df$SD_SLP_BYR_STD_INFO_ID),c("SUM_DOC_DEFN_COL_ID", "DATA_VALUE", "SD_SLP_ID", "SD_SLP_BYR_STD_INFO_ID")])
    df3<- unique(df[!is.na(df$SD_SLP_SPC_STD_INFO_ID),c("SUM_DOC_DEFN_COL_ID", "DATA_VALUE", "SD_SLP_ID", "SD_SLP_SPC_STD_INFO_ID")])
    df4<- unique(df[,c("SUM_DOC_DEFN_COL_ID", "DATA_VALUE", "SD_SLP_ID")])
    
    if(nrow(df1>0)) {
      df1_W <- reshape2::dcast(df1, SD_SLP_ID+SD_SLP_LND_STD_INFO_ID~SUM_DOC_DEFN_COL_ID, value.var = "DATA_VALUE")
    }else{
      df1_W<-df1 
    }
    if(nrow(df2>0)) {
      df2_W <- reshape2::dcast(df2, SD_SLP_ID+SD_SLP_BYR_STD_INFO_ID~SUM_DOC_DEFN_COL_ID, value.var = "DATA_VALUE")
    }else{
      df2_W<- df2
    }
    if(nrow(df3>0)) {
      df3_W <- reshape2::dcast(df3, SD_SLP_ID+SD_SLP_SPC_STD_INFO_ID~SUM_DOC_DEFN_COL_ID, value.var = "DATA_VALUE")
    }else{
      df3_W<- df3
    }
    if(nrow(df4>0)){
      df_W <- reshape2::dcast(df4, SD_SLP_ID~SUM_DOC_DEFN_COL_ID, value.var = "DATA_VALUE")
    }else{
      stop()
    }
    if (nrow(df3_W)>0){
      df_W <- merge(df_W, df3_W, all.x = T)
    }else {
      df_W$SD_SLP_LND_STD_INFO_ID <- NA
    }
    if (nrow(df2)>0){
      df_W <- merge(df_W, df2_W, all.x = T)
    }else {
      df_W$SD_SLP_BYR_STD_INFO_ID <- NA
    }
    if (nrow(df1_W)>0){
      df_W <- merge(df_W, df1_W, all.x = T)
    }else {
      df_W$SD_SLP_SPC_STD_INFO_ID <- NA
    }
    df_W <- getCols(df_W, type="SUM")
    return(df_W)
  }
  doLOG_EFRT_ENTRD_DETS <- function(df = NULL){
    idEm <- c("LOG_EFRT_STD_INFO_ID", "TRIP_DMP_COMPANY_ID")
    measureEm<- "COLUMN_DEFN_ID"
    
    wide <- reshape2::dcast(df, paste(paste(idEm, collapse = " + "), "~", paste(measureEm, collapse = " + ")), value.var = "DATA_VALUE")
    wide <- getCols(wide, type = "COL")
    return(wide)
  }
  doLOG_SPC_ENTRD_DETS  <- function(df = NULL){
    idEm <- c("LOG_SPC_STD_INFO_ID", "TRIP_DMP_COMPANY_ID")
    measureEm <- "COLUMN_DEFN_ID"
    
    wide <- reshape2::dcast(df, paste(paste(idEm, collapse = " + "), "~", paste(measureEm, collapse = " + ")), value.var = "DATA_VALUE")
    wide <- getCols(wide, type = "COL")
    return(wide)
  }
  doMON_DOC_ENTRD_DETS  <- function(df = NULL){
    idEm <- c("MON_DOC_ID", "TRIP_DMP_COMPANY_ID")
    measureEm <- "COLUMN_DEFN_ID"
    
    wide <- reshape2::dcast(df, paste(paste(idEm, collapse = " + "), "~", paste(measureEm, collapse = " + ")), value.var = "DATA_VALUE")
    
    cat("\nwhat's with the >1000 'COLUMN_DEFN_IDs'?")
    wide <- getCols(wide, type = "COL")
    return(wide)
  }
  doSLIP_SPC_ENTRD_DETS  <- function(df = NULL){
    idEm <- c("SLIP_SPC_STD_INFO_ID", "TRIP_DMP_COMPANY_ID")
    measureEm <- "COLUMN_DEFN_ID"
    
    wide <- reshape2::dcast(df, paste(paste(idEm, collapse = " + "), "~", paste(measureEm, collapse = " + ")), value.var = "DATA_VALUE")
    wide <- getCols(wide, type = "COL")
    return(wide)
  }
  doSUM_DOC_ENTR_DETS  <- function(df = NULL){
    df$SUM_DOC_ENTR_DET_ID <- NULL
    df <- unique(df)
    idEm <- c("SUM_DOC_ID")
    measureEm <- "SUM_DOC_DEFN_COL_ID"
    
    wide <- reshape2::dcast(df, paste(paste(idEm, collapse = " + "), "~", 
                                      paste(measureEm, collapse = " + ")), value.var = "DATA_VALUE")
    wide <- getCols(wide, type="SUM")
    return(wide)
  }
  
  dets <- switch(dfNm,
                 "LOG_EFRT_ENTRD_DETS" = doLOG_EFRT_ENTRD_DETS(df=df),
                 "LOG_SPC_ENTRD_DETS" = doLOG_SPC_ENTRD_DETS(df=df),
                 "MON_DOC_ENTRD_DETS" = doMON_DOC_ENTRD_DETS(df=df),
                 "SD_LOG_ENTRD_DETS" = doSD_LOG_ENTRD_DETS(df=df),
                 "SD_SLP_ENTRD_DETS" = doSD_SLP_ENTRD_DETS(df=df),
                 "SLIP_SPC_ENTRD_DETS" = doSLIP_SPC_ENTRD_DETS(df=df),
                 "SUM_DOC_ENTR_DETS" = doSUM_DOC_ENTR_DETS(df=df)
  )
  
  dets[] <- lapply(dets, function(x) utils::type.convert(as.character(x), as.is = TRUE))
  dets[grep("DATE", colnames(dets))] <- lapply(dets[grep("DATE", colnames(dets))], function(x) as.Date(x, format="%Y-%b-%d", origin = "1970-01-01"))
  dets[grep("DATE", colnames(dets))] <- lapply(dets[grep("DATE", colnames(dets))], function(x) as.POSIXct.Date(x, format="%Y-%b-%d", origin = "1970-01-01"))
  
  return(dets)
}





