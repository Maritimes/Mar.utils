# R.utils::sourceDirectory("c:/git/Maritimes/Mar.utils/R/", modifiedOnly=F)
# load("C:/DFO-MPO/wrangledData/MARFISSCI.PRO_SPC_INFO.RData")
# PSTEST<- PRO_SPC_INFO[which(PRO_SPC_INFO$YEAR == 2020 & PRO_SPC_INFO$SPECIES_CODE == 130),]
# # .rs.restartR()

data.table::setDF(PSTEST)
getVMSfromMARFIS <- function(fn.oracle.username = "_none_", 
                             fn.oracle.password = "_none_", 
                             fn.oracle.dsn = "_none_",
                             usepkg = 'rodbc',
                             data.dir=NULL,
                             df=NULL,
                             VR_field = "VR_NUMBER",
                             LIC_field = "LICENCE_ID",
                             GC_field = "GEAR_CODE",
                             LANDED_field = "LANDED_DATE",
                             look_ahead_days = 7,
                             lat.field = NULL,
                             lon.field = NULL,
                             make_segments=T,
                             make_segments_shp = T){
  library(data.table)
  library(sqldf)
  
  
  #data.table doesn't like column name references - ensure the col names are known
  colnames(df)[colnames(df)==VR_field] <- "VR_NUMBER"
  colnames(df)[colnames(df)==LIC_field] <- "LICENCE_ID"
  colnames(df)[colnames(df)==GC_field] <- "GEAR_CODE"
  colnames(df)[colnames(df)==LANDED_field] <- "LANDED_DATE"
  
  bbox <- NA
  if (!is.null(lat.field) && !is.null(lon.field)){
    df<- unique(df[,c("VR_NUMBER", "LICENCE_ID", "GEAR_CODE", "LANDED_DATE", lat.field, lon.field)])
    bbox <- c(range(df[!is.na(df[lat.field]),lat.field]), 
              range(df[!is.na(df[lon.field]),lon.field]))
  }else{
    df<- unique(df[,c("VR_NUMBER", "LICENCE_ID", "GEAR_CODE", "LANDED_DATE")])
  }
  
  df<-df[!is.na(df$VR_NUMBER) & !is.na(df$LICENCE_ID) & !is.na(df$GEAR_CODE), ]
  df <- data.table::setDT(df) 
  df <- setorder(df, VR_NUMBER, LANDED_DATE) 
  df <-df[, pseudo_start := shift(LANDED_DATE, type= "lag"), by= VR_NUMBER ]
  df <- data.table::setDF(df) 
  df[is.na(df$pseudo_start),"pseudo_start"] <- df[is.na(df$pseudo_start),"LANDED_DATE"] - as.difftime(look_ahead_days, unit="days")
  dateRange <- as.Date(c(min(df$pseudo_start), max(df$LANDED_DATE)))
  vrns<- sort(unique(df$VR_NUMBER))
  message("Starting VMS extraction - grab a coffee")
  if (is.na(bbox)){
    theVMS <- VMS_get_recs(fn.oracle.username = fn.oracle.username, 
                           fn.oracle.password = fn.oracle.password, 
                           fn.oracle.dsn = fn.oracle.dsn,
                           usepkg = usepkg,
                           vrnList = vrns,
                           dateStart = as.character(min(dateRange)),
                           dateEnd = as.character(max(dateRange)),  
                           rowNum = 1000000, 
                           quietly = T)
  }else{
    theVMS <- VMS_get_recs(fn.oracle.username = fn.oracle.username, 
                           fn.oracle.password = fn.oracle.password, 
                           fn.oracle.dsn = fn.oracle.dsn,
                           usepkg = usepkg,
                           vrnList = vrns,
                           dateStart = as.character(min(dateRange)),
                           dateEnd = as.character(max(dateRange)),  
                           minLat = bbox[1], maxLat=bbox[2], 
                           minLon = bbox[3], maxLon=bbox[4],
                           rowNum = 1000000, 
                           quietly = T)
  }
  if (nrow(theVMS)==1000000)warning("Hit extraction row limit")
  theVMS <- VMS_clean_recs(df=theVMS)
  theVMS$VR_NUMBER <- as.numeric(theVMS$VR_NUMBER)
  theVMS$elapsedDist_m <- theVMS$elapsedTime_min  <- NULL
  
  # theVMS <- readRDS("theVMS.RDS")
  df<-unique(df[df$VR_NUMBER %in% theVMS$VR_NUMBER,])
  #filtered landings, now filter VMS to ensure we have VMS data to join
  theVMS <- theVMS[theVMS$VR_NUMBER %in% df$VR_NUMBER,]
  
  #associate all of the vms date with fishing activity where the positions are between the 'pseudo-start'
  #and the LANDED_DATE
  combined=sqldf("select * 
                 from theVMS v left join df m on 
                 v.VR_NUMBER = m.VR_NUMBER and 
                 v.POSITION_UTC_DATE between m.pseudo_start and m.LANDED_DATE")
  #remove duplicate (VR) column
  combined <- combined[!is.na(combined$LICENCE_ID), !duplicated(colnames(combined))]
  
  if (!is.null(data.dir)){
    get_data_tables(usepkg = usepkg, fn.oracle.username = fn.oracle.username, fn.oracle.password = fn.oracle.password, fn.oracle.dsn = fn.oracle.dsn, 
                    schema = "MARFISSCI", data.dir = data.dir, tables = "MARFLEETS_LIC")
    theseLics <- MARFLEETS_LIC[MARFLEETS_LIC$LICENCE_ID %in% combined$LICENCE_ID, c("LICENCE_ID", "GEAR_CODE","GEAR", "SPECIES")]
    combined <- merge(combined, theseLics, by=c("LICENCE_ID", "GEAR_CODE"), all.x=T)
  }else{
    message("If a data.dir provided, GEAR and Licenced species informaton will be added to the output.")
  }
  combined <- setorder(combined, VR_NUMBER, LICENCE_ID, GEAR_CODE, POSITION_UTC_DATE)  #LICENCE_ID, GEAR_CODE, 
  res<-list()
  res$marf_VMS <- combined
  res$marf_VMS_Segments <- NA
  if(make_segments) {
    these_segs <- Mar.utils::make_segments(combined, objField = "trek",seqField = "POSITION_UTC_DATE", createShp = make_segments_shp, plot=F, filename = "marf_VMS_segs")
    res$marf_VMS_Segments <- these_segs$segments
  }
  return(res)
}