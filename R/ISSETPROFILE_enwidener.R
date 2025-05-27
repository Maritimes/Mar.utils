#' @title ISSETPROFILE_enwidener
#' @description This function reshapes the default ISDB.ISSETPROFILE data such that 
#' each fishing set gets a single record instead of being spread across 4 records. 
#' Values such as AIR_TMP which would have previously been associated with P1 - P4 
#' now become AIR_TMP1 through AIR_TMP4.  Additionally, LATITUDE, LONGITUDE and YEAR
#' fields are added using the first value for each of P1-P4 for each set.
#' @param ISSETPROFILE A data.frame of the structure of ISDB.ISSETPROFILE - having 
#' columns SETDATE, SETTIME, LATITUDE, LONGITUDE, DEPTH, VESSEL_SPEED, AIR_TEMPERATURE, 
#' NET_TEMPERATURE, WATER_TEMPERATURE, BAR_PRESSURE, FISHSET_ID, SET_NO, PNTCD_ID.
#' @return A data.table in the same shape as your SQL view.
#' @export
ISSETPROFILE_enwidener <- function(ISSETPROFILE) {
  if ("DUR_32" %in% colnames(ISSETPROFILE)) {
    return(ISSETPROFILE)
  }
  
  ## 1) to data.table, drop NA codes, cheap fixes
  dt <- data.table::as.data.table(ISSETPROFILE)
  dt <- dt[!is.na(PNTCD_ID) & PNTCD_ID %in% 1:4]
  dt[ , `:=`(
    SETDATE   = as.Date(SETDATE),
    DATE_TIME = as.POSIXct(
      paste(SETDATE, sprintf("%04d", as.numeric(SETTIME))),
      "%Y-%m-%d %H%M", tz = "UTC"
    ),
    LONGITUDE = -LONGITUDE
  )]
  
  ## 2) aggregate → fix –Inf → force levels → dcast → copy
  agg <- dt[ , .(
    DATE_TIME = max(DATE_TIME, na.rm=TRUE),
    LAT       = max(LATITUDE,  na.rm=TRUE),
    LONG      = max(LONGITUDE, na.rm=TRUE),
    DEP       = max(DEPTH,     na.rm=TRUE),
    VESS_SPD  = max(VESSEL_SPEED,   na.rm=TRUE),
    AIR_TMP   = max(AIR_TEMPERATURE,   na.rm=TRUE),
    NET_TMP   = max(NET_TEMPERATURE,   na.rm=TRUE),
    WAT_TMP   = max(WATER_TEMPERATURE, na.rm=TRUE),
    BAR_PRESS = max(BAR_PRESSURE,      na.rm=TRUE)
  ), by=.(FISHSET_ID, SET_NO, PNTCD_ID)]
  
  for (nm in c("LAT","LONG","DEP","VESS_SPD","AIR_TMP",
               "NET_TMP","WAT_TMP","BAR_PRESS")) {
    idx <- which(agg[[nm]] == -Inf)
    if (length(idx)) data.table::set(agg, i=idx, j=nm, value=NA_real_)
  }
  
  agg[ , PNTCD_ID := factor(PNTCD_ID, levels=1:4)]
  
  wide <- data.table::dcast(
    agg,
    FISHSET_ID + SET_NO ~ PNTCD_ID,
    value.var = c("DATE_TIME","LAT","LONG","DEP", "VESS_SPD","AIR_TMP","NET_TMP","WAT_TMP","BAR_PRESS"),
    sep  = "",
    fill = NA
  )
  wide <- data.table::copy(wide)
  
  ## 3) derived columns added one‐by‐one via set()
  
  # durations
  data.table::set(wide, j="DUR_32", value = as.integer(difftime(wide$DATE_TIME3, wide$DATE_TIME2, units="mins")))
  data.table::set(wide, j="DUR_41", value = as.integer(difftime(wide$DATE_TIME4, wide$DATE_TIME1, units="mins")))
  
  # distances, rounded to 3 decimals
  n <- nrow(wide)
  dist32 <- rep(NA_real_, n)
  ok32   <- with(wide, !is.na(LAT2)&!is.na(LONG2)&!is.na(LAT3)&!is.na(LONG3))
  if (any(ok32)) {
    la2 <- wide$LAT2[ok32]; lo2 <- wide$LONG2[ok32]
    la3 <- wide$LAT3[ok32]; lo3 <- wide$LONG3[ok32]
    R   <- 6371
    dlat <- (la3 - la2)*pi/180; dlon <- (lo3 - lo2)*pi/180
    a    <- sin(dlat/2)^2 + cos(la2*pi/180)*cos(la3*pi/180)*sin(dlon/2)^2
    dist32[ok32] <- round(2*R*atan2(sqrt(a), sqrt(1-a)), 3)
  }
  data.table::set(wide, j="DISTNM_32", value=dist32)
  
  dist41 <- rep(NA_real_, n)
  ok41   <- with(wide, !is.na(LAT1)&!is.na(LONG1)&!is.na(LAT4)&!is.na(LONG4))
  if (any(ok41)) {
    la1 <- wide$LAT1[ok41]; lo1 <- wide$LONG1[ok41]
    la4 <- wide$LAT4[ok41]; lo4 <- wide$LONG4[ok41]
    R    <- 6371
    dlat <- (la4 - la1)*pi/180; dlon <- (lo4 - lo1)*pi/180
    a    <- sin(dlat/2)^2 + cos(la1*pi/180)*cos(la4*pi/180)*sin(dlon/2)^2
    dist41[ok41] <- round(2*R*atan2(sqrt(a), sqrt(1-a)), 3)
  }
  data.table::set(wide, j="DISTNM_41", value=dist41)
  
  # first‐valid latitude
  lat_final <- with(wide,
                    ifelse(is.na(LAT1)|LAT1==0,
                           ifelse(is.na(LAT2)|LAT2==0,
                                  ifelse(is.na(LAT3)|LAT3==0, LAT4, LAT3),
                                  LAT2),
                           LAT1
                    )
  )
  data.table::set(wide, j="LATITUDE", value=lat_final)
  
  # first‐valid longitude
  lon_final <- with(wide,
                    ifelse(is.na(LONG1)|LONG1==0,
                           ifelse(is.na(LONG2)|LONG2==0,
                                  ifelse(is.na(LONG3)|LONG3==0, LONG4, LONG3),
                                  LONG2),
                           LONG1
                    )
  )
  data.table::set(wide, j="LONGITUDE", value=lon_final)
  
  # YEAR = year of first non‐NA DATE_TIME*
  first_dt <- data.table::fcoalesce(
    wide$DATE_TIME1, wide$DATE_TIME2,
    wide$DATE_TIME3, wide$DATE_TIME4
  )
  data.table::set(wide, j="YEAR", value = lubridate::year(first_dt))
  
  # reorder
  data.table::setcolorder(wide, c(
    "FISHSET_ID","SET_NO",
    paste0("DATE_TIME",1:4),
    "DUR_32","DUR_41","DISTNM_32","DISTNM_41",
    paste0("LAT",1:4), paste0("LONG",1:4),
    paste0("DEP",1:4),
    paste0("VESS_SPD",1:4),
    paste0("AIR_TMP",1:4),
    paste0("NET_TMP",1:4),
    paste0("WAT_TMP",1:4),
    paste0("BAR_PRESS",1:4),
    "LATITUDE","LONGITUDE","YEAR"
  ))
  
  # convert to base data.frame in‐place
  data.table::setDF(wide)
  wide
}