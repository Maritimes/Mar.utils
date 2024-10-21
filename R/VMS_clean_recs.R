#' @title VMS_clean_recs
#' @description This function takes raw VMS data (i.e. a dataframe having sequential coordinates and 
#' times) and cleans it.  Data is cleaned by removing records that are no more than \code{minDist_m}
#' from the previous position.  Additionally, resultant data is grouped into "treks" which can be 
#' thought of as discrete forays by a vessel.  A new trek occurs when the time between subsequent 
#' points for a vessel exceeds \code{maxBreak_mins}, which by default is 24 hours (1440 mins).  
#' @param df default is \code{NULL}.  This is the dataframe to be processed.  It 
#' should have coordinates in decimal degrees and they should be in fields 
#' called "LATITUDE" and "LONGITUDE".  It also needs a field with the time 
#' associated with each position, i.e. the  \code{timeField}.
#' @param objField default is \code{VR_NUMBER}. This is a field identifying which 
#' points are associated withe eachother. 
#' @param timeField default is \code{POSITION_UTC_DATE}. This is the field which 
#' will be used to calculate the time differences between records.  
#' @param lat.field the default is \code{"LATITUDE"}. the name of the field holding latitude values 
#' (in decimal degrees)
#' @param lon.field the default is \code{"LONGITUDE"}.  the name of the field holding longitude 
#' values (in decimal degrees)
#' @param minDist_m the default is \code{50}. This is the minimum distance (m) a vessel must move 
#' from it's last position in order for the record to be kept.  This should be greater than zero to 
#' avoid records for vessels sitting in port. 
#' @param maxBreak_mins the default is \code{1440}(1440 minutes corresponds with 24 hours) .  This 
#' is the maximum time (in mins) that is allowed between positions before a new "trek" is created. 
#' @return a dataframe with an additional "trek" column identifying a number of
#' discrete paths for each unique value of \code{objField}.
#' @param minKnots  default is \code{NULL}.  This is the minimum vessel speed that should be include 
#' in the output(also see \code{speedField})
#' @param maxKnots default is \code{NULL}.  This is the maximum vessel speed that should be include 
#' in the output (also see \code{speedField})
#' @param speedField default is \code{"calc"}.  Valid values are "calc" or any field that exists 
#' within your df
#' @param dropOrphans default is \code{TRUE}.  VMS data is primarily used for tracks, but during the 
#' filtering process, it's possible to end up with singular positions.  By default, these lone 
#' positions will be discarded.
#' @import data.table
#' @family vms
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#' @note If a field called  "UPDATE_DATE" exists, it will be used to "break ties" in the case of 
#' duplicate records (i.e. the more recent record will is retained.
VMS_clean_recs <- function(df=NULL,lat.field= "LATITUDE",lon.field="LONGITUDE",
                            objField = "VR_NUMBER", timeField ="POSITION_UTC_DATE",
                            minDist_m = 50, maxBreak_mins = 1440,
                            minKnots = NULL, maxKnots = NULL, speedField= "calc", dropOrphans =T){
  .data <- lag_lon <- lag_lat <- change <- trek <- distCalc_m<- timeCalc_min<- UPDATE_DATE <- NA
  if (!inherits(df[[timeField]], "POSIXct")) stop(paste(timeField, "must be of class POSIXct"))
  if (tolower(speedField)=="calc"){
    theSpeedField<-"SPEED_CALC_KTS"
  } else if (speedField %in% names(df)){
    theSpeedField <- speedField
  }else{
    stop("speedField should either be 'calc' or an appropriate field from your data")
  }

  
  calculate_dist_time <- function(df, lat.field, lon.field, objField, timeField) {
    # Function to calculate distance and time between successive points
    df <- df %>%
      dplyr::arrange(.data[[objField]], .data[[timeField]]) %>%
      dplyr::group_by(.data[[objField]]) %>%
      dplyr::mutate(
        lag_lon = dplyr::lag(.data[[lon.field]], default = dplyr::first(.data[[lon.field]])),
        lag_lat = dplyr::lag(.data[[lat.field]], default = dplyr::first(.data[[lat.field]])),
        distCalc_m = dplyr::if_else(dplyr::row_number() == 1, 0, round(geosphere::distGeo(cbind(lag_lon, lag_lat), cbind(.data[[lon.field]], .data[[lat.field]])))),
        timeCalc_min = dplyr::if_else(dplyr::row_number() == 1, 0, round(as.numeric(difftime(.data[[timeField]], dplyr::lag(.data[[timeField]], default = first(.data[[timeField]])), units = "min")),2))
      ) %>% 
      dplyr::ungroup() %>%
      dplyr::select(-lag_lon, -lag_lat) %>%
      dplyr::arrange(.data[[objField]], .data[[timeField]])
    return(df)
  }
  
  trekFinder <- function(df, final=FALSE){
    #in this we'll be dropping any records where only a single position was found for a trek 
    #generate treks - new trek everytime more than maxBreak_mins between positions
    
    df <- df %>%
      dplyr::arrange(.data[[objField]], .data[[timeField]]) %>%  # Ensure the data is sorted
      dplyr::mutate(change = .data[[objField]] != dplyr::lag(.data[[objField]], default = .data[[objField]][1]) | timeCalc_min > maxBreak_mins) %>%
      dplyr::mutate(trek = cumsum(change)) %>%
      dplyr::group_by(trek) 
    
    if(dropOrphans) {
      df <- df %>% dplyr::filter(dplyr::n() > 1)
    }
    
    df <- df %>%
      dplyr::ungroup() %>%
      dplyr::select(-change)  %>%
      dplyr::arrange(.data[[objField]], .data[[timeField]]) %>%
      dplyr::group_by(trek) %>%
      dplyr::mutate(distCalc_m = dplyr::if_else(dplyr::row_number() == 1, 0, distCalc_m),
             timeCalc_min = dplyr::if_else(dplyr::row_number() == 1, 0, timeCalc_min)) 
    
    if(dropOrphans) {
      df <- df %>% dplyr::filter(dplyr::n() > 1)
    }
    
    df <- df %>%
      dplyr::ungroup() %>%
      dplyr::arrange(.data[[objField]], .data[[timeField]]) %>%
      dplyr::mutate(change = .data[[objField]] != dplyr::lag(.data[[objField]], default = .data[[objField]][1]) | (distCalc_m == 0 & timeCalc_min == 0)) %>%
      dplyr::mutate(trek = cumsum(change)) %>%
      dplyr::select(-change)  %>%
      dplyr::arrange(.data[[objField]], .data[[timeField]]) 
    return(df)
  }
  
  speedFinder <- function(df){
    #calculate the speed in knots
    df <- df %>%
      dplyr::mutate(SPEED_CALC_KTS = dplyr::if_else(distCalc_m == 0 & timeCalc_min == 0, 0, round((distCalc_m / 1852) / (timeCalc_min / 60),2))) %>% 
      dplyr::arrange(.data[[objField]], .data[[timeField]]) 
    
    if ((!is.null(minKnots)|!is.null(maxKnots))){
      if (!is.null(minKnots)) df<-df[df[[theSpeedField]]>=minKnots,]
      if (!is.null(maxKnots)) df<-df[df[[theSpeedField]]<=maxKnots,]
      df$SPEED_CALC_KTS <- NULL
      if(dropOrphans){
        df <- df %>% 
          dplyr::group_by(trek) %>% 
          dplyr::filter(dplyr::n() > 1)%>%
          dplyr::ungroup()
      }
    }
    return(df)
  }
  
  filterRecs <- function(df){
    df <- calculate_dist_time(df, lat.field, lon.field, objField, timeField)
    #retain records where the distance from the previous position is >= minDist (i.e 50m) AND where
    #timeCalc_min and distCalc_m are both 0 (i.e. first position of a trek)
    df <- df %>%
      dplyr::filter(distCalc_m >= minDist_m | (distCalc_m == 0 & timeCalc_min == 0)) %>%
      dplyr::select(-distCalc_m, -timeCalc_min)
    return(df)
  }
  
  initialTidy <- function(df){
    #drop invalid coords
    df <- df %>%
      dplyr::filter(!(abs(df[[lon.field]]) < 1 & abs(df[[lat.field]]) < 1),
             df[[lon.field]] >= -180, df[[lon.field]] <= 180,
             df[[lat.field]] >= -90, df[[lat.field]] <= 90)
    
    if ("UPDATE_DATE" %in% names(df)) {
      if (!inherits(df$UPDATE_DATE, "POSIXct")) warning(paste("If 'UPDATE_DATE' exists within the data, it should be of class POSIXct so it can assist with cleaning.  For now, those cleaing steps will be skipped"))
      df <- df %>%
        dplyr::arrange(.data[[objField]], .data[[timeField]], UPDATE_DATE) %>%
        dplyr::group_by(.data[[objField]], .data[[timeField]]) %>%
        dplyr::slice(dplyr::n()) %>%
        dplyr::ungroup()  %>%
        dplyr::arrange(.data[[objField]], .data[[timeField]]) 
    }  
    
    #cases existed in test data where a single time was duplicated for a single VR.  
    #Keep only 1 of these times.  I'm not aware of a way to identify which would be the better record 
    #to retain
    df <- df %>%
      dplyr::arrange(.data[[objField]], .data[[timeField]]) %>%
      dplyr::distinct(.data[[objField]], .data[[timeField]], .keep_all = TRUE)
    
    return(df)
  }
  
  df <- initialTidy(df)
  
  nrow_ <- nrow(df)
  while(TRUE) {
    df <- filterRecs(df)
    if(nrow(df) == nrow_) break
    nrow_ <- nrow(df)
  }
  
  df <- calculate_dist_time(df, lat.field, lon.field, objField, timeField)
  
  df <- trekFinder(df, final=T)
  
  df <- speedFinder(df)
  
  df <- df %>% as.data.frame()
  return(df)
  
  # if(!is.null(minKnots))df<-df[df$KNOTS_CALC>=minKnots,]
  # if(!is.null(maxKnots))df<-df[df$KNOTS_CALC<=maxKnots,]
}
