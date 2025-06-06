#' @title set_select
#' @description This function randomly selects stations within particular strata.  A specified 
#' number of stations of 3 different categories can be selected, and the minimum distance required 
#' between the stations can be specified.  Additional details about each set can be added (such as 
#' which NAFO zones or marine protected area a set falls in) can be added through provision of sf 
#' files to the parameters \code{addExtData1_sf} and/or \code{addExtData2_sf}.   Additionally, 
#' certain areas can be set to exclude stations the inclusion of an additional sf object.
#' @param stationData default is \code{NULL}.  This is either 1) the path to a csv file or 2) an
#' existing r dataframe containing the all of the strata for which stations should be generated .  
#' In addition to the strata, this file must also include the fields  \code{"PRIMARY"}, 
#' \code{"SECONDARY"}, and \code{"ALTERNATE"}.  These three fields should contain integers 
#' corresponding with how many of that type of station should be generated.  If no stations of a 
#' particular type should be generated, a value of NA should be present. Strata where \code{PRIMARY} 
#' is set to NA will not have any stations generated. Below is an example of how this file might 
#' look:
#' > head(stationFile)
#'   STRATUM PRIMARY ALTERNATE SECONDARY
#'       5Z1       6         3         4
#'       5Z2      11         6        NA
#'       5Z3       5         2         2
#'       5Z4       4         2         2
#'
#' @param stationDataField default is \code{"STRATUM"}.  This is just the name of the field 
#' that contains the strata.  For the example above, this would be "STRATUM".
#' @param strata_sf default is \code{NULL}. This should point to an sf object the strata to use. 
#' This object must have a field that has values identical to those found in  the 
#' \code{"stationDataField"} within the \code{"stationData"} file.
#' @param strataField default is \code{NULL}.  This is the name of the field within 
#' \code{strata_sf} that contains the identifier for the strata.  Continuing with the example above,
#' this field would contain values including "5Z1", "5Z2", "5Z3" and "5Z4".
#' @param localCRS default is \code{2961}.  In order to create sampling stations, the function 
#' reprojects any spatial data to a locally appropriate projection.  The default value is UTM Zone 
#' 20N, and is appropriate only for Maritimes data.  Any valid CRS can be entered here, and should 
#' be appropriate for your data.  
#' @param minDistNM default is \code{4}. This is the minimum required distance between the sets.  By
#' default, sets will be no closer than 4 nautical miles from each other.
#' @param avoid_sf default is \code{NULL}. This is and sf object containing polygons of the areas 
#' where stations should not be located.   For example, one might populate a file with areas known 
#' to contain unexploded ordinance, or areas where bottom contact  is forbidden.  No stations will 
#' be generated where polygons exist in this file.
#' @param addExtData1_sf default is \code{NULL}.  If additional information should be added to 
#' the output for each set, an sf object can be provided here.  For example, one might provide 
#' a file of NAFO zones or special fishing areas.
#' @param addExtDataFields1 default is \code{NULL}.  If a value is provided to 
#' \code{addExtData1_sf}, this should correspond with one or more fields within that file.  If a 
#' set falls within a particular polygon, the value for that polygon from these fields will be 
#' provided as part of the output.
#' @param addExtData2_sf default is \code{NULL}.  This is identical to addExtData1_sf, but allows 
#' for including information from an additional sf object.
#' @param addExtDataFields2 default is \code{NULL}.  If a value is provided to 
#' \code{addExtData2_sf}, this should correspond with one or more fields within that file.  If a 
#' set falls within a particular polygon, the value for that polygon from these fields will be 
#' provided as part of the output.
#' @param outName This is the name of the output file excel that will be generated and/or the name of a 
#' layer withing a gpkg file.
#' @param writexls  default is \code{TRUE}. Write the results to an excel file in your working 
#' directory?
#' @param writegpkg  default is \code{TRUE}. Write the results to a spatial gpkg file in your 
#'  working directory? (for use in a)
#' @param tryXTimes default is \code{100}  By default, the script will make this many attempts to 
#' fit the requested number of stations into each strata. 
#' @examples \dontrun{
#' Spring_4X_2025 <- set_select(stationData = "c:/2025/Spring_4X_RM.csv",
#'                             outName =  "Spring_4X_2025",
#'                             stationDataField = "STRATUM",
#'                             strata_sf = Mar.data::Strata_Mar_sf, strataField = "StrataID", 
#'                             addExtData1_sf = Mar.data::NAFOSubunits_sf, addExtDataFields1 = "NAFO",
#'                             addExtData2_sf = oceans_areas_sf, 
#'                             addExtDataFields2 = c("NAME_E","ZONE_E"), 
#'                             avoid_sf = this_avoid_sf)
#' 
#' 
#' Georges_5Z_2025 <- set_select(stationData = "c:/2025/Georges_5Z_RM.csv.csv",
#'                              outName =  "Georges_5Z_2025",
#'                              stationDataField = "STRATUM",
#'                              strata_sf = Mar.data::Strata_Mar_sf, strataField = "StrataID", 
#'                              addExtData1_sf = Mar.data::NAFOSubunits_sf, addExtDataFields1 = "NAFO",
#'                              addExtData2_sf = oceans_areas_sf, 
#'                              addExtDataFields2 = c("NAME_E","ZONE_E"), 
#'                              avoid_sf = this_avoid_sf)
#'                        }
#' @family surveys
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export

set_select <- function(
    stationData = NULL, stationDataField = NULL,
    strata_sf = NULL, strataField = NULL,
    addExtData1_sf = NULL, addExtDataFields1 =NULL,
    addExtData2_sf=NULL, addExtDataFields2 =NULL,
    avoid_sf = NULL,
    outName = NULL,
    writexls = TRUE,
    writegpkg =TRUE,
    localCRS = 2961,
    minDistNM = 4,
    tryXTimes = 100){
  timestamp <- format(Sys.time(),"%Y%m%d_%H%M")
  TYPE <- LABEL <- polygon_ <- NA 
  localCRS_ <- localCRS
  if (is.character(stationData) && file.exists(stationData)){
    stationData_ <- utils::read.csv(stationData)
  }else{
    stationData_ <- stationData
  }
  stationDataField_ <- stationDataField
  strata_sf_ <- strata_sf |> sf::st_transform(crs = localCRS_) 
  strataField_ <- strataField
  #referencing fields via variables is annoying, so add a field of a known name to each
  stationData_$filterField_ <- stationData_[,stationDataField]
  strata_sf_$filterField_ <- sf::st_drop_geometry(strata_sf_[, strataField_])[,1]
  #delete the user-selected fields so they don't  cause issues with merging
  stationData_[,stationDataField] <- NULL
  strata_sf_[, stationDataField_]<- NULL
  
  #buffer is in meters, and is 1/2 the min distance
  buffSize <- (minDistNM * 1852)
  
  assignStrata <- function(df = NULL, type = "PRIMARY", n_sets=NULL){
    if (nrow(df[which(is.na(df$TYPE)),])==n_sets){
      df[which(is.na(df$TYPE)),"TYPE"]<-type
    }else{
      df[sample(which(is.na(df$TYPE)),n_sets, replace = F),"TYPE"]<-type
    }
    return(df)
  }
  convert.dd.dddd<-function(x){
    dat<-data.frame(ddmm.mm=NA,dd.dddd=x)
    
    #degrees-minutes-seconds -> degrees-minutes
    ddmmss<-dat$dd.dddd[!is.na(dat$dd.dddd)&abs(dat$dd.dddd)>9000]
    ddmm.ss<-ddmmss/100
    ddmm<-trunc(ddmm.ss)
    ss<-(ddmm.ss-ddmm)*100
    dat$ddmm.mm[!is.na(dat$dd.dddd)&abs(dat$dd.dddd)>9000]<-ddmm+ss/60
    
    #degrees-minutes -> degrees-minutes
    dat$ddmm.mm[!is.na(dat$dd.dddd)&abs(dat$dd.dddd)>90&abs(dat$dd.dddd)<9000]<-dat$dd.dddd[!is.na(dat$dd.dddd)&abs(dat$dd.dddd)>90&abs(dat$dd.dddd)<9000]
    
    #degrees -> degrees-minutes
    dd.dddd<-dat$dd.dddd[!is.na(dat$dd.dddd)&abs(dat$dd.dddd)<90]
    dd<-trunc(dd.dddd)
    mm.mm<-(dd.dddd-dd)*60
    dat$ddmm.mm[!is.na(dat$dd.dddd)&abs(dat$dd.dddd)<90]<-dd*100+mm.mm
    
    return(dat$ddmm.mm)
  }
  
  # Clips the areas of x out of y
  # st_erase from https://bit.ly/3WuRj1b
  # st_erase <- function(x, y) sf::st_difference(x, sf::st_union(sf::st_combine(y)))
  st_erase = function(x, y) {
    sf::st_difference(
      sf::st_geometry(x) |> sf::st_buffer(0),
      sf::st_union(sf::st_combine(sf::st_geometry(y))) |> sf::st_buffer(0)
    )
  }
  # 
  #handle submitted station data - replace "NA" and 0 with NA, and convert fields to numeric/character as appropriate
  stationData_[, setdiff(names(stationData_), "filterField_")] <- lapply(stationData_[, setdiff(names(stationData_), "filterField_")], function(x) {
    ifelse(x %in% c(0, "NA"), NA, x)
  })
  
  stationData_[] <- lapply(stationData_, function(x) utils::type.convert(as.character(x), as.is = TRUE))
  stationData_ <- stationData_[!is.na(stationData_$PRIMARY),]
  stationData_$TOT <- rowSums(stationData_[,c("PRIMARY", "SECONDARY", "ALTERNATE")], na.rm = T)
  #create filtered version of strata file of only those strata present in the stationData_ csv and add the stationData_ into the sf
  filtStrata <- strata_sf_[strata_sf_$filterField_ %in% stationData_$filterField_,]
  filtStrata <- merge(filtStrata, stationData_, all.x=T, by.x="filterField_", by.y="filterField_")
  filtStrata <- sf::st_set_geometry(filtStrata, sf::st_geometry(filtStrata))

  if(!is.null(avoid_sf)){
    avoid_sf <- avoid_sf |> sf::st_transform(crs = localCRS_)
    avoid_sf <- sf::st_set_geometry(avoid_sf, sf::st_geometry(avoid_sf))
    overlapping_areas <- sf::st_intersection(avoid_sf, filtStrata)
    
    filtStrata_sp <- methods::as(filtStrata, "Spatial")
    filtStrata_sp <- cleangeo::clgeo_Clean(filtStrata_sp)
    filtStrata <- sf::st_as_sf(filtStrata_sp)

    tt <- st_erase(filtStrata, overlapping_areas)
    filtStrata <- sf::st_sf(data = sf::st_drop_geometry(filtStrata), geometry = tt)
    names(filtStrata) <- gsub("data.", "", names(filtStrata))
    # filtStrata <- sf::st_difference(filtStrata, avoid_sf)
  }

  allStrat <- unique(sf::st_drop_geometry(filtStrata$filterField_))
  stations <-list()
  failed = FALSE
  failedStrata <- c()
  for (s in 1:length(allStrat)){ 
    x = filtStrata[filtStrata$filterField_==allStrat[s],]
    x.owin <- spatstat.geom::as.owin(x)
    polySet = tryCatch(
      {
        suppressWarnings(sf::st_as_sf(spatstat.random::rSSI(r = buffSize, n = x$TOT[1], win = x.owin, giveup = tryXTimes)))
      },
      error=function(cond){
        return(-1)
      }
    )
    
    if (any(class(polySet)=="numeric") | nrow(polySet[polySet$label == "point",])< x$TOT[1]){
      failedStrata <- c(failedStrata, allStrat[s])
      failed <- TRUE
      next
    }
    if(!failed){
      polySet <- polySet[polySet$label=="point",]
      polySet <- sf::st_set_crs(polySet, localCRS_)
      stations[[allStrat[s]]] <- sf::st_as_sf(polySet)
      stations[[allStrat[s]]]$polygon_ <- allStrat[s]
      
      stations[[allStrat[s]]]<- merge(stations[[allStrat[s]]], sf::st_drop_geometry(x), by.x= "polygon_", by.y="filterField_")
      stations[[allStrat[s]]]$TYPE <- NA
      n_prim <- stations[[allStrat[s]]]$PRIMARY[1]
      n_alt  <- stations[[allStrat[s]]]$ALTERNATE[1]
      n_sec  <- stations[[allStrat[s]]]$SECONDARY[1]
      n_tot  <- stations[[allStrat[s]]]$TOT[1]
      
      if (!is.na(n_prim)){
        stations[[allStrat[s]]] <- assignStrata(df= stations[[allStrat[s]]], 
                                                type = "PRIMARY", 
                                                n_sets = n_prim)
      }
      if (!is.na(n_alt)){
        stations[[allStrat[s]]] <- assignStrata(df= stations[[allStrat[s]]], 
                                                type = "ALTERNATE", 
                                                n_sets = n_alt)
      }     
      if (!is.na(n_sec)){
        stations[[allStrat[s]]] <- assignStrata(df= stations[[allStrat[s]]], 
                                                type = "SECONDARY", 
                                                n_sets = n_sec)
      }   
    }
  } 
  
  if(length(failedStrata)>0){
    stop("\nDespite ", tryXTimes, " attempts, the strata below appear to be too small to sufficiently place the requested number of stations ", minDistNM, " NM apart:",
         "\n\t", paste(failedStrata, collapse = ", ", sep = ", "),
         "\nPlease review your stationData file and ensure that the number of stations is reasonable for the area of the strata")  
  }
  stations <- do.call(rbind, stations)
  stations <- sf::st_transform(x = stations, crs = 4326) 
  # if (!"geometry" %in% names(stations) & "geom" %in% names(stations)) stations$geometry <- stations$geom
  stations <- cbind(stations, round(sf::st_coordinates(stations$geom),6))
  colnames(stations)[colnames(stations)=="X"] <- "LON_DD"
  colnames(stations)[colnames(stations)=="Y"] <- "LAT_DD"
  stations$LON_DDMM <- convert.dd.dddd(stations$LON_DD)
  stations$LAT_DDMM <- convert.dd.dddd(stations$LAT_DD)
  stations <- stations[,c("polygon_","TYPE","LON_DD", "LAT_DD", "LON_DDMM", "LAT_DDMM")]
  
  # apply station names
  # primaries     1 - 299
  # secondaries 501 - 699
  # alternates  901 - 1099
  stations <- stations |> 
    dplyr::arrange(TYPE,polygon_) |> 
    dplyr::group_by(TYPE) |> 
    dplyr::mutate(LABEL = dplyr::row_number()) |> 
    dplyr::mutate(LABEL = ifelse(TYPE == "SECONDARY", LABEL+500, 
                                 ifelse(TYPE == "ALTERNATE", LABEL+900, LABEL))) |> 
    dplyr::ungroup()|> 
    dplyr::arrange(polygon_,LABEL)
  
  stations <- stations[!is.na(stations$LABEL),]
  
  
  if (!is.null(addExtData1_sf)){
    moreAreas1<- sf::st_drop_geometry(stations)
    moreAreas1 <- moreAreas1[,c("LABEL","LAT_DD", "LON_DD")]
    ext1 <- addExtData1_sf |> sf::st_transform(crs = localCRS_) 
    for (i in 1:length(addExtDataFields1)){
      moreAreas1 <- Mar.utils::identify_area(moreAreas1, lat.field = "LAT_DD", lon.field = "LON_DD", agg.poly.shp = ext1, agg.poly.field = addExtDataFields1[i])
    }
    moreAreas1[moreAreas1=="<outside known areas>"]<-NA
    moreAreas1$LAT_DD <- moreAreas1$LON_DD <- NULL
    stations <- merge(stations, moreAreas1, by="LABEL")
  }
  if (!is.null(addExtData2_sf)){
    moreAreas2 <- sf::st_drop_geometry(stations)
    moreAreas2 <- moreAreas2[,c("LABEL","LAT_DD", "LON_DD")]
    ext2 <- addExtData2_sf |> sf::st_transform(crs = localCRS_) 
    for (n in 1:length(addExtDataFields2)){
      moreAreas2 <- Mar.utils::identify_area(moreAreas2, lat.field = "LAT_DD", lon.field = "LON_DD", agg.poly.shp = ext2, agg.poly.field = addExtDataFields2[n] )
    }
    moreAreas2[moreAreas2=="<outside known areas>"]<-NA
    moreAreas2$LAT_DD <- moreAreas2$LON_DD <- NULL
    stations <- merge(stations, moreAreas2, by="LABEL")
  }
  

  if (!is.null(outName)){
    outFile<- paste0(outName, "_setSelect_",timestamp)
  }else{
    outFile<- paste0("setSelect_",timestamp)
  }
  if (writexls) xlsx::write.xlsx2(as.data.frame(stations |> sf::st_drop_geometry()) , paste0(outFile,".xlsx"), sheetName = "setSelect", col.names = TRUE, row.names = FALSE, append = FALSE)
  if (writegpkg) stations <- sf::st_write(stations, dsn = paste0(getwd(), "/setSelect.gpkg"), outFile, append = F, delete.dsn=T)
  if (writexls | writegpkg) message("wrote excel and/or gpkg files to ", getwd())
  return(stations)
}

