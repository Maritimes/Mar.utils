#' @title df_to_sf
#' @description This function will accept a dataframe and turn it into an sf object of class 
#' "POLYGON", "LINESTRING" or "POINTS".  It can handle PBSMapping polysets or any appropriately 
#' structured dataframe.
#' @param df the default is \code{NULL}.  This is a dataframe that has coordinates holding latitudes
#' and longtitudes in decimal degrees.
#' @param lat.field the default is \code{"X"}. the name of the field holding latitude values
#' (in decimal degrees)
#' @param lon.field the default is \code{"Y"}.  the name of the field holding longitude
#' values (in decimal degrees)
#' @param primary.object.field the default is \code{"PID"}.  If polygons or lines are specified,
#' this field is used to identify discrete objects.
#' @param secondary.object.field the default is \code{"SID"}. Complex polygons need this additional 
#' identifier in order to create objects such as polygons with holes.
#' @param order.field the default is \code{"POS"}. This parameter specifies the field that controls 
#' the order points should be joined.  In order to specifiy a "hole", the order field for the c
#' oordinates specifying the hole should be descending (rather than ascending).
#' @param type the default is \code{"poly"}, but it can also be \code{"line"} or \code{"points"}
#' @return an sf object
#' @note this function was modified from 
#' https://github.com/Mar-scal/Assessment_fns/blob/master/Maps/pbs_2_sf.R 
#' @family general_use
#' @importFrom dplyr %>%
#' @author  Dave Keith \email{Dave.Keith@@dfo-mpo.gc.ca}, Freya Keyser \email{Freya.Keyser@@dfo-mpo.gc.ca} and adapted by Mike McMahon \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
df_to_sf <- function(df = NULL, 
                     lat.field = "Y", 
                     lon.field = "X",
                     primary.object.field = "PID",
                     secondary.object.field = "SID",
                     order.field = "POS",
                     type = "poly") {
  if(nrow(df[!complete.cases(df[ , c(lat.field, lon.field)]), ])>0){
    ndrop <- nrow(df[!complete.cases(df[ , c(lat.field, lon.field)]), ])
    message("Dropping ",ndrop, " records that are missing latitude, longitude, or both")
    df <- df[complete.cases(df[ , c(lat.field, lon.field)]), ]
  }

  if (!(all(df[,lon.field] >= -180) & all(df[,lon.field] <= 180) & all(df[,lat.field] >= -90)& all(df[,lat.field] <= 90))){
    stop("Submitted df must be unprojected geographic coordinates (lon.field between -180 and 180, lat.field between -90 and 90, and there should be no NA values in the coordinates")
  }
  #capture initial state of sf_use_s2 prior to setting it to FALSE for this function
  initState <- getOption("sf_use_s2")
  sf::sf_use_s2(FALSE)
  if (type =="points" | "EID" %in% names(df)){
    return(sf::st_as_sf(df, coords = c(lon.field, lat.field), crs = 4326, agr = "constant"))
  }
  #If no secondary ID field exists, just create a fake one with a single value to simplify ensuing code
  if(!secondary.object.field %in% names(df)) {
    df$SID <- 1
    secondary.object.field = "SID"
  }
  
  #secondary field gets dropped since it's not visible as part of the resultant lines/polygons
  dfData <- unique(df[,!names(df)%in% c(lat.field, lon.field, secondary.object.field, order.field, "hole"), drop = FALSE])
  
  pids <- NULL
  
  pb <- txtProgressBar(style = 3)
  for(i in 1:length(unique(df[,primary.object.field]))){
    thisData <- dfData[dfData[,primary.object.field]==unique(df[,primary.object.field])[i],, drop = FALSE]
    pid <- df %>%
      filter(PID == unique(df[,primary.object.field])[i])
    sids <- NULL  
    for(j in 1:length(unique(pid[,secondary.object.field]))){
      sid <- pid %>%
        dplyr::filter(SID==unique(pid[,secondary.object.field])[j])
      if(type=="poly"){
        sid$hole <- ifelse(sid[,order.field][1]<sid[,order.field][2],"N","Y")
        sid <- sid %>%
          sf::st_as_sf(coords=c(lon.field, lat.field), crs=4326) %>%
          dplyr::group_by(.data[[primary.object.field]], .data[[secondary.object.field]], hole) %>%
          dplyr::summarize(do_union=F, .groups = 'keep') %>%
          sf::st_cast("POLYGON")
        if(j==1) {
          sids <- rbind(sids, sid)
        }else{
          if(unique(sid$hole)=="Y"){
            sids <- suppressMessages(suppressWarnings(sf::st_difference(sids, sid))) %>%
              dplyr::select(all_of(c(primary.object.field, secondary.object.field, "hole")))
          }else if (unique(sid$hole)=="N"){
            sid <- dplyr::select(sid, all_of(c(primary.object.field, secondary.object.field, "hole")))
            sids <- rbind(sids, sid)
          }else{
            message("This should never happen")
          }
        }
      }else{
        sid <- sid %>%
          sf::st_as_sf(coords=c(lon.field, lat.field), crs=4326) %>%
          dplyr::group_by(.data[[primary.object.field]], .data[[secondary.object.field]]) %>%
          dplyr::summarize(do_union=F, .groups = 'keep') %>%
          sf::st_cast("LINESTRING")
        if(j==1) {
          sids <- rbind(sids, sid)
        }else{
          sid <- dplyr::select(sid, PID, SID)
          sids <- rbind(sids, sid)
        }
      }
    }
    pid <- sf::st_combine(sids) %>% sf::st_sf()
    # bind non-geometry info onto the geometry
    if(!all(names(thisData) %in% names(pid))) pid = cbind(pid, thisData)
    pids <- rbind(pids, pid)
    setTxtProgressBar(txtProgressBar(min = 0, max = length(unique(df[,primary.object.field])), style = 3), i)
  }
  close(pb)
  #set sf_use_s2 back to it's original state, if it existed at all
  if (is.null(initState)){
    .Options$sf_use_s2 <- NULL
  }else{
    .Options$sf_use_s2 <- initState
  }
  return(pids)
}
