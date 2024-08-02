#' @title df_to_sf
#' @description This function will accept a dataframe and turn it into an sf object of class 
#' "POLYGON", "LINESTRING" or "POINTS".  It can handle PBSMapping polysets or any appropriately 
#' structured dataframe.
#' @param df the default is \code{NULL}.  This is a dataframe that has coordinates holding latitudes
#' and longtitudes in decimal degrees.
#' @param lat.field the default is \code{"LATITUDE"}. the name of the field holding latitude values
#' (in decimal degrees)
#' @param lon.field the default is \code{"LONGITUDE"}.  the name of the field holding longitude
#' values (in decimal degrees)
#' @param primary.object.field the default is \code{"PID"}.  If polygons or lines are specified,
#' this field is used to identify discrete objects.
#' @param secondary.object.field the default is \code{"SID"}. Complex polygons need this additional 
#' identifier in order to create objects such as polygons with holes.
#' @param order.field the default is \code{"POS"}. This parameter specifies the field that controls 
#' the order points should be joined.  In order to specifiy a "hole", the order field for the c
#' oordinates specifying the hole should be descending (rather than ascending).
#' @param type the default is \code{"polys"}, but it can also be \code{"lines"} or \code{"points"}
#' @return an sf object
#' @note this function was modified from 
#' https://github.com/Mar-scal/Assessment_fns/blob/master/Maps/pbs_2_sf.R 
#' @family general_use
#' @importFrom dplyr %>%
#' @author  Dave Keith \email{Dave.Keith@@dfo-mpo.gc.ca}, Freya Keyser \email{Freya.Keyser@@dfo-mpo.gc.ca} and adapted by Mike McMahon \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
df_to_sf <- function(df = NULL, 
                     lat.field = "LATITUDE", 
                     lon.field = "LONGITUDE",
                     primary.object.field = "PID",
                     secondary.object.field = "SID",
                     order.field = "POS",
                     type = "polys") {

  hole <- NA
  type <- tolower(type)
  if(nrow(df[!stats::complete.cases(df[ , c(lat.field, lon.field)]), ])>0){
    ndrop <- nrow(df[!stats::complete.cases(df[ , c(lat.field, lon.field)]), ])
    message("Dropping ",ndrop, " records that are missing latitude, longitude, or both")
    df <- df[stats::complete.cases(df[ , c(lat.field, lon.field)]), ]
  }

  if (!(all(df[,lon.field] >= -180) & all(df[,lon.field] <= 180) & all(df[,lat.field] >= -90)& all(df[,lat.field] <= 90))){
    stop("Submitted df must be unprojected geographic coordinates (lon.field between -180 and 180, lat.field between -90 and 90, and there should be no NA values in the coordinates")
  }
  #capture initial state of sf_use_s2 prior to setting it to FALSE for this function
  initState <- getOption("sf_use_s2")
  sf::sf_use_s2(FALSE)
  if (type =="points" | "EID" %in% names(df)){
    df_sf <- sf::st_as_sf(df, coords = c(lon.field, lat.field), crs = 4326, agr = "constant")
    df_sf<- cbind(df_sf, sf::st_coordinates(df_sf$geometry))
    colnames(df_sf)[colnames(df_sf)=="X"] <- lon.field
    colnames(df_sf)[colnames(df_sf)=="Y"] <- lat.field
    return(df_sf)
  }
  #If no secondary ID field exists, just create a fake one with a single value to simplify ensuing code
  if(!secondary.object.field %in% names(df)) {
    df$SID <- 1
    secondary.object.field = "SID"
  }
  
  pids <- NULL
  pb <- utils::txtProgressBar(style = 3)
  
  allPrims <- unique(df[,primary.object.field])
  for(i in 1:length(allPrims)){
    pid <- df[df[[primary.object.field]]== allPrims[i],]
    sids <- NULL  
    if(type=="polys" & nrow(pid)<4)next
    if(type=="lines" & nrow(pid)<2)next
    for(j in 1:length(unique(pid[,secondary.object.field]))){
      sid <- pid[pid[[secondary.object.field]]==unique(pid[,secondary.object.field])[j],]
      if(type=="polys"){
        if(nrow(sid)<4) next
        sid$hole <- ifelse(sid[,order.field][1]<sid[,order.field][2],"N","Y")
        sid <- sid %>%
          sf::st_as_sf(coords=c(lon.field, lat.field), crs=4326) %>%
          dplyr::group_by(!!rlang::sym(primary.object.field), !!rlang::sym(secondary.object.field), hole) %>%
          dplyr::summarize(do_union=F, .groups = 'keep') %>%
          sf::st_cast("POLYGON")
        if(j==1) {
          sids <- rbind(sids, sid)
        }else{
          if(unique(sid$hole)=="Y"){
            sids <- suppressMessages(suppressWarnings(sf::st_difference(sids, sid))) %>%
              dplyr::select(dplyr::all_of(c(primary.object.field, secondary.object.field, "hole")))
          }else if (unique(sid$hole)=="N"){
            sid <- dplyr::select(sid, dplyr::all_of(c(primary.object.field, secondary.object.field, "hole")))
            sids <- rbind(sids, sid)
          }else{
            message("This should never happen")
          }
        }
      }else{
        if(nrow(sid)<2) next
        sid <- sid[with(sid,order(sid[[order.field]])),]
        sid <- sid %>%
          sf::st_as_sf(coords=c(lon.field, lat.field), crs=4326) %>%
          dplyr::group_by(!!rlang::sym(primary.object.field), !!rlang::sym(secondary.object.field)) %>%
          dplyr::summarize(do_union=F, .groups = 'keep') %>%
          sf::st_cast("LINESTRING")
        if(j==1) {
          sids <- rbind(sids, sid)
        }else{
          sid <- dplyr::select(sid, dplyr::all_of(c(primary.object.field, secondary.object.field)))
          sids <- rbind(sids, sid)
        }
      }
    }
    pid <- sf::st_combine(sids) %>% sf::st_sf()
    # bind primary identifier onto the geometry
    pid = cbind(pid, allPrims[i])
    pids <- rbind(pids, pid)
    utils::setTxtProgressBar(utils::txtProgressBar(min = 0, max = length(unique(df[,primary.object.field])), style = 3), i)
  }
  close(pb)
  colnames(pids)[colnames(pids)=="allPrims.i."] <- primary.object.field
  #set sf_use_s2 back to it's original state, if it existed at all
  if (is.null(initState)){
    .Options$sf_use_s2 <- NULL
  }else{
    .Options$sf_use_s2 <- initState
  }
  return(pids)
}
