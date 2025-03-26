#' @title VMS_from_MARFIS
#' @description This function takes a dataframe (with coordinate fields in decimal
#' degrees), and extracts asssociated VMS data
#' @param df a dataframe to be analyzed. 
#' @param cxn A valid Oracle connection object. This parameter allows you to 
#' pass an existing connection, reducing the need to establish a new connection 
#' within the function. If provided, it takes precedence over the connection-
#' related parameters.
#' @param fn.oracle.username Default is \code{'_none_'}. This is your username 
#' for accessing Oracle objects. If you have a value for \code{oracle.username} 
#' stored in your environment (e.g., from an rprofile file), this can be left 
#' out and that value will be used. If a value for this is provided, it will 
#' take priority over your existing value. Deprecated; use \code{cxn} instead.
#' @param fn.oracle.password Default is \code{'_none_'}. This is your password 
#' for accessing Oracle objects. If you have a value for \code{oracle.password} 
#' stored in your environment (e.g., from an rprofile file), this can be left 
#' out and that value will be used. If a value for this is provided, it will 
#' take priority over your existing value. Deprecated; use \code{cxn} instead.
#' @param fn.oracle.dsn Default is \code{'_none_'}. This is your DSN/ODBC 
#' identifier for accessing Oracle objects. If you have a value 
#' for \code{oracle.dsn} stored in your environment (e.g., from an rprofile 
#' file), this can be left out and that value will be used. If a value for this 
#' is provided, it will take priority over your existing value. Deprecated; use 
#' \code{cxn} instead.
#' @param usepkg default is \code{'rodbc'}. This indicates whether the 
#' connection to Oracle should use \code{'rodbc'} or \code{'roracle'} to 
#' connect.  rodbc is slightly easier to setup, but roracle will extract data ~ 
#' 5x faster. Deprecated; use \code{cxn} instead.
#' @param data.dir  the default is \code{NULL}. If you are hoping to load existing data,
#' this folder should contain a data folder containing your rdata files. For this function, only
#' MARFLEETS_LIC will be used, and only to add gear and licenced species onto each record.
#' @param VR_field the default is \code{"VR_NUMBER"}
#' @param LIC_field the default is \code{"LICENCE_ID"}
#' @param GC_field the default is \code{"GEAR_CODE"}
#' @param LANDED_field the default is \code{"LANDED_DATE"}
#' @param look_ahead_days the default is \code{7}
#' @param lat.field the default is \code{NULL}. This is the name of the field holding latitude values 
#' (in decimal degrees)
#' @param lon.field the default is \code{NULL}. This is the name of the field holding longitude 
#' values (in decimal degrees)
#' @param make_segments the default is \code{TRUE}. This indicates whether or not the results should
#' include an sp object of all of the VMS data as lines (in addition to a data frame)
#' @param make_segments_spatial the default is \code{FALSE}. This indicates whether or not a shapefile
#' should be created of all of the VMS data as lines (in addition to a data frame). 
#' \code{make_segments} must be TRUE for shapefiles to be generated. 
#' @import data.table
#' @return a list containing a data.frame called "marf_VMS" of the joined marfis/VMS data, and, 
#' if \code{make_segments} is TRUE, an sp object called "marf_VMS_segments". Additionally, if 
#' make_segments and make_segments_shp are both TRUE, a shapefile will be created in the working 
#' directory.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
VMS_from_MARFIS <- function(df = NULL,
                            cxn = NULL, 
                            fn.oracle.username = "_none_", 
                            fn.oracle.password = "_none_", 
                            fn.oracle.dsn = "_none_",
                            usepkg = 'rodbc',
                            data.dir = NULL,
                            VR_field = "VR_NUMBER",
                            LIC_field = "LICENCE_ID",
                            GC_field = "GEAR_CODE",
                            LANDED_field = "LANDED_DATE",
                            look_ahead_days = 7,
                            lat.field = NULL,
                            lon.field = NULL,
                            make_segments = TRUE,
                            make_segments_spatial = FALSE) {
  deprecationCheck(fn.oracle.username = fn.oracle.username, 
                   fn.oracle.password = fn.oracle.password, 
                   fn.oracle.dsn = fn.oracle.dsn,
                   usepkg = usepkg)
  
  # data.table doesn't like column name references - ensure the col names are known
  VR_NUMBER <- LANDED_DATE <- pseudo_start <- MARFLEETS_LIC <- LICENCE_ID <- GEAR_CODE <- POSITION_UTC_DATE <- NULL
  colnames(df)[colnames(df) == VR_field] <- "VR_NUMBER"
  colnames(df)[colnames(df) == LIC_field] <- "LICENCE_ID"
  colnames(df)[colnames(df) == GC_field] <- "GEAR_CODE"
  colnames(df)[colnames(df) == LANDED_field] <- "LANDED_DATE"
  df <- df[!is.na(df$VR_NUMBER) & !is.na(df$LICENCE_ID) & !is.na(df$GEAR_CODE), ]
  
  bbox <- NA
  if (!is.null(lat.field) && !is.null(lon.field)) {
    df <- unique(df[, c("VR_NUMBER", "LICENCE_ID", "GEAR_CODE", "LANDED_DATE", lat.field, lon.field)])
    bbox <- c(range(df[!is.na(df[lat.field]), lat.field]), 
              range(df[!is.na(df[lon.field]), lon.field]))
  } else {
    df <- unique(df[, c("VR_NUMBER", "LICENCE_ID", "GEAR_CODE", "LANDED_DATE")])
  }
  
  df <- data.table::setDT(df) 
  df <- data.table::setorder(df, VR_NUMBER, LANDED_DATE) 
  df <- df[, pseudo_start := data.table::shift(LANDED_DATE, type = "lag"), by = VR_NUMBER ]
  df <- data.table::setDF(df) 
  df[is.na(df$pseudo_start), "pseudo_start"] <- df[is.na(df$pseudo_start), "LANDED_DATE"] - as.difftime(look_ahead_days, units = "days")
  dateRange <- as.Date(c(min(df$pseudo_start), max(df$LANDED_DATE)))
  vrns <- sort(unique(df$VR_NUMBER))
  message("Starting VMS extraction - this can take a long time - grab a coffee")
  
  if (is.null(cxn)) {
    if (all(is.na(bbox))) {
      theVMS <- VMS_get_recs(fn.oracle.username = fn.oracle.username, 
                             fn.oracle.password = fn.oracle.password, 
                             fn.oracle.dsn = fn.oracle.dsn,
                             usepkg = usepkg,
                             vrnList = vrns,
                             dateStart = as.character(min(dateRange)),
                             dateEnd = as.character(max(dateRange)),  
                             rowNum = 1000000, 
                             quietly = T)
    } else {
      theVMS <- VMS_get_recs(fn.oracle.username = fn.oracle.username, 
                             fn.oracle.password = fn.oracle.password, 
                             fn.oracle.dsn = fn.oracle.dsn,
                             usepkg = usepkg,
                             vrnList = vrns,
                             dateStart = as.character(min(dateRange)),
                             dateEnd = as.character(max(dateRange)),  
                             minLat = bbox[1], maxLat = bbox[2], 
                             minLon = bbox[3], maxLon = bbox[4],
                             rowNum = 1000000, 
                             quietly = T)
    }
  } else {
    if (all(is.na(bbox))) {
      theVMS <- VMS_get_recs(cxn = cxn, 
                             vrnList = vrns,
                             dateStart = as.character(min(dateRange)),
                             dateEnd = as.character(max(dateRange)),  
                             rowNum = 1000000, 
                             quietly = T)
    } else {
      theVMS <- VMS_get_recs(cxn = cxn, 
                             vrnList = vrns,
                             dateStart = as.character(min(dateRange)),
                             dateEnd = as.character(max(dateRange)),  
                             minLat = bbox[1], maxLat = bbox[2], 
                             minLon = bbox[3], maxLon = bbox[4],
                             rowNum = 1000000, 
                             quietly = T)
    }
  }
  
  if (nrow(theVMS) == 1000000) warning("Hit extraction row limit")
  theVMS <- VMS_clean_recs(df = theVMS)
  theVMS$VR_NUMBER <- as.numeric(theVMS$VR_NUMBER)
  theVMS$elapsedDist_m <- theVMS$elapsedTime_min <- NULL
  
  df <- unique(df[df$VR_NUMBER %in% theVMS$VR_NUMBER, ])
  
  # change dates to posixct so they can be compared to vms
  df$LANDED_DATE <- as.POSIXct(df$LANDED_DATE)
  df$pseudo_start <- as.POSIXct(df$pseudo_start)
  
  # filtered landings, now filter VMS to ensure we have VMS data to join
  theVMS <- theVMS[theVMS$VR_NUMBER %in% df$VR_NUMBER, ]
  
  # associate all of the vms date with fishing activity where the positions are between the 'pseudo-start' and the LANDED_DATE
  combined = sqldf::sqldf("select * 
                           from theVMS v left join df m on 
                           v.VR_NUMBER = m.VR_NUMBER and 
                           v.POSITION_UTC_DATE between m.pseudo_start and m.LANDED_DATE")
  
  # remove duplicate (VR) column
  combined <- combined[!is.na(combined$LICENCE_ID), !duplicated(colnames(combined))]
  
  if (!is.null(data.dir)) {
    e = new.env()
    get_data_tables(cxn = cxn, 
                    fn.oracle.username = fn.oracle.username, 
                    fn.oracle.password = fn.oracle.password, 
                    fn.oracle.dsn = fn.oracle.dsn, 
                    schema = "MARFISSCI", 
                    data.dir = data.dir, 
                    tables = "MARFLEETS_LIC", 
                    env = e)
    theseLics <- unique(e$MARFLEETS_LIC[e$MARFLEETS_LIC$LICENCE_ID %in% combined$LICENCE_ID, c("LICENCE_ID", "GEAR_CODE", "GEAR", "SPECIES")])
    combined <- merge(combined, theseLics, by = c("LICENCE_ID", "GEAR_CODE"), all.x = T)
  } else {
    message("If a data.dir provided, GEAR and Licenced species information will be added to the output.")
  }
  
  combined <- data.table::setorder(combined, VR_NUMBER, LICENCE_ID, GEAR_CODE, POSITION_UTC_DATE)
  res <- list()
  res$marf_VMS <- combined
  combined$m.pseudo_start <- NULL
  res$marf_VMS_Segments <- NA
  
  if (make_segments) {
    these_segs <- Mar.utils::make_segments(combined, objField = "trek", seqField = "POSITION_UTC_DATE", create.spatial = make_segments_spatial, filename = "marf_VMS_segs")
    res$marf_VMS_Segments <- these_segs$segments
  }
  
  return(res)
}