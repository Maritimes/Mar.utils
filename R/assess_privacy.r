#' @title assess_privacy
#' @description At this time, data with privacy considerations must be aggregated such that each 
#' polygon has a minimum of 5 unique values for sensitive fields like Licenses, License Holders, and 
#' Vessels.  This function takes a dataframe and shapefile and for each polygon in the 
#' shapefile calculates 1) aggregate values for a number of (user-specified) fields , and 2) 
#' how many unique values exist in each polygon for each of a number of sensitive fields. 
#' @param df a dataframe to be analyzed. If left \code{NULL}, a value for \code{db} should be provided

#' @param grid.shape default is \code{"hex"}.  This identifies the shape of the 
#' you want to aggregate your data into.  The options are "hex" or "square"

#' @param lat.field the default is \code{"LATITUDE"}. This is the name of the 
#' field holding latitude values (in decimal degrees)

#' @param lon.field the default is \code{"LONGITUDE"}.  This is the name of the 
#' field holding longitudevalues (in decimal degrees)

#' @param calculate the default is \code{c("MEAN", "COUNT", "SUM")}. These are the 
#' analytics which should be performed for every field identified in \code{agg.field}.
#' For example, if KEPT_WT and DISCAD_WT are both identified in \code{agg.field},
#' then for every resultant aggregated polygon (e.g. hexagon), the mean, count 
#' and sum of both of these fields is calculated for every polygon.

#' @param rule.of default is \code{5} Whether or not data can be shown (even 
#' aggregated) depends on the presence of a threshold number of unique values 
#' for certain sensitive fields.  This parameter sets that threshold.

#' @param agg.fields the default is \code{"KEPT_WT"}.  These are the fields in the data that contain 
#' the values you want to aggregate (e.g. calulate the mean, sum or count of.  This field needs to be
#' numeric.

#' @param sens.fields the defaults are \code{NULL}  These are fields
#' to which the "rule of 5" should be applied. The Treasury Secretariat states that when data is 
#' shown to the public, certain fields must have at least 5 unique values for these fields 
#' aggregated together. When run, this function will look at these fields, and calculate how many 
#' unique values exist for each.  It will then populate a field 'TOTUNIQUE' with the minimum number 
#' of unique values of all the assessed fields. If this is 5 or more, a field called 'CAN_SHOW' will 
#' be marked as 'YES' (otherwise it will be 'NO').

#' @param facet.field default is \code{NULL}.  In cases like bycatch data, you may have a dataframe
#' where each row might represent different species.  You probably want a breakdown of each individual
#' species, rather than summing them all up to get some generic weight of all species combined. This is 
#' the field that will be used to aggregate data by common values (like Species_Code) .

#' @param key.fields default is \code{NULL}.  This is a vector of fields that are required
#' to uniquely identify each fishing set.  If a \code{facet.field} is provided, the \code{facet.field}, 
#' \code{key.fields} and \code{agg.fields} are all pulled off of the original data and then merged back 
#' onto it. The key.fields are instrumental in ensuring that the data is able to get rejoined back to the 
#' original sets.

#' @param for.public default is \code{TRUE}. While calculating the aggregated values within each 
#' 2min cell, this script first establishes whether or not cells within an area have enough unique 
#' values of sensitive fields to be allowed to show any data at all.  If this parameter is \code{TRUE},
#' the calculated valued value for areas that cannot be shown will be wiped prior to generating the 
#' output files. 
#' 
#' @param create.shps default is \code{TRUE}.  This indicates whether or not 
#' shapefiles should be created for 1) the polygon file (with aggregated values 
#' for each polygon and an indication of whether or not each polygon meets the 
#' privacy constraints), and 2) the 2 min gridded data (only for within those 
#' polygons that meet the privacy constraints).

#' @param create.centroid.csv default is \code{FALSE}.  This indicates whether or not a csv  should 
#' be created for the 2 min gridded data (only for within those polygons that meet the privacy 
#' constraints).  This is a more portable option than the shapefiles or SpatialPolygonsDataFrame 
#' (i.e. it is a dramatically smaller file, usable without a GIS).
#' 
#' @param file.id default is \code{NULL} Whatever is entered here will be used 
#' to name the output shapefiles and/or plots.  If nothing is enetered, the 
#' output files will just be named using timestamps.

#' @param agg.poly.shp default is \code{NULL}.  This is the shapefile that has 
#' polygons that should be checked for sufficient unique values of the 
#' sens.fields.  If NULL, NAFO zones will be used.  Otherwise, a path to any 
#' polygon shapefile can be provided.

#' @param agg.poly.field default is \code{NULL}.  This identifies the field within 
#' the shapefile provided to agg.poly.shp that should be used to check for 
#' sufficient unique values of the sens.fields.
#' @param ignore.col.limit default is \code{FALSE} ESRI's ArcGIS doesn't like when 
#' a shapefile has more than 255 columns.  By default, this function will halt with a warning 
#' if it detects that it is producing too many columns.  To ignore the warning, and produce the 
#' shapefile anyways, set it to TRUE.
#' @import data.table

#' @return a SpatialPolygonsDataFrame, and generates a shapefile
#' @family privacy
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#' @note If sensitive fields have names that are different than what is provided in the \code{sen.fields}, 
#' they will not be detected, or included in the checks.  Please make very sure you correctly 
#' identify such fields.
#' 
#' It should be also noted that shapefiles can only have 255 columns, which can 
#' be exceeded by this function relatively easily when a \code{facet.field} is 
#' provided (e.g. for bycatch species).  For example, if all 3 default \code{calculate} 
#' fields are requested on 3 different \code{agg.fields}, and there are 30 
#' unique values in the \code{facet.field}, this will result in (3*3*30 =) 270 
#' fields plus 3 or 4 additional housekeeping fields. In this event, a warning 
#' will be provided, but no shapefiles will be output. 
assess_privacy <- function(
  df= NULL, 
  grid.shape = 'hex',
  lat.field = 'LATITUDE',
  lon.field = 'LONGITUDE',
  rule.of = 5,
  agg.fields = "KEPT_WT",
  calculate = c("MEAN", "COUNT", "SUM"),
  sens.fields = NULL,
  facet.field = NULL,
  key.fields = NULL,
  for.public = TRUE,
  create.shps = TRUE,
  create.centroid.csv = FALSE,
  file.id = NULL,
  agg.poly.shp = NULL,
  agg.poly.field = NULL, 
  ignore.col.limit = FALSE
){
  #set up
  ts = format(Sys.time(), "%Y%m%d_%H%M")
  `:=` <- function (x, value) value
  
  analyticChooser <- function(x, calculate){
    #this function is called by the aggregate functions to allow use to select which analytics are calculated for all agg.fields
    res <- NA
    if ("CNT" %in% calculate)  res= c(res, round(length(x[x!=0]), 0))
    if ("SUM" %in% calculate)  res= c(res, round(sum(x), 4))
    if ("MEAN" %in% calculate)  res= c(res, round(mean(x), 4))
    res = res[!is.na(res)]
    return(res)
  }
  
  mkCentroidDf <- function(grid = NULL){
    # the default polygons are large.  This can convert them to csvs 
    this <- as.data.frame(geosphere::centroid(grid))
    colnames(this) <- c("lon", "lat") 
    this <- data.frame("ID" = 1:nrow(this), this)
    sp::coordinates(this) <- c("lon", "lat") 
    sp::proj4string(this) <- sp::proj4string(grid) # assign projection
    
    this@data <- sp::over(x = this, y = grid, returnList = FALSE)
    this@data[,c("LON","LAT")] <- sp::coordinates(this)
    this_csv <- this@data
    return(this_csv)
  }
  
  #deal with cases where differing rows may refer to different species in the same sets
  #in short:
  #1)pull key.fields, facet.field and agg.fields into dfLong
  #2)dcast dfLong to dfWide (e.g. every species gets it's own column instead of row)
  #3)with most of these fields removed, unique() the remaining (set-related) info and drastically reduce nrows
  #4)merge the wide species data back onto the shortened set info
  if (!is.null(key.fields) && !is.null(facet.field) && !is.null(agg.fields)){
    foo <- function(df, id, measure, val) {
      res = data.table::dcast(data.table::setDT(df), paste(paste(id, collapse = " + "), "~", 
                                                           paste(measure, collapse = " + ")), function(x) analyticChooser(x, calculate), 
                              value.var = val)
      res = data.table::setDF(res)
      return(res)
    }
    dfLong <- df[,c(key.fields,c(facet.field, agg.fields))]
    dfLong <- dfLong[!is.na(dfLong[facet.field]),]

    dfRest= df[,!names(df) %in% c(facet.field, agg.fields)]
    dfRestU <- unique(dfRest)
    if (nrow(dfRest)==nrow(dfRestU)){
      stop(cat("facet.field must identify a single identifier field that causes repeated sets - e.g. species code/name","\n",
               "agg.fields must identify the vaulues you want information for related to the facet.field - e.g. kept_wt, discarded wt, etc","\n",
               "key.fields must identify enough fields required to identify unique fishing sets - e.g. trip & set","\n"))
    }else{
      dfWide <- foo(dfLong, id=key.fields, measure = facet.field,val = agg.fields)
      # dfWide$ALL_TOT<-NA
      # dfWide$ALL_TOT <- rowSums(dfWide[,!names(dfWide) %in% key.fields],na.rm = T)
      agg.fields <- names(dfWide[,!names(dfWide) %in% key.fields])
      # cat("Updated agg.fields to reflect new, faceted data structure","\n")
      df<-dfRestU
    }
  }
  df = df_to_sp(df, lat.field, lon.field)
  # df = Mar.utils::df_qc_spatial(df, lat.field, lon.field, FALSE)
  # sp::coordinates(df) = c(lon.field, lat.field)
  # sp::proj4string(df) = sp::CRS(SRS_string="EPSG:4326")
  if (is.null(agg.poly.shp)){
    agg.poly=  Mar.data::NAFOSubunits
    defFields <- c("NAFO_1", "NAFO_2", "NAFO_3","NAFO_BEST")
    # know that the CRS of agg.poly obj is WGS84, but perhaps saved with 
    #slight variations in the text
      sp::proj4string(agg.poly) <- sp::CRS(as.character(NA))
      sp::proj4string(agg.poly) = sp::CRS(SRS_string="EPSG:4326")
    
    if (is.null(agg.poly.field)){
      agg.poly.field = 'NAFO_BEST'
    }
    defFields = defFields[!defFields %in% agg.poly.field]
    agg.poly@data[ ,defFields] <- list(NULL)
  }else{
    agg.poly <- rgdal::readOGR(dsn = agg.poly.shp, verbose = FALSE)
    if (is.na(sp::proj4string(agg.poly))) {
      cat('\nNo projection found for input shapefile - assuming geographic.')
      sp::proj4string(agg.poly) = sp::CRS(SRS_string="EPSG:4326")
    }
    #convert the shape to geographic
    agg.poly <- suppressWarnings(sp::spTransform(agg.poly,sp::CRS(SRS_string="EPSG:4326")))
  }
  pip <- sp::over( df, agg.poly , fn = NULL)
  df@data = cbind(df@data, pip)
 
  if (!is.null(key.fields) && !is.null(facet.field) && !is.null(agg.fields)) df <- sp::merge(df, dfWide, by=key.fields, all.x=T)
  df@data[agg.fields][is.na(df@data[agg.fields])] <- 0
  df@data[agg.fields] <- lapply(df@data[agg.fields], as.numeric)
  
  calc= NA
  if ("MEAN" %in% calculate) calc = c(calc, "MEAN = round(mean(x), 4)")
  if ("CNT" %in% calculate) calc =  c(calc, "CNT = round(length(x[x!=0]), 0)")
  if ("SUM" %in% calculate) calc = c(calc, "SUM = round(sum(x), 4)")
  calc = calc[!is.na(calc)]
  # 
  # 
  # POLY.agg <- as.data.frame(as.list(stats::aggregate(
  #   df@data[agg.fields],
  #   by = df@data[c(agg.poly.field)],
  #   FUN = function(x)
  #     c(
  #       MEAN = round(mean(x), 4),
  #       CNT = round(length(x[x!=0]), 0),
  #       SUM = round(sum(x), 4)
  #     )
  # )))

  POLY.agg <- as.data.frame(as.list(stats::aggregate(
    df@data[agg.fields],
    by = df@data[c(agg.poly.field)],
    FUN = function(x) analyticChooser(x, calculate)
  )))
  

  POLY.agg[,2:ncol(POLY.agg)] <- sapply(POLY.agg[,2:ncol(POLY.agg)], as.numeric)
  if (!is.null(sens.fields)){
    POLY.agg.sens = as.data.frame(as.list(stats::aggregate(
      df@data[intersect(sens.fields, colnames(df@data))],
      by = df@data[c(agg.poly.field)],
      FUN = function(x)
        c(
          CNT = round(length(unique(x)), 0)
        )
    )))

   
    POLY.agg.sens$TOTUNIQUE = apply(as.data.frame(POLY.agg.sens[,2:ncol(POLY.agg.sens)]), 1, min)
    POLY.agg.sens$CAN_SHOW <- 'NA'
    if (nrow(POLY.agg.sens[POLY.agg.sens$TOTUNIQUE>=rule.of,])>0) POLY.agg.sens[POLY.agg.sens$TOTUNIQUE>=rule.of,]$CAN_SHOW <- 'YES'
    if (nrow(POLY.agg.sens[POLY.agg.sens$TOTUNIQUE<rule.of,])>0) POLY.agg.sens[POLY.agg.sens$TOTUNIQUE< rule.of,]$CAN_SHOW <- 'NO'
    POLY.agg = merge(POLY.agg, POLY.agg.sens)
    if (for.public){
      #if the output is for the public, all of the calculated aggregate values for polys that can't 
      #be shown need to be dropped
      keepFields = c(agg.poly.field, "CAN_SHOW")
      POLY.agg[POLY.agg$CAN_SHOW == "NO",!names(POLY.agg) %in% keepFields]<-NA
      #also, all of the fields with the counts of the unique numbers of all of the sensitive fields 
      #should be dropped
      POLY.agg[ ,c(sens.fields,"TOTUNIQUE")] <- list(NULL)
    }
    POLY.agg = sp::merge(agg.poly,POLY.agg)
    rm(POLY.agg.sens) 
  }else{
    POLY.agg$CAN_SHOW = 'YES'
    POLY.agg = sp::merge(agg.poly,POLY.agg)
  }
  allowed.areas = POLY.agg@data[!is.na(POLY.agg$CAN_SHOW) & POLY.agg$CAN_SHOW=='YES',agg.poly.field]
  allowed.areas.sp = agg.poly[agg.poly@data[[agg.poly.field]] %in% allowed.areas,]
  df$ORD_df = seq.int(nrow(df))
  if (grid.shape =="hex"){
    grid2Min<-Mar.data::hex
  } else{
    grid2Min<-Mar.data::grid2Min
  }
  # know that the CRS of both grid2Min obj is WGS84, but perhaps saved with 
  #slight variations in the text
  sp::proj4string(grid2Min) <- sp::CRS(as.character(NA))
  sp::proj4string(grid2Min) = sp::CRS(SRS_string="EPSG:4326")

  grid2Min$ORD_gr <-  seq.int(nrow(grid2Min)) 
  if (length(allowed.areas)>0){
    #clip the data to those overlaying acceptable NAFO
    df <- df[allowed.areas.sp, ]
    grid2Min <- grid2Min[allowed.areas.sp,]
    #step 1 -- figure out which grid each point is in.
    join <- sp::over(df, grid2Min)  
    join$ORD_df <- seq.int(nrow(join)) 
    df@data = cbind(df@data,join)
    grid.agg = as.data.frame(as.list(stats::aggregate(
      df@data[agg.fields],
      by = df@data[c('ORD_gr')],
      # FUN = function(x)
      #   c(
      #     MEAN = round(mean(x), 4),
      #     CNT = round(length(x[x!=0]), 0),
      #     SUM = round(sum(x), 4)
      #   )
      FUN = function(x) analyticChooser(x, calculate)
    )))
    #step 3 -- append the aggregated data back onto the grid 
    grid2Min=sp::merge(grid2Min, grid.agg)
    #populate all empty cells with 0
    grid2Min@data[!names(grid2Min@data) %in% c("HEXID", "ORD_gr")][is.na(grid2Min@data[!names(grid2Min@data) %in% c("HEXID", "ORD_gr")])] <- 0 
    file.id = ifelse(!is.null(file.id),paste0(file.id,"_"),"")
    POLY.agg.name = paste0(file.id,'POLY_AGG_', ts)
    this.df.name = paste0(file.id,'Grid2Min_', ts)
    this.csv.name = paste0(file.id,'Grid2MinCentroid_', ts)
    
    # there are 3 analytics possible within the agg function via (analyticChooser) - CNT, SUM and MEAN
    # the resultant column get added to the data with the original names with "*.1", "*.2", or "*.3"
    # appended, depending on which analytic the values correspond with
    # the next bit will help establish which appended number corresponds with which analytic
    # possible analytics
    usedAnal <- intersect(c("CNT", "SUM", "MEAN"), calculate) # this returns the ones that were run
    nums <- match(calculate,usedAnal)   
    for (i in 1:length(nums)){
      thisSearch = nums[i]
      thisrep = usedAnal[thisSearch]
      colnames(POLY.agg@data) <- sub(paste0("\\.", thisSearch), paste0("_",thisrep), colnames(POLY.agg@data))
      colnames(grid2Min@data) <- sub(paste0("\\.", thisSearch), paste0("_",thisrep), colnames(grid2Min@data))
    }
results <- list()
    if (create.centroid.csv) {
      this <- mkCentroidDf(grid2Min)
      results$centroidCsv <- this
      write.csv(this, paste0(this.csv.name,".csv"))
      message(paste0("\nCreated csv ", getwd(), .Platform$file.sep, this.csv.name,".csv\n"))
    }
    if (create.shps){
      if (!ignore.col.limit && max(ncol(POLY.agg@data), ncol(grid2Min@data))>255){
        warning("\nCan not create shapefiles.  Shapefiles are limited to 255 fields, and your data will have more than that.
The number of fields is determined by:
1) the number of unique values found in your facet.field (if present, e.g. bycatch species);
2) the number of specified agg.fields (e.g. kept_weight, discarded weight); 
3) for each combination above, a sum, count and mean is calculated;
4) several housekeeping fields are also created")
      }else{
      POLY.agg = prepare_shape_fields(POLY.agg) 
      rgdal::writeOGR(obj = POLY.agg, dsn= getwd(), layer = POLY.agg.name, driver="ESRI Shapefile", overwrite_layer=TRUE)
      message(paste0("\nCreated shapefile ", getwd(), .Platform$file.sep, POLY.agg.name,".shp\n"))
      grid2Min = prepare_shape_fields(grid2Min)
      rgdal::writeOGR(obj = grid2Min, dsn=getwd(), layer = this.df.name, driver="ESRI Shapefile", overwrite_layer=TRUE)
      message(paste0("\nCreated shapefile ", getwd(), .Platform$file.sep, this.df.name,".shp\n"))
      }
    }
    results$Grid2Min <- grid2Min
    results$POLY_AGG <- POLY.agg
  }else{
    print("No polygon has enough unique values to allow aggregated data to be shown")
    results= list("Grid2Min" = NULL, "POLY_AGG" = NULL)
  }
  return(invisible(results))
}