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
#' field in \code{df} holding latitude values (in decimal degrees)

#' @param lon.field the default is \code{"LONGITUDE"}.  This is the name of the 
#' field in \code{df} holding longitudevalues (in decimal degrees)

#' @param calculate the default is \code{c("MEAN", "COUNT", "SUM")}. These are the 
#' analytics which should be performed for every field identified in \code{agg.field}.
#' For example, if KEPT_WT and DISCARD_WT are both identified in \code{agg.field},
#' then for every resultant aggregated polygon (e.g. hexagon), the mean, count 
#' and sum of both of these fields is calculated for every polygon.

#' @param rule.of default is \code{5} Whether or not data can be shown (even 
#' aggregated) depends on the presence of a threshold number of unique values 
#' for certain sensitive fields.  This parameter sets that threshold.

#' @param agg.fields the default is \code{"KEPT_WT"}.  These are the fields in the data that contain 
#' the values you want to aggregate (e.g. calculate the mean, sum or count of.  This field needs to be
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
#' @param create.spatial default is \code{TRUE}.  This indicates whether or not to create a gpkg file
#' containing spatial files for 1) the polygon file (with aggregated values 
#' for each polygon and an indication of whether or not each polygon meets the 
#' privacy constraints), and 2) the 2 min gridded data (only for within those 
#' polygons that meet the privacy constraints).  

#' @param create.centroid.csv default is \code{FALSE}.  This indicates whether or not a csv  should 
#' be created for the 2 min gridded data (only for within those polygons that meet the privacy 
#' constraints).  This is a more portable option than the gpkg file created by the \code{create.spatial}
#' parameter, and is usable without a GIS.  If this is \code{TRUE} AND \code{create.spatial} is 
#' \code{TRUE}, then the centroid file will also be added to the generated gpkg file.
#'  
#' @param file.id default is \code{NULL} Whatever is entered here will be used 
#' to name the output shapefiles and/or plots.  If nothing is entered, the 
#' output files will just be named using timestamps.

#' @param agg.poly.shp default is \code{NULL}.  This is the shapefile that has 
#' polygons that should be checked for sufficient unique values of the 
#' sens.fields.  If NULL, NAFO zones will be used.  Otherwise, a path to any 
#' polygon shapefile can be provided.

#' @param agg.poly.field default is \code{NULL}.  This identifies the field within 
#' the shapefile provided to agg.poly.shp that should be used to check for 
#' sufficient unique values of the sens.fields.

#' @param custom.grid  default is \code{NULL}.  If there is a need to use a custom grid to apply to 
#' the data, 

#' @import data.table

#' @return a list containing an sf grid layer, an sf overlay later, and if \code{create.spatial==T},
#' a gpkg spatial file containing these same objects. Additionally, if \code{create.centroid.csv =T},
#' it can also produce a csv of the centroids of the grid layer (which willl also be loaded into the 
#' gpkg file).
#' @family privacy
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#' @note If sensitive fields have names that are different than what is provided in the \code{sen.fields}, 
#' they will not be detected, or included in the checks.  Please make very sure you correctly 
#' identify such fields.
#' 
#' It should be also noted that this function can result in spatial files with 100s of columns 
#' relatively easily when a \code{facet.field} is provided (e.g. for bycatch species).  For example, 
#' if all 3 default \code{calculate} fields are requested on 3 different \code{agg.fields}, and 
#' there are 30 unique values in the \code{facet.field}, this will result in (3*3*30 =) 270 
#' fields plus 3 or 4 additional housekeeping fields.
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
    create.spatial = TRUE,
    create.centroid.csv = FALSE,
    file.id = NULL,
    agg.poly.shp = NULL,
    agg.poly.field = NULL, 
    custom.grid = NULL
){
  #set up
  ts = format(Sys.time(), "%Y%m%d_%H%M")
  `:=` <- function (x, value) value
  
  if (!is.null(custom.grid)){
    ogrPath = dirname(custom.grid)
    ogrLayer = sub('\\.shp$', '', basename(custom.grid))
    sp_this <- sf::st_read(dsn = ogrPath, layer = ogrLayer, quiet=T)
    if (!inherits(sp_this,"sf"))stop("custom.grid could not be loaded")
  }
  analyticChooser <- function(x, calculate){
    #this function is called by the aggregate functions to allow use to select which analytics are calculated for all agg.fields
    res <- NA
    if ("COUNT" %in% calculate)  res= c(res, round(length(x[x!=0]), 0))
    if ("SUM" %in% calculate)  res= c(res, round(sum(x), 4))
    if ("MEAN" %in% calculate)  res= c(res, round(mean(x), 4))
    res = res[!is.na(res)]
    return(res)
  }
  
  mkCentroidDf <- function(grid = NULL){
    # the default polygons are large.  This can convert them to csvs 
    this <- sf::st_centroid(grid)
    this_coords<-   as.data.frame(sf::st_coordinates(this))
    #coords with 7 digits are accurate to cms 
    this_coords[c("X","Y")] <-  round(this_coords[c("X","Y")], 7)
    this <- cbind(this, this_coords)
    this <- sf::st_drop_geometry(this)
    colnames(this)[colnames(this)=="X"] <- "LONGITUDE"
    colnames(this)[colnames(this)=="Y"] <- "LATITUDE"
    return(this)
  }
  
  noCoords <- df[!stats::complete.cases(df[,c(lat.field, lon.field)]),]
  if(nrow(noCoords>0)){
    message(nrow(noCoords)," records had one or more missing coordinates and were dropped")
    df <- df[stats::complete.cases(df[,c(lat.field, lon.field)]),]
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
    
    noKey <- df[!stats::complete.cases(df[,c(key.fields)]),]
    if(nrow(noKey>0)){
      message(nrow(noKey)," records had one or more missing valuess within the key.fields and were dropped")
      df <- df[stats::complete.cases(df[,c(key.fields)]),]
    }
    dfLong <- df[,c(key.fields,c(facet.field, agg.fields))]
    dfLong <- dfLong[!is.na(dfLong[facet.field]),]
    dfRest  <- df[,!names(df) %in% c(facet.field, agg.fields)]
    dfRestU <- unique(dfRest)
    if (nrow(dfRest)==nrow(dfRestU)){
      stop(message("facet.field must identify a single identifier field that causes repeated sets - e.g. species code/name","\n",
                   "agg.fields must identify the vaulues you want information for related to the facet.field - e.g. kept_wt, discarded wt, etc","\n",
                   "key.fields must identify enough fields required to identify unique fishing sets - e.g. trip & set","\n"))
    }else{
      dfWide <- foo(dfLong, id=key.fields, measure = facet.field,val = agg.fields)
      
      
      # 
      # dfWide$ALL_TOT<-NA
      # dfWide$ALL_TOT <- rowSums(dfWide[,!names(dfWide) %in% key.fields],na.rm = T)
      agg.fields <- names(dfWide[,!names(dfWide) %in% key.fields])
      # df<-merge(dfRestU, dfWide, by = key.fields)
      df <- dfRestU
    }
    
    dfLong <- dfRest <- dfRestU <- NULL
  }
  df <- df_to_sf(df, lat.field = lat.field, lon.field = lon.field, type = "points")
  if (is.null(agg.poly.shp)){
    agg.poly=  Mar.data::NAFOSubunits_sf
    defFields <- c("NAFO_1", "NAFO_2", "NAFO_3","NAFO")
    if (is.null(agg.poly.field)){
      agg.poly.field = 'NAFO'
    }
    defFields = defFields[!defFields %in% agg.poly.field]
  }else{
    agg.poly     <- sf::st_read (agg.poly.shp, quiet=T)
    if (is.na(sf::st_crs(agg.poly))){
      message('\nNo projection found for input shapefile - assuming geographic.')
      sf::st_crs(agg.poly) <- 4326
    }else{
      #convert the shape to geographic
      agg.poly <- sf::st_transform(agg.poly, crs = 4326)
    }
  }
  df <- sf::st_join(df, agg.poly[,agg.poly.field], join = sf::st_intersects)
  
  if (!is.null(key.fields) && !is.null(facet.field) && !is.null(agg.fields)) {
    for (a in 1:length(agg.fields)){
      dfWide[[agg.fields[a]]] <- as.numeric(dfWide[[agg.fields[a]]])
    }
    
    df <- merge(df, dfWide, by=key.fields, all.x=T)
  }else{
    for (a in 1:length(agg.fields)){
      df[[agg.fields[a]]] <- as.numeric(df[[agg.fields[a]]])
    }
  }
  POLY.agg <- as.data.frame(df[,c(agg.fields, agg.poly.field)])
  POLY.agg <- as.data.frame(as.list(stats::aggregate(
    POLY.agg[,agg.fields],
    by = list(POLY.agg[,c(agg.poly.field)]),
    FUN = function(x) analyticChooser(x, calculate)
  )))
  colnames(POLY.agg)[colnames(POLY.agg)=="Group.1"] <- agg.poly.field
  
  if (!is.null(sens.fields)){
    
    plainDF <- as.data.frame(sf::st_drop_geometry(df))
    POLY.agg.sens = as.data.frame(stats::aggregate(
      plainDF[intersect(sens.fields, colnames(plainDF))],
      by = plainDF[c(agg.poly.field)],
      FUN = function(x)
        c(
          COUNT = round(length(unique(x)), 0)
        )
    ))
    
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
    
    POLY.agg = merge(agg.poly,POLY.agg)
    rm(POLY.agg.sens) 
  }else{
    POLY.agg$CAN_SHOW = 'YES'
    POLY.agg = merge(agg.poly,POLY.agg)
  }
  allowed.areas = as.vector(sf::st_drop_geometry(POLY.agg[!is.na(POLY.agg$CAN_SHOW) & POLY.agg$CAN_SHOW=='YES',agg.poly.field]))[[1]]
  
  allowed.areas.sp = agg.poly[agg.poly[,agg.poly.field][[1]] %in% allowed.areas,]
  df$ORD_df = seq.int(nrow(df))
  if(!is.null(custom.grid)){
    grid2Min<- sp_this
  }else if (grid.shape =="hex"){
    grid2Min<-Mar.data::hex_sf
  } else{
    message("need to make a grid2min_sf")
    grid2Min<-Mar.data::grid2Min
  }
  # know that the CRS of both grid2Min obj is WGS84, but perhaps saved with 
  #slight variations in the text
  grid2Min <- sf::st_set_crs(grid2Min,4326)
  grid2Min$ORD_gr <-  seq.int(nrow(grid2Min)) 
  if (length(allowed.areas)>0){
    #clip the data to those overlaying acceptable NAFO
    df <- df[allowed.areas.sp, ]
    grid2Min <- grid2Min[allowed.areas.sp,]
    #step 1 -- figure out which grid each point is in.
    
    join <- sf::st_join(df, grid2Min, join = sf::st_intersects)
    join$ORD_df <- seq.int(nrow(join)) 
    join_df <- sf::st_drop_geometry(join)
    grid.agg = as.data.frame(as.list(stats::aggregate(
      join_df[agg.fields],
      by = join_df[c('ORD_gr')],
      FUN = function(x) analyticChooser(x, calculate)
    )))
    #step 3 -- append the aggregated data back onto the grid 
    grid2Min=merge(grid2Min, grid.agg, all.x=T)
    #populate all empty cells with 0
    #not sure we want to change NA to zero??
    grid2Min[!names(grid2Min) %in% c("HEXID", "ORD_gr")][is.na(grid2Min[!names(grid2Min) %in% c("HEXID", "ORD_gr")])] <- 0 
    
    file.id = ifelse(!is.null(file.id),paste0(file.id,"_"),"")
    POLY.agg.name = paste0(file.id,'POLY_AGG_', ts)
    this.df.name = paste0(file.id,'Grid2Min_', ts)
    this.csv.name = paste0(file.id,'Grid2MinCentroid_', ts)
    
    # there are 3 analytics possible within the agg function via (analyticChooser) - COUNT, SUM and MEAN
    # the resultant column get added to the data with the original names with "*.1", "*.2", or "*.3"
    # appended, depending on which analytic the values correspond with
    # the next bit will help establish which appended number corresponds with which analytic
    # possible analytics
    usedAnal <- intersect(c("COUNT", "SUM", "MEAN"), calculate) # this returns the ones that were run
    nums <- match(calculate,usedAnal)   
    for (i in 1:length(nums)){
      thisSearch = nums[i]
      thisrep = usedAnal[thisSearch]
      colnames(POLY.agg) <- sub(paste0("\\.", thisSearch), paste0("_",thisrep), colnames(POLY.agg))
      colnames(grid2Min) <- sub(paste0("\\.", thisSearch), paste0("_",thisrep), colnames(grid2Min))
    }
    
    results <- list()
    if (create.centroid.csv) {
      this <- mkCentroidDf(grid2Min)
      utils::write.csv(this, paste0(this.csv.name,".csv"))
      df_sf_to_gpkg(this, layerName = this.csv.name, gpkgName = "assess_privacy.gpkg")
      results$centroidCsv <- this
      message(paste0("\nCreated csv ", getwd(), .Platform$file.sep, this.csv.name,".csv\n"))
    }
    if (create.spatial){
      df_sf_to_gpkg(POLY.agg, layerName = POLY.agg.name, gpkgName = "assess_privacy.gpkg")
      df_sf_to_gpkg(grid2Min, layerName = this.df.name, gpkgName = "assess_privacy.gpkg")
    }
    results$Grid2Min <- grid2Min
    results$POLY_AGG <- POLY.agg
  }else{
    message("No polygon has enough unique values to allow aggregated data to be shown")
    results= list("Grid2Min" = NULL, "POLY_AGG" = NULL)
  }
  return(invisible(results))
}