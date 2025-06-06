#' @title aggregator
#' @description This function takes a dataframe with coordinates, and facilitates aggregating the
#' data for specific (user-specified) fields.  It outputs a gridded version of the original data.  
#' Any numeric field can be aggregated, and aggregate values can be one or more of SUMs, MEANs 
#' and/or COUNTs
#' @param df a dataframe to be analyzed.

#' @param lat.field the default is \code{"LATITUDE"}. This is the name of the field holding latitude 
#' values (in decimal degrees)

#' @param lon.field the default is \code{"LONGITUDE"}.  This is the name of the field holding 
#' longitude values (in decimal degrees)
#' 
#' @param facet.field default is \code{NULL}.  In cases like bycatch data, you may have a dataframe
#' where each row might represent different species.  You probably want a breakdown of each individual
#' species, rather than summing them all up to get some generic weight of all species combined. This is 
#' the field that will be used to aggregate data by common values (like Species_Code) .

#' @param agg.fields the default is \code{NULL}.  This is a vector of 1 or more fields in the data 
#' that contain the values you want to aggregate (e.g. calculate the mean, sum or count of).  These 
#' fields needs to be numeric.  The output aggregated data will ONLY have the \code{lat.field}, the 
#' \code{lon.field} and these \code{agg.fields}.
#' 
#' @param agg.minutes the default is \code{5}.  This specifies how many minutes the data should be 
#' aggregated by. 

#' @param calculate the default is \code{c("MEAN", "COUNT", "SUM")}. These are the 
#' analytics which should be performed for every field identified in \code{agg.field}.
#' For example, if KEPT_WT and DISCAD_WT are both identified in \code{agg.field},
#' then for every resultant aggregated polygon (e.g. hexagon), the mean, count 
#' and sum of both of these fields is calculated for every polygon.

#' @return a data frame
#' @family privacy
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#' 
aggregator = function(df = NULL,
                      lat.field = "LATITUDE",
                      lon.field = "LONGITUDE",
                      agg.fields = NULL,
                      agg.minutes = 5,
                      facet.field = NULL,
                      calculate = c("COUNT", "SUM", "MEAN")) {
  df <- df[,c(lat.field, lon.field, agg.fields, facet.field)]
  
  analyticChooser <- function(x, calculate){
    #this function is called by the aggregate functions to allow use 
    #to select which analytics are calculated for all agg.fields
    res <- NA
    if ("COUNT" %in% calculate)  res= c(res, round(length(x[x!=0]), 0))
    if ("SUM" %in% calculate)  res= c(res, round(sum(x), 4))
    if ("MEAN" %in% calculate)  res= c(res, round(mean(x), 4))
    res = res[!is.na(res)]
    return(res)
  }
  
  agg = agg.minutes / 60
  df = df[!is.na(df[lat.field]) & !is.na(df[lon.field]), ]
  df$LATITUDE_BOOYUCKASHA = (round(df[, lat.field] / agg) * agg) #+ 0.5 * agg
  df$LONGITUDE_BOOYUCKASHA = (round(df[, lon.field] / agg) * agg) #- 0.5 * agg
  
  df[, c(lat.field,lon.field)] <- NULL
  
  df[agg.fields][is.na(df[agg.fields])] <-0
  df[agg.fields] <- sapply(df[agg.fields], as.numeric)
  if(is.null(facet.field)){
    df.agg = as.data.frame(as.list(stats::aggregate(
      df[agg.fields],
      by = df[c("LATITUDE_BOOYUCKASHA", "LONGITUDE_BOOYUCKASHA")],
      function(x) analyticChooser(x, calculate)
    )))
  }else{
    df.agg = as.data.frame(as.list(stats::aggregate(
      df[agg.fields],
      by = df[c(facet.field, "LATITUDE_BOOYUCKASHA", "LONGITUDE_BOOYUCKASHA")],
      function(x) analyticChooser(x, calculate)
    )))
    #i may have broken the ability to agg by multiple fields below
    if (!"MEAN" %in% calculate) df.agg <- reshape2::dcast(df.agg , LATITUDE_BOOYUCKASHA+LONGITUDE_BOOYUCKASHA  ~ get(facet.field) , value.var = agg.fields, fun.aggregate = sum)
  }
  
  usedAnal <- intersect(c( "COUNT", "SUM"), calculate) # this returns the ones that were run
  nums <- match(calculate,usedAnal)   
  for (i in 1:length(nums)){
    thisSearch = nums[i]
    thisrep = usedAnal[thisSearch]
    colnames(df.agg) <- sub(paste0("\\.", thisSearch), paste0("_",thisrep), colnames(df.agg))
  }
  
  df.agg[is.na(df.agg)] <- 0
  colnames(df.agg)[colnames(df.agg)=="LATITUDE_BOOYUCKASHA"] <- lat.field
  colnames(df.agg)[colnames(df.agg)=="LONGITUDE_BOOYUCKASHA"] <- lon.field
  return(df.agg)
}