#' @title subset_by_time
#' @description This function take a dataframe (e.g. from VMS_get_recs), and can return a subset
#' of the original records, ensuring that each is at least some amount of time from the previous.  
#' Specifically, by providing a \code{min_minutes}, it will return only those records that are at 
#' least that amount of time apart. 
#' @param df the dataframe that should be subset
#' @param min_minutes the default is \code{15}.  This is the minimum # of minutes that must separate 
#' successive records in \code{df}, for each group of data.  By default, groups are identified 
#' by unique VR_NUMBERS.  
#' @param time_field the default is \code{"POSITION_UTC_DATE"}.  This is the name of the field that 
#' contains the time associated with the records.  POSIXct fields are known to work.
#' @param group_field the default is \code{"VR_NUMBER"}.  This is a field that identifies the group 
#' of positions that should be subset.  
#' @return a data.frame
#' @family vms
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export

subset_by_time <- function(df =  NULL, min_minutes = 15, time_field = "POSITION_UTC_DATE", group_field = "VR_NUMBER"){
  
  grp_fld__ <- tim_fld__ <- NA
  
  #most operations use seconds - calculate
  min_mins_secs <- min_minutes*60
  df$grp_fld__ <- df[,group_field]
  df$tim_fld__ <- df[,time_field]
  requireNamespace("data.table")
  df_dt <- data.table::setDT(df)
  
  #sort data by VR_NUMBER, POSITION_UTC_DATE
  df_dt <- data.table::setorder(df_dt, grp_fld__, tim_fld__)
  # calculate successive diffs in time
  df_dt <- df_dt[ , diff := tim_fld__ - shift(tim_fld__), by = grp_fld__] 
  df_dt$diff <- as.integer(df_dt$diff)  
  #NAs will be the first occurrence of a group - set its time to be kept
  df_dt[is.na(df_dt$diff),"diff"]<-min_mins_secs
  df_dt <-setDF(df_dt)
  
  grps <- unique(df_dt$grp_fld__)
  res<- df_dt[FALSE,]
  res$diff <- NULL
  
  for (g in 1:length(grps)){
    thisGrp <- df_dt[df_dt$grp_fld__ == grps[g],]
    thisGrp$keep <- FALSE
    current.sum <- 0
    for (c in 1:nrow(thisGrp)) {
      current.sum <- current.sum + thisGrp[c, "diff"]
      if (current.sum >= min_mins_secs) {
        thisGrp[c, "keep"] <- T
        current.sum <- 0
      }
    }
    thisGrp<-thisGrp[thisGrp$keep ==T,]
    thisGrp$diff <- thisGrp$keep <- NULL
    res <- rbind.data.frame(res, thisGrp)
  }
  return(res)
}


# test<- subsetVMSByMinTime(vms_df = vms, min_minutes = 61)
# test<- subsetVMSByMinTime(vms_df = vms, min_minutes = 1)
