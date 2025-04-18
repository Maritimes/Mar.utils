% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subset_by_time.R
\name{subset_by_time}
\alias{subset_by_time}
\title{subset_by_time}
\usage{
subset_by_time(
  df = NULL,
  min_minutes = 15,
  time_field = "POSITION_UTC_DATE",
  group_field = "VR_NUMBER"
)
}
\arguments{
\item{df}{the dataframe that should be subset}

\item{min_minutes}{the default is \code{15}.  This is the minimum # of minutes that must separate 
successive records in \code{df}, for each group of data.  By default, groups are identified 
by unique VR_NUMBERS.}

\item{time_field}{the default is \code{"POSITION_UTC_DATE"}.  This is the name of the field that 
contains the time associated with the records.  POSIXct fields are known to work.}

\item{group_field}{the default is \code{"VR_NUMBER"}.  This is a field that identifies the group 
of positions that should be subset.}
}
\value{
a data.frame
}
\description{
This function take a databframe (e.g. from VMS_get_recs), and can return a subset
of the original records, ensuring that each is at least some amount of time from the previous.  
Specifically, by providing a \code{min_minutes}, it will return only those records that are at 
least that amount of time apart.
}
\seealso{
Other general_use: 
\code{\link{DDMMSS_to_DD}()},
\code{\link{DDMMx_to_DD}()},
\code{\link{aggregator}()},
\code{\link{clip_by_poly}()},
\code{\link{combine_lists}()},
\code{\link{df_qc_spatial}()},
\code{\link{df_sf_to_gpkg}()},
\code{\link{df_to_sf}()},
\code{\link{drop_cols}()},
\code{\link{prepare_shape_fields}()},
\code{\link{updateCheck}()}
}
\author{
Mike McMahon, \email{Mike.McMahon@dfo-mpo.gc.ca}
}
\concept{general_use}
