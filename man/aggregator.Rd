% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregator.R
\name{aggregator}
\alias{aggregator}
\title{aggregator}
\usage{
aggregator(
  df = NULL,
  lat.field = "LATITUDE",
  lon.field = "LONGITUDE",
  agg.fields = NULL,
  agg.minutes = 5,
  facet.field = NULL,
  calculate = c("COUNT", "SUM", "MEAN")
)
}
\arguments{
\item{df}{a dataframe to be analyzed.}

\item{lat.field}{the default is \code{"LATITUDE"}. This is the name of the field holding latitude 
values (in decimal degrees)}

\item{lon.field}{the default is \code{"LONGITUDE"}.  This is the name of the field holding 
longitude values (in decimal degrees)}

\item{agg.fields}{the default is \code{NULL}.  This is a vector of 1 or more fields in the data 
that contain the values you want to aggregate (e.g. calculate the mean, sum or count of).  These 
fields needs to be numeric.  The output aggregated data will ONLY have the \code{lat.field}, the 
\code{lon.field} and these \code{agg.fields}.}

\item{agg.minutes}{the default is \code{5}.  This specifies how many minutes the data should be 
aggregated by.}

\item{facet.field}{default is \code{NULL}.  In cases like bycatch data, you may have a dataframe
where each row might represent different species.  You probably want a breakdown of each individual
species, rather than summing them all up to get some generic weight of all species combined. This is 
the field that will be used to aggregate data by common values (like Species_Code) .}

\item{calculate}{the default is \code{c("MEAN", "COUNT", "SUM")}. These are the 
analytics which should be performed for every field identified in \code{agg.field}.
For example, if KEPT_WT and DISCAD_WT are both identified in \code{agg.field},
then for every resultant aggregated polygon (e.g. hexagon), the mean, count 
and sum of both of these fields is calculated for every polygon.}
}
\value{
a data frame
}
\description{
This function takes a dataframe with coordinates, and facilitates aggregating the
data for specific (user-specified) fields.  It outputs a gridded version of the original data.  
Any numeric field can be aggregated, and aggregate values can be one or more of SUMs, MEANs 
and/or COUNTs
}
\seealso{
Other privacy: 
\code{\link{assess_privacy}()},
\code{\link{plot_hex_data}()}
}
\author{
Mike McMahon, \email{Mike.McMahon@dfo-mpo.gc.ca}
}
\concept{privacy}
