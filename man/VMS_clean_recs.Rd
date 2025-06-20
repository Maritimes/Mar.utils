% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/VMS_clean_recs.R
\name{VMS_clean_recs}
\alias{VMS_clean_recs}
\title{VMS_clean_recs}
\usage{
VMS_clean_recs(
  df = NULL,
  lat.field = "LATITUDE",
  lon.field = "LONGITUDE",
  objField = "VR_NUMBER",
  timeField = "POSITION_UTC_DATE",
  minDist_m = 50,
  maxBreak_mins = 1440,
  minKnots = NULL,
  maxKnots = NULL,
  speedField = "calc",
  dropOrphans = T
)
}
\arguments{
\item{df}{default is \code{NULL}.  This is the dataframe to be processed.  It 
should have coordinates in decimal degrees and they should be in fields 
called "LATITUDE" and "LONGITUDE".  It also needs a field with the time 
associated with each position, i.e. the  \code{timeField}.}

\item{lat.field}{the default is \code{"LATITUDE"}. the name of the field holding latitude values 
(in decimal degrees)}

\item{lon.field}{the default is \code{"LONGITUDE"}.  the name of the field holding longitude 
values (in decimal degrees)}

\item{objField}{default is \code{VR_NUMBER}. This is a field identifying which 
points are associated withe eachother.}

\item{timeField}{default is \code{POSITION_UTC_DATE}. This is the field which 
will be used to calculate the time differences between records.}

\item{minDist_m}{the default is \code{50}. This is the minimum distance (m) a vessel must move 
from it's last position in order for the record to be kept.  This should be greater than zero to 
avoid records for vessels sitting in port.}

\item{maxBreak_mins}{the default is \code{1440}(1440 minutes corresponds with 24 hours) .  This 
is the maximum time (in mins) that is allowed between positions before a new "trek" is created.}

\item{minKnots}{default is \code{NULL}.  This is the minimum vessel speed that should be include 
in the output(also see \code{speedField})}

\item{maxKnots}{default is \code{NULL}.  This is the maximum vessel speed that should be include 
in the output (also see \code{speedField})}

\item{speedField}{default is \code{"calc"}.  Valid values are "calc" or any field that exists 
within your df}

\item{dropOrphans}{default is \code{TRUE}.  VMS data is primarily used for tracks, but during the 
filtering process, it's possible to end up with singular positions.  By default, these lone 
positions will be discarded.}
}
\value{
a dataframe with an additional "trek" column identifying a number of
discrete paths for each unique value of \code{objField}.
}
\description{
This function takes raw VMS data (i.e. a dataframe having sequential coordinates and 
times) and cleans it.  Data is cleaned by removing records that are no more than \code{minDist_m}
from the previous position.  Additionally, resultant data is grouped into "treks" which can be 
thought of as discrete forays by a vessel.  A new trek occurs when the time between subsequent 
points for a vessel exceeds \code{maxBreak_mins}, which by default is 24 hours (1440 mins).
}
\note{
If a field called  "UPDATE_DATE" exists, it will be used to "break ties" in the case of 
duplicate records (i.e. the more recent record will is retained.
}
\seealso{
Other vms: 
\code{\link{VMS_from_MARFIS}()},
\code{\link{VMS_get_recs}()},
\code{\link{subset_by_time}()}
}
\author{
Mike McMahon, \email{Mike.McMahon@dfo-mpo.gc.ca}
}
\concept{vms}
