% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/VMS_get_recs.R
\name{VMS_get_recs}
\alias{VMS_get_recs}
\title{VMS_get_recs}
\usage{
VMS_get_recs(usepkg = "roracle", dateStart = NULL, dateEnd = NULL,
  vrnList = NULL, hrBuffer = 4, shp = NULL, shp.field = NULL,
  rowNum = 50000)
}
\arguments{
\item{usepkg}{default is \code{'roracle'}. This indicates whether the 
connection to Oracle should use \code{'rodbc'} or \code{'roracle'} to 
connect.  rodbc is slightly easier to setup, but roracle will extract data 
~ 5x faster.}

\item{dateStart}{default is \code{NULL}. This is the start date ('YYYY-MM-DD') 
for your VMS data search}

\item{dateEnd}{default is \code{NULL}. This is the end date ('YYYY-MM-DD') 
for your VMS data search}

\item{vrnList}{default is \code{NULL}.  A vector or VRNs can be added so 
that the only positions returned match particular vessel(s).}

\item{hrBuffer}{default is \code{4}.  This is the number of hours worth of 
VMS data you would like to pad your area search by.  For example, if a vessel
has a single VMS position in the results, padding it will add additional 
points before and after the intrusion which can illustrate what it was doing 
at the time.}

\item{shp}{default is \code{NULL}.  This is the shapefile that has 
polygons that should be checked for existence of VMS data.}

\item{shp.field}{default is \code{NULL}.  This is the field in the shapefile
that should attached to the reurned data.  This will typically be a field 
that identifies the area(s).}

\item{rowNum}{default is \code{50000}.  In the event of populating a table 
with VMS data, no end date is entered.  If this is the case, this value will 
limit how many VMS records are pulled on each function call.  It is in place l
to prevent crashing your application.}
}
\value{
a DataFrame with the column \code{agg.poly.field} added (if value for 
\code{shp} is supplied)
}
\description{
This function extracts VMS data for a given timespan and area.  
A time buffer can be added returns other points that are not within the area
of interest, but give context to the path.
}
\author{
Mike McMahon, \email{Mike.McMahon@dfo-mpo.gc.ca}
}