% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assess_privacy.r
\name{assess_privacy}
\alias{assess_privacy}
\title{assess_privacy}
\usage{
assess_privacy(
  df = NULL,
  grid.shape = "hex",
  lat.field = "LATITUDE",
  lon.field = "LONGITUDE",
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
)
}
\arguments{
\item{df}{a dataframe to be analyzed. If left \code{NULL}, a value for \code{db} should be provided}

\item{grid.shape}{default is \code{"hex"}.  This identifies the shape of the 
you want to aggregate your data into.  The options are "hex" or "square"}

\item{lat.field}{the default is \code{"LATITUDE"}. This is the name of the 
field in \code{df} holding latitude values (in decimal degrees)}

\item{lon.field}{the default is \code{"LONGITUDE"}.  This is the name of the 
field in \code{df} holding longitudevalues (in decimal degrees)}

\item{rule.of}{default is \code{5} Whether or not data can be shown (even 
aggregated) depends on the presence of a threshold number of unique values 
for certain sensitive fields.  This parameter sets that threshold.}

\item{agg.fields}{the default is \code{"KEPT_WT"}.  These are the fields in the data that contain 
the values you want to aggregate (e.g. calculate the mean, sum or count of.  This field needs to be
numeric.}

\item{calculate}{the default is \code{c("MEAN", "COUNT", "SUM")}. These are the 
analytics which should be performed for every field identified in \code{agg.field}.
For example, if KEPT_WT and DISCARD_WT are both identified in \code{agg.field},
then for every resultant aggregated polygon (e.g. hexagon), the mean, count 
and sum of both of these fields is calculated for every polygon.}

\item{sens.fields}{the defaults are \code{NULL}  These are fields
to which the "rule of 5" should be applied. The Treasury Secretariat states that when data is 
shown to the public, certain fields must have at least 5 unique values for these fields 
aggregated together. When run, this function will look at these fields, and calculate how many 
unique values exist for each.  It will then populate a field 'TOTUNIQUE' with the minimum number 
of unique values of all the assessed fields. If this is 5 or more, a field called 'CAN_SHOW' will 
be marked as 'YES' (otherwise it will be 'NO').}

\item{facet.field}{default is \code{NULL}.  In cases like bycatch data, you may have a dataframe
where each row might represent different species.  You probably want a breakdown of each individual
species, rather than summing them all up to get some generic weight of all species combined. This is 
the field that will be used to aggregate data by common values (like Species_Code) .}

\item{key.fields}{default is \code{NULL}.  This is a vector of fields that are required
to uniquely identify each fishing set.  If a \code{facet.field} is provided, the \code{facet.field}, 
\code{key.fields} and \code{agg.fields} are all pulled off of the original data and then merged back 
onto it. The key.fields are instrumental in ensuring that the data is able to get rejoined back to the 
original sets.}

\item{for.public}{default is \code{TRUE}. While calculating the aggregated values within each 
2min cell, this script first establishes whether or not cells within an area have enough unique 
values of sensitive fields to be allowed to show any data at all.  If this parameter is \code{TRUE},
the calculated valued value for areas that cannot be shown will be wiped prior to generating the 
output files.}

\item{create.spatial}{default is \code{TRUE}.  This indicates whether or not to create a gpkg file
containing spatial files for 1) the polygon file (with aggregated values 
for each polygon and an indication of whether or not each polygon meets the 
privacy constraints), and 2) the 2 min gridded data (only for within those 
polygons that meet the privacy constraints).}

\item{create.centroid.csv}{default is \code{FALSE}.  This indicates whether or not a csv  should 
be created for the 2 min gridded data (only for within those polygons that meet the privacy 
constraints).  This is a more portable option than the gpkg file created by the \code{create.spatial}
parameter, and is usable without a GIS.  If this is \code{TRUE} AND \code{create.spatial} is 
\code{TRUE}, then the centroid file will also be added to the generated gpkg file.}

\item{file.id}{default is \code{NULL} Whatever is entered here will be used 
to name the output shapefiles and/or plots.  If nothing is entered, the 
output files will just be named using timestamps.}

\item{agg.poly.shp}{default is \code{NULL}.  This is the shapefile that has 
polygons that should be checked for sufficient unique values of the 
sens.fields.  If NULL, NAFO zones will be used.  Otherwise, a path to any 
polygon shapefile can be provided.}

\item{agg.poly.field}{default is \code{NULL}.  This identifies the field within 
the shapefile provided to agg.poly.shp that should be used to check for 
sufficient unique values of the sens.fields.}

\item{custom.grid}{default is \code{NULL}.  If there is a need to use a custom grid to apply to 
the data,}
}
\value{
a list containing an sf grid layer, an sf overlay later, and if \code{create.spatial==T},
a gpkg spatial file containing these same objects. Additionally, if \code{create.centroid.csv =T},
it can also produce a csv of the centroids of the grid layer (which willl also be loaded into the 
gpkg file).
}
\description{
At this time, data with privacy considerations must be aggregated such that each 
polygon has a minimum of 5 unique values for sensitive fields like Licenses, License Holders, and 
Vessels.  This function takes a dataframe and shapefile and for each polygon in the 
shapefile calculates 1) aggregate values for a number of (user-specified) fields , and 2) 
how many unique values exist in each polygon for each of a number of sensitive fields.
}
\note{
If sensitive fields have names that are different than what is provided in the \code{sen.fields}, 
they will not be detected, or included in the checks.  Please make very sure you correctly 
identify such fields.

It should be also noted that this function can result in spatial files with 100s of columns 
relatively easily when a \code{facet.field} is provided (e.g. for bycatch species).  For example, 
if all 3 default \code{calculate} fields are requested on 3 different \code{agg.fields}, and 
there are 30 unique values in the \code{facet.field}, this will result in (3*3*30 =) 270 
fields plus 3 or 4 additional housekeeping fields.
}
\seealso{
Other privacy: 
\code{\link{aggregator}()},
\code{\link{plot_hex_data}()}
}
\author{
Mike McMahon, \email{Mike.McMahon@dfo-mpo.gc.ca}
}
\concept{privacy}
