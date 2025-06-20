% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_select.R
\name{set_select}
\alias{set_select}
\title{set_select}
\usage{
set_select(
  stationData = NULL,
  stationDataField = NULL,
  strata_sf = NULL,
  strataField = NULL,
  addExtData1_sf = NULL,
  addExtDataFields1 = NULL,
  addExtData2_sf = NULL,
  addExtDataFields2 = NULL,
  avoid_sf = NULL,
  outName = NULL,
  writexls = TRUE,
  writegpkg = TRUE,
  localCRS = 2961,
  minDistNM = 4,
  tryXTimes = 100
)
}
\arguments{
\item{stationData}{default is \code{NULL}.  This is either 1) the path to a csv file or 2) an
existing r dataframe containing the all of the strata for which stations should be generated .  
In addition to the strata, this file must also include the fields  \code{"PRIMARY"}, 
\code{"SECONDARY"}, and \code{"ALTERNATE"}.  These three fields should contain integers 
corresponding with how many of that type of station should be generated.  If no stations of a 
particular type should be generated, a value of NA should be present. Strata where \code{PRIMARY} 
is set to NA will not have any stations generated. Below is an example of how this file might 
look:
> head(stationFile)
  STRATUM PRIMARY ALTERNATE SECONDARY
      5Z1       6         3         4
      5Z2      11         6        NA
      5Z3       5         2         2
      5Z4       4         2         2}

\item{stationDataField}{default is \code{"STRATUM"}.  This is just the name of the field 
that contains the strata.  For the example above, this would be "STRATUM".}

\item{strata_sf}{default is \code{NULL}. This should point to an sf object the strata to use. 
This object must have a field that has values identical to those found in  the 
\code{"stationDataField"} within the \code{"stationData"} file.}

\item{strataField}{default is \code{NULL}.  This is the name of the field within 
\code{strata_sf} that contains the identifier for the strata.  Continuing with the example above,
this field would contain values including "5Z1", "5Z2", "5Z3" and "5Z4".}

\item{addExtData1_sf}{default is \code{NULL}.  If additional information should be added to 
the output for each set, an sf object can be provided here.  For example, one might provide 
a file of NAFO zones or special fishing areas.}

\item{addExtDataFields1}{default is \code{NULL}.  If a value is provided to 
\code{addExtData1_sf}, this should correspond with one or more fields within that file.  If a 
set falls within a particular polygon, the value for that polygon from these fields will be 
provided as part of the output.}

\item{addExtData2_sf}{default is \code{NULL}.  This is identical to addExtData1_sf, but allows 
for including information from an additional sf object.}

\item{addExtDataFields2}{default is \code{NULL}.  If a value is provided to 
\code{addExtData2_sf}, this should correspond with one or more fields within that file.  If a 
set falls within a particular polygon, the value for that polygon from these fields will be 
provided as part of the output.}

\item{avoid_sf}{default is \code{NULL}. This is and sf object containing polygons of the areas 
where stations should not be located.   For example, one might populate a file with areas known 
to contain unexploded ordinance, or areas where bottom contact  is forbidden.  No stations will 
be generated where polygons exist in this file.}

\item{outName}{This is the name of the output file excel that will be generated and/or the name of a 
layer withing a gpkg file.}

\item{writexls}{default is \code{TRUE}. Write the results to an excel file in your working 
directory?}

\item{writegpkg}{default is \code{TRUE}. Write the results to a spatial gpkg file in your 
working directory? (for use in a)}

\item{localCRS}{default is \code{2961}.  In order to create sampling stations, the function 
reprojects any spatial data to a locally appropriate projection.  The default value is UTM Zone 
20N, and is appropriate only for Maritimes data.  Any valid CRS can be entered here, and should 
be appropriate for your data.}

\item{minDistNM}{default is \code{4}. This is the minimum required distance between the sets.  By
default, sets will be no closer than 4 nautical miles from each other.}

\item{tryXTimes}{default is \code{100}  By default, the script will make this many attempts to 
fit the requested number of stations into each strata.}
}
\description{
This function randomly selects stations within particular strata.  A specified 
number of stations of 3 different categories can be selected, and the minimum distance required 
between the stations can be specified.  Additional details about each set can be added (such as 
which NAFO zones or marine protected area a set falls in) can be added through provision of sf 
files to the parameters \code{addExtData1_sf} and/or \code{addExtData2_sf}.   Additionally, 
certain areas can be set to exclude stations the inclusion of an additional sf object.
}
\examples{
\dontrun{
Spring_4X_2025 <- set_select(stationData = "c:/2025/Spring_4X_RM.csv",
                            outName =  "Spring_4X_2025",
                            stationDataField = "STRATUM",
                            strata_sf = Mar.data::Strata_Mar_sf, strataField = "StrataID", 
                            addExtData1_sf = Mar.data::NAFOSubunits_sf, addExtDataFields1 = "NAFO",
                            addExtData2_sf = oceans_areas_sf, 
                            addExtDataFields2 = c("NAME_E","ZONE_E"), 
                            avoid_sf = this_avoid_sf)


Georges_5Z_2025 <- set_select(stationData = "c:/2025/Georges_5Z_RM.csv.csv",
                             outName =  "Georges_5Z_2025",
                             stationDataField = "STRATUM",
                             strata_sf = Mar.data::Strata_Mar_sf, strataField = "StrataID", 
                             addExtData1_sf = Mar.data::NAFOSubunits_sf, addExtDataFields1 = "NAFO",
                             addExtData2_sf = oceans_areas_sf, 
                             addExtDataFields2 = c("NAME_E","ZONE_E"), 
                             avoid_sf = this_avoid_sf)
                       }
}
\author{
Mike McMahon, \email{Mike.McMahon@dfo-mpo.gc.ca}
}
\concept{surveys}
