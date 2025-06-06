% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{oni}
\alias{oni}
\title{ONI -- Oceanographic Niño Index}
\format{
A tibble also of class \code{pacea_index} with columns:
\describe{
\item{year:}{year of value}
\item{month:}{month (1 to 12) of value}
\item{val:}{absolute values of three-month averages (preceding, current, and
next month), deg C; note that recent values may change in subsequent
updates -- see details}
\item{anom:}{anomalies based on 30-year base periods that are updated every
5 years, deg C; note that recent values may change in subsequent
updates -- see details}
}
}
\source{
Generated from running \code{data-raw/coastwide-indices/coastwide-indices.R}.
}
\usage{
oni
}
\description{
The Oceanic Niño Index is a monthly index which is one measure of the El
Niño-Southern Oscillation.
}
\details{
The Oceanic Niño Index (ONI) is a 3-month running
mean of sea surface temperature (SST) anomalies in the Niño 3.4 region
(5 deg N to 5 deg S, 120 deg W to 170 deg W) plotted on
the centre month. The SST anomalies are calculated based on 30-year base
periods that are updated every 5 years, which accounts for global warming
and some of the decadal-scale SST variability (as seen in the Pacific
Decadal Oscillation index).
The ONI is provided by the NOAA’s National Weather
Service National Centers for Environmental Prediction CPC:
http://www.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ensoyears.shtml

Preceding paragraph adapted from: Ross, T., and Robert, M. (2022). Normal
temperatures despite strong cool
climate indices and an emerging freshening trend. Pages 23-30 of
Boldt, J.L., Joyce, E., Tucker, S., and Gauthier, S. (Eds.). 2022. State of
the physical, biological and selected fishery resources of Pacific Canadian
marine ecosystems in 2021. Canadian Technical Report of Fisheries and
Aquatic Sciences. 3482 vii+242 p.

The above website is updated automatically on the first Thursday of each
month, and states that:

Because of the high frequency filter applied to the
ERSSTv5 data, ONI values may change up to two months after
the initial "real time" value is posted. Therefore, the most recent ONI
values should be considered an estimate.
On the site, Warm (red) and cold (blue) periods are based on a threshold of
+/- 0.5 deg C for the Oceanic Niño Index (ONI), using a 3-month running mean of
ERSST.v5 SST anomalies in the Niño 3.4 region (5 deg N-5 deg S, 120 deg W
-170 deg W), based on centered 30-year base periods updated every 5 years.
For historical purposes, periods of below and above normal SSTs are colored in
blue and red (on the website) when the threshold is met for a minimum of 5
consecutive overlapping seasons. The ONI is one measure of the El
Niño-Southern Oscillation, and other indices can confirm whether features
consistent with a coupled ocean-atmosphere phenomenon accompanied these periods.

Also see https://www.ncei.noaa.gov/access/monitoring/enso/sst

Associated code adapted from code generously shared by Chris Rooper.
}
\examples{
\dontrun{
oni
plot(oni)
}
}
\author{
Andrew Edwards
}
\keyword{datasets}
