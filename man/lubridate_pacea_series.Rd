% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lubridate-pacea-series.R
\name{lubridate_pacea_series}
\alias{lubridate_pacea_series}
\title{Convert an object that represents a time series (\code{pacea_index} etc.) to have lubridate
year/month column, and smooth over a year if required}
\usage{
lubridate_pacea_series(obj, smooth_over_year = FALSE)
}
\arguments{
\item{obj}{a \code{pacea_index} object, which is a time series.}

\item{smooth_over_year}{logical to smooth monthly values over each calendar
year (as per Tetjana Ross' plots, see \code{?oni} for reference). Note that the
corresponding \code{date} is for 1st January of each year.}
}
\value{
original object with month values smoothed (averaged) over the year if
requested, and a new \code{date} column in lubridate format. Sets a resulting
date to be the first of the month or Jan 1st of that year (as appropriate),
for easy
plotting, but note that these should not be taken as exact dates, which is
why we saved the original objects just as year and/or month, not lubridate
dates.
}
\description{
Called from \code{plot.pacea_index()} and others to give a lubridate column for
easier plotting.
}
\examples{
\dontrun{
lubridate_pacea_series(oni)
lubridate_pacea_series(oni, smooth_over_year=TRUE)

}
}
\author{
Andrew Edwards
}
