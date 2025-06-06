% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-pacea-anom.R
\name{plot.pacea_stanom}
\alias{plot.pacea_stanom}
\title{Plot anomaly of BCCM spatiotemporal data layer}
\usage{
\method{plot}{pacea_stanom}(x, months.plot, years.plot, clim.dat, bc = TRUE, eez = TRUE, ...)
}
\arguments{
\item{x}{a BCCM ROMS \code{pacea_stanom} object; output from using \code{calc_anom()}}

\item{months.plot}{months to plot. Defaults to current month (if available)}

\item{years.plot}{years to plot.}

\item{clim.dat}{climatology data, obtained from using \code{calc_clim()}. If used, contours of deviations from climatology will be plotted}

\item{bc}{logical. Should BC coastline layer be plotted? Can only be plotted with one plot layer.}

\item{eez}{logical. Should BC EEZ layer be plotted? Can only be plotted with one plot layer.}

\item{...}{other arguments to be passed on, but not currently used (\code{?ggplot}
says the same thing); this should remove a R-CMD-check warning.}
}
\value{
plot of the spatial data to the current device (returns nothing)
}
\description{
Plot for BCCM ROMS anomaly objects using \code{ggplot()}. A quick visualization of anomaly data (relative to climatology), specifying month(s) and year(s). For more options and configurable plots see vignette.
}
\examples{
\dontrun{
pdata <- bccm_surface_temperature()
anom_data <- calc_anom(pdata)
plot(anom_data)
}
}
