% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-pacea-zooplankton.R
\name{plot.pacea_zooplankton}
\alias{plot.pacea_zooplankton}
\title{Plot the one of the zooplankton anomalies.}
\usage{
\method{plot}{pacea_zooplankton}(
  obj,
  species_group = "total_biomass",
  xlab = "Year",
  ylab = NULL,
  lwd = 15,
  mgp_val = c(2.5, 1, 0),
  ...
)
}
\arguments{
\item{obj}{a `pacea_zooplankton` object, which is a tibble with columns
giving `year`, `num_samples` (number of samples), `volume_filtered`,
`total_biomass`, and then 24 species groups of zooplankton (see
`?zooplankton_sog`, currently the only zooplankton data in pacea).}

\item{species_group}{which column of `obj` to plot, either `total_biomass`
(the default) or one of the 24 species groups described in `?zooplankton_sog`.}

\item{xlab}{x-axis label, defaults to `Year`}

\item{ylab}{y-axis label, if `NULL` (the default) then automatically selects
the `axis_name` corresponding to `species_group` in the object
`zooplankton_sog_axis_names`. User can also just provide their own label.}

\item{lwd}{line thickness to plot the anomalies, defaults to 15 since these
are annual; likely needs tweaking depending on the size of the final
figure.}

\item{mgp_val}{mgp value for `par()`, first value is slightly less than the
default to fit the superscripts in okay. Resets to existing value after plotting.}

\item{...}{optional arguments passed onto `plot.pacea_index()` and
`plot()`. Note that the x-axis is constructed using a lubridate `date`
object, so `xlim` needs to be a  `date` object (see `plot.pacea_index()` example).}
}
\value{
plot of the time series to the current device (returns nothing)
}
\description{
Temporal plot for one of the columns of a pacea zooplankton
(`pacea_zooplankton`) object, as defined in `?zooplankton_sog`. The specified
`species_group` gets passed onto `plot.pacea_index()` as the anomaly to be
plotted, and yields a correctly automated y-axis label (that can be user-specified if
desired). Other options specified here also get passed onto
`plot.pacea_index()` and `plot()`, such as for display style, adding in
times of other events, etc. See examples and zooplankton vignette, as well as the
climatic and oceanographic indices vignette.
}
\examples{
\dontrun{
# Also see zooplankton vignette
plot(zooplankton_sog)    # Default is total_biomass, calls plot.pacea_zooplankton()
plot(zooplankton_sog, species_group = "cladocera")  # y-axis name is automated
}
}
\author{
Andrew Edwards
}
