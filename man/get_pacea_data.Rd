% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-pacea-data.R
\name{get_pacea_data}
\alias{get_pacea_data}
\title{Get large pacea data from repository}
\usage{
get_pacea_data(layer, update = FALSE, ask = interactive(), force = FALSE)
}
\arguments{
\item{layer}{Name of the data object.}

\item{update}{Logical. Would you like to check for a newer version of the layer?}

\item{ask}{Logical. Should the user be asked before downloading the data to local cache? Defaults to the value of interactive().}

\item{force}{Logical. Should download of data be forced? Overrides \code{ask} argument if TRUE.}
}
\value{
Data object requested
}
\description{
Large spatiotemporal pacea data files can be downloaded individually from pacea-data.
}
\details{
Some large pacea data sets are stored in a GitHub repository, pacea-data, and get_pacea_data allows users to choose which data to download locally. Refer to \code{pacea_data} to view the data files available for download. Code adapted from 'bcmaps'
}
\examples{
\dontrun{
get_pacea_data("bccm_surface_temperature")
}

}
