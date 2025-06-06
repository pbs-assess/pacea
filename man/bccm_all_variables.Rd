% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shortcuts-get-data-bccm.R
\name{bccm_all_variables}
\alias{bccm_all_variables}
\title{Download all BCCM data}
\source{
Peña, M.A., Fine, I. and Callendar, W. 2019. Interannual variability in primary production and shelf-offshore transport of nutrients along the northeast Pacific Ocean margin. Deep-Sea Research II, doi:10.1016/j.dsr2.2019.104637.
}
\usage{
bccm_all_variables()
}
\value{
downloaded files to \code{pacea_cache()} directory
}
\description{
This function downloads all British Columbia continental margin (BCCM) model data for the BC Pacific region to local drive.
}
\details{
Use \code{bccm_data} To view the full list of available variables. See help files for specific variables for more details on BCCM data (e.g. \code{?bccm_surface_oxygen}).

CAUTION: The BCCM files are large and downloading all data may take a while.
}
\examples{
\dontrun{
bccm_all_variables()
}
}
