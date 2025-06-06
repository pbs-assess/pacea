% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{grid26_depth}
\alias{grid26_depth}
\title{Geospatial pacea grid - BC coastline grid template with depth data}
\format{
A simple features dataframe, with columns \code{x} (the geometry),
\code{mean_depth}, \code{min_depth}, and \code{max_depth}. Depths are negative because
there are a few positive ones (above sea level) along the coastlines, which
we will look at in more detail. NB we will make the depths all positive at
some point as that's more logical (well, except for donig physical
oceanography models).
}
\source{
Generated from running \code{data-raw/depth/depth.R}.
}
\usage{
grid26_depth
}
\description{
TODO Needs updating, plus vignette, when we have time; see Issue 48. A simple
features object of the grid used for interpolation of British Columbia
continental margin (BCCM) model. The grid is divided into two regions with
differing resolutions: a higher resolution inshore 2km x 2km grid and lower
resolution offshore 6km x 6km grid. The boundary between the two regions is
created using the 200m isobath along the continental shelf.
}
\details{
TODOGrid cells were removed where the overlapped land using the 'bc_coast' shapefile in pacea. The grid only covers the Canadian Pacific EEZ region with a 10km buffer.
}
\examples{
\dontrun{
head(grid26_depth)
plot(grid26_depth)  # not great as it shows the grid, this is better:
grid26_depth \%>\%
 ggplot(.) +
 geom_sf(aes(fill=mean_depth),
         colour=NA)

# convert to sf points object - from gri26 help TODO
#library(sf)
#grid26_points <- st_centroid(grid26)
#}

}
\keyword{datasets}
