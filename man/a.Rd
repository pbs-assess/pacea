% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/a.R
\name{a}
\alias{a}
\title{Shorthand for \code{as.data.frame()} for printing all of a tibble.}
\usage{
a(tib)
}
\arguments{
\item{tib}{\code{tibble} to be printed to console, R Markdown file, etc.}
}
\value{
print \code{a} as a data.frame, so show all the rows
}
\description{
Shorthand for \code{as.data.frame()} for printing all of a tibble.
}
\details{
Often want to print all of a (small) tibble, but \code{as.data.frame()}
is cumbersome.
}
\examples{
\dontrun{
# To see all the years (rows of a long tibble):
oni \%>\% a()

# To see all the species (columns of a wide tibble):
tail(zooplankton_sog) \%>\% a()
}
}
\author{
Andrew Edwards
}
