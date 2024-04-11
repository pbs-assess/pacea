##' Plot the one of the zooplankton anomalies.
##'
##' Temporal plot for one of the columns of a pacea zooplankton
##' (`pacea_zooplankton`) object, as defined in `?zooplankton`. The specified
##' `species_group` gets passed onto `plot.pacea_index()` as the anomaly to be
##' plotted, as do any options specified here for display style, adding in
##' times of other events, etc. See examples and vignette, as well as the
##' climatic and oceanographic indices vignette.
##'
##' @param obj a `pacea_zooplankton` object, which is a tibble with columns
##'   giving `year`, `num_samples` (number of samples), `volume_filtered`,
##'   `total_biomass`, and then 24 species groups of zooplankton (see `?zooplankton`).
##' @param species_group which column of `obj` to plot, either `total_biomass`
##'   (the default) or one of the 24 species groups described in `?zooplankton`.
##' @param xlab x-axis label, defaults to `Date`
##' @param ylab y-axis label, if `NULL` (the default` then automatically selects
##'   the `axis_name` corresponding to `species_group` in the object
##'   `zooplankton_axis_names`. User can also just provide their own label.
##' @param lwd line thickness to plot the anomalies, defaults to 15 since these
##'   are annual; likely needs tweaking depending on the size of the final figure.
##' @param ... optional arguments passed onto `plot.pacea_index()` and
##'   `plot()`. Note that the x-axis is constructed using a lubridate `date`
##'   object, so `xlim` needs to be a  `date` object (see `plot.pacea_index()` example).
##'
##' @return plot of the time series to the current device (returns nothing)
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' plot(oni)
##' plot(oni,
##'      xlim = c(lubridate::dmy(01011950),
##'      lubridate::dmy(01012040))) # to expand x-axis
##' plot(npi_monthly,
##'      value = "value")
##' plot(oni,
##'      event_years = bluntnose_caught_years,
##'      xlim = c(lubridate::dmy(01011995), lubridate::dmy(01012024)),
##'      lwd = 2)
##' }
plot.pacea_zooplankton <- function(obj,
                             species_group = "total_biomass",
                             xlab = "Date",
                             ylab = NULL,
                             lwd = 15,
                             ...
                             ){

  stopifnot("species_group must be a column of the zooplankton object" =
              species_group %in% names(obj))

  obj_to_plot <- dplyr::select(obj,
                               year,
                               anomaly = eval(species_group))
  if(is.null(ylab)){
    ylab <- filter(zooplankton_axis_names,
                   species_group_name == species_group)$axis_name
  }

  plot.pacea_index(obj_to_plot,
                   ylab = ylab,
                   lwd = lwd,
                   ...)
}
