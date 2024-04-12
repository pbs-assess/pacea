##' Plot the one of the zooplankton anomalies.
##'
##' Temporal plot for one of the columns of a pacea zooplankton
##' (`pacea_zooplankton`) object, as defined in `?zooplankton_sog`. The specified
##' `species_group` gets passed onto `plot.pacea_index()` as the anomaly to be
##' plotted, and yields a correctly automated y-axis label (that can be user-specified if
##' desired). Other options specified here also get passed onto
##' `plot.pacea_index()` and `plot()`, such as for display style, adding in
##' times of other events, etc. See examples and zooplankton vignette, as well as the
##' climatic and oceanographic indices vignette.
##'
##' @param obj a `pacea_zooplankton` object, which is a tibble with columns
##'   giving `year`, `num_samples` (number of samples), `volume_filtered`,
##'   `total_biomass`, and then 24 species groups of zooplankton (see
##'   `?zooplankton_sog`, currently the only zooplankton data in pacea).
##' @param species_group which column of `obj` to plot, either `total_biomass`
##'   (the default) or one of the 24 species groups described in `?zooplankton_sog`.
##' @param xlab x-axis label, defaults to `Date`
##' @param ylab y-axis label, if `NULL` (the default) then automatically selects
##'   the `axis_name` corresponding to `species_group` in the object
##'   `zooplankton_sog_axis_names`. User can also just provide their own label.
##' @param lwd line thickness to plot the anomalies, defaults to 15 since these
##'   are annual; likely needs tweaking depending on the size of the final
##'   figure.
##' @param mgp_val mgp value for `par()`, first value is slightly less than the
##'   default to fit the superscripts in okay. Resets to existing value after plotting.
##' @param ... optional arguments passed onto `plot.pacea_index()` and
##'   `plot()`. Note that the x-axis is constructed using a lubridate `date`
##'   object, so `xlim` needs to be a  `date` object (see `plot.pacea_index()` example).
##'
##' @return plot of the time series to the current device (returns nothing)
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' # Also see zooplankton vignette
##' plot(zooplankton_sog)    # Default is total_biomass, calls plot.pacea_zooplankton()
##' plot(zooplankton_sog, species_group = "cladocera")  # y-axis name is automated
##' }
plot.pacea_zooplankton <- function(obj,
                             species_group = "total_biomass",
                             xlab = "Date",
                             ylab = NULL,
                             lwd = 15,
                             mgp_val = c(2.5, 1, 0),
                             ...
                             ){

  stopifnot("species_group must be a column of the zooplankton object" =
              species_group %in% names(obj))

  mgp_orig <- par()$mgp
  par(mgp = mgp_val)

  obj_to_plot <- dplyr::select(obj,
                               year,
                               anomaly = eval(species_group))
  if(is.null(ylab)){
    ylab <- filter(zooplankton_sog_axis_names,
                   species_group_name == species_group)$axis_name
  }

  plot.pacea_index(obj_to_plot,
                   ylab = ylab,
                   lwd = lwd,
                   ...)

  par(mgp = mgp_orig)  # Leave things as you found them
  invisible()
}
