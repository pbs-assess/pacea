##' Plot a pacea time series object
##'
##'  <desc>
##'
##' @param obj a `pacea_t` object, which is a time series.
##' @param value which column of `obj` to plot
##' @param xlab x-axis label
##' @param ylab y-axis label, the default is an attribute of the `pacea_t`
##'   object. TODO Note that this isn't automated yet to refer to anomaly or absolute
##'   values.
##' @param ... optional arguments passed onto `plot()`
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'
##' }
plot.pacea_t <- function(obj,
                         value = "anom",
                         xlab = "Date",
                         ylab = attr(obj, "axis_name"),
                         ...
                         ){
  stopifnot("value must be a column of obj" =
              value %in% names(obj))

  # TODO extract date-related columns automatically and create the date column correctly
  #  This works for oni

  obj_lub <- dplyr::mutate(obj,
                           date = paste(year,
                                        month,
                                        sep = "-"))
  obj_lub$date <- lubridate::ym(obj_lub$date)

  plot.default(obj_lub$date,
               obj_lub[[value]], # [[]] returns a vector not a tibble
               xlab = xlab,
               ylab = ylab,
              ...)
}
