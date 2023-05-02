##' Plot a pacea recruitment time series object (currently assumes annual values)
##'
##' Temporal plot for a pacea recruitment time series (of class
##' `pacea_recruitment`) object.
##'
##' @param obj a `pacea_recruitment` object, which is a time series.
##' @inherit plot.pacea_index
##' @param x_tick_extra_years number of extra years to expand around the range
##'   of data for which to add annual tick marks
##' @param uncertainty_bar_col colour for uncertainty bars for certain types of
##'   plot (e.g. estimated fish recruitment)
##' @param y_max maximum y value for certain types of plot (use this if you get
##'   an error when specifying `ylim`)
##' @return plot of the time series as median with bars showing uncertainty (if
##'   `low` and `high` are columns of `obj) to the current device; returns nothing.
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' plot(hake_recruitment)
##' plot(hake_recruitment,
##'      xlim = c(lubridate::dmy(01011950),
##'               lubridate::dmy(01012040))) # to expand x-axis
##' }
##'
##' assume these are annual values
plot.pacea_recruitment <- function(obj,
                                   value = "median",
                                   xlab = "Year",
                                   ylab = attr(obj, "axis_name"),
                                   smooth_over_year = FALSE,
                                   y_tick = 0.25,
                                   x_tick_extra_years = 20,
                                   uncertainty_bar_col = "blue",
                                   y_max = NULL,
                                   ...
                                   ){
  stopifnot("value must be a column of the pacea_t object" =
            value %in% names(obj))

  stopifnot("function currently assumes annual (not monthly) recruitments" =
            !("month" %in% names(obj)))

  obj_lub <- lubridate_pacea_series(obj = obj)

  if(all(c("low", "high") %in% names(obj))){   # we have uncertainties so plot them
    style = "uncertainty"
  }

  if(style == "uncertainty"){
    plot_with_uncertainty(obj_lub,
                          value = value,
                          xlab = xlab,
                          ylab = ylab,
                          y_tick = y_tick,
                          x_tick_extra_years = x_tick_extra_years,
                          uncertainty_bar_col = uncertainty_bar_col,
                          y_max = y_max,
                          ...)
  } else {
    plot.default(obj_lub$date,
                 obj_lub[[value]], # [[]] returns a vector not a tibble
                 xlab = xlab,
                 ylab = ylab,
                 type = type,
                 y_tick = y_tick,
                 x_tick_extra_years = x_tick_extra_years,
                 ...)
  }
}
