##' Plot a pacea recruitment time series object (currently assumes annual values)
##'
##' Temporal plot for a pacea recruitment time series (of class
##' `pacea_recruitment`) object. The `style` option here (unlike for
##' `plot.pacea_index()` defaults to `no_uncertainty` and gets changed to
##' `uncertainty` if `low` and `high` are columns of `obj`.
##'
##' @param obj a `pacea_recruitment` object, which is a time series. Function
##'   will run on other objects (not give an error) but is not tested on those.
##' @param value the column to plot if no uncertainties, or what to plot as dots
##'   if showing uncertainties (likely always `median`)
##' @param style `no_uncertainty` for plain time series without uncertainty,
##'   gets overridden to have uncertainty bars if `low` and `high` are columns
##'   of `obj`
##' @param uncertainty_bar_col colour for uncertainty bars for certain types of
##'   plot (e.g. estimated fish recruitment)
##' @param y_max maximum y value for certain types of plot (use this if you get
##'   an error when specifying `ylim`)
##' @param add_line_at_1 whether to add a horizontal line at 1 (only sensible for scaled recruitments)
##' @param add_line_at_1_col colour for line at 1
##' @param add_line_at_1_lty line type of line at 1
##' @param ... further options passed onto `plot.default()`
##' @inherit plot.pacea_index
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
##' plot(hake_recruitment_over_2010)  # automatically changes style of plot
##'                                   #  if 'over' is in the object name
##' }
plot.pacea_recruitment <- function(obj,
                                   value = "median",
                                   xlab = "Year",
                                   ylab = attr(obj, "axis_name"),
                                   y_tick_by = 1,
                                   x_tick_extra_years = 20,
                                   start_decade_ticks = lubridate::ymd("1800-01-01",
                                                                 truncated = 2),
                                   style = "no_uncertainty",
                                   uncertainty_bar_col = "blue",
                                   y_max = NULL,
                                   add_line_at_1 = FALSE,
                                   add_line_at_1_col = "darkgreen",
                                   add_line_at_1_lty = 2,
                                   ...
                                   ){
  stopifnot("value must be a column of the pacea_recruitment object" =
            value %in% names(obj))

  stopifnot("function currently assumes annual (not monthly) recruitments" =
            !("month" %in% names(obj)))

  obj_lub <- lubridate_pacea_series(obj = obj)

  if(all(c("low", "high") %in% names(obj))){   # we have uncertainties so plot them
    style = "uncertainty"
  }

  if(grepl("over_2010", deparse(substitute(obj)))){
    add_line_at_1 = TRUE
    # Set these defaults if user hasn't changed from default
    if(uncertainty_bar_col == "blue"){
      uncertainty_bar_col = "red"
    }
    if(is.null(y_max)){
      y_max = 1.2
    }
    if(y_tick_by == 1){
      y_tick_by = 0.1
    }
  }

  if(grepl("over_R0", deparse(substitute(obj)))){
    add_line_at_1 = TRUE
    # Set these defaults if user hasn't changed from default
    if(uncertainty_bar_col == "blue"){
      uncertainty_bar_col = "red"
    }
    if(is.null(y_max)){
      y_max = 10
    }
    # y_tick_by 1 = default is fine
  }

  if(style == "uncertainty"){
    plot_with_uncertainty_discrete(obj_lub,
                                   value = value,
                                   xlab = xlab,
                                   ylab = ylab,
                                   uncertainty_bar_col = uncertainty_bar_col,
                                   y_max = y_max,
                                   add_line_at_1 = add_line_at_1,
                                   add_line_at_1_col = add_line_at_1_col,
                                   add_line_at_1_lty = add_line_at_1_lty,
                                   ...)
  } else {
    plot.default(obj_lub$date,
                 obj_lub[[value]], # [[]] returns a vector not a tibble
                 xlab = xlab,
                 ylab = ylab,
                 ...)
  }

  add_tickmarks(obj_lub,
                y_tick_by = y_tick_by,
                y_tick_start = floor(par("usr")[3]),
                y_tick_end = ceiling(par("usr")[4]),
                x_tick_extra_years = x_tick_extra_years,
                start_decade_ticks = start_decade_ticks)
}
