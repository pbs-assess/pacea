##' TODO Plot a pacea recruitment time series object (currently assumes annual values)
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
##' @param xlab label for x-axis
##' @param ylab label for y-axis (default is automatic from attribute of `obj`)
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
##' @param x_tick_extra_years number of extra years to expand around the range
##'   of data for which to add annual tick marks
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
plot.pacea_biomass <- function(obj,
                               value = "median",
                               xlab = "Year",
                               ylab = attr(obj, "axis_name"),
                               y_tick_by = 1,
                               x_tick_extra_years = 20,
                               start_decade_ticks = lubridate::ymd("1800-01-01",
                                                                   truncated = 2),
                               style = "no_uncertainty",
                               y_max = NULL,
                               uncertainty_shade_col = rgb(0, 0, 1, .1),
                               uncertainty_line_col = "blue",
                               uncertainty_line_lty = 3,
                               median_type = "o",
                               median_pch = 16,
                               median_line_col = "black",
                               median_line_lty = 1,
                               median_line_lwd = 2,
                               ...   # pass onto default plot
                               ){
  stopifnot("value must be a column of the pacea_biomass object" =
            value %in% names(obj))

  stopifnot("function currently assumes annual (not monthly) biomass" =
            !("month" %in% names(obj)))

  obj_lub <- lubridate_pacea_series(obj = obj)

  if(all(c("low", "high") %in% names(obj))){   # we have uncertainties so plot them
    style = "uncertainty"
  }

  if(style == "uncertainty"){
    if(is.null(y_max)){
      y_max = max(obj_lub$high)
    }
    plot_with_uncertainty_continuous(obj_lub,
                                     value = value,
                                     xlab = xlab,
                                     ylab = ylab,
                                     y_tick_by = y_tick_by,
                                     x_tick_extra_years = x_tick_extra_years,
                                     uncertainty_shade_col = uncertainty_shade_col,
                                     uncertainty_line_col = uncertainty_line_col,
                                     uncertainty_line_lty = uncertainty_line_lty,
                                     y_max = y_max,
                                     median_type = median_type,
                                     median_pch = median_pch,
                                     median_line_col = median_line_col,
                                     median_line_lty = median_line_lty,
                                     median_line_lwd = median_line_lwd,
                                     ...)
    # To add B0, adapt something like this from hake:
    #   points(equil.yr, unfished.eq.s[2], pch=16)
    #  arrows(equil.yr, unfished.eq.s[1], equil.yr, unfished.eq.s[3],
    #      angle = 90, code = 3, length = 0.06, col = color)
    #   axis(1,
    #       at = yrs[1],
    #   lab = paste0("Unfished\nequilibrium"),
    #   cex.axis = 0.9,
    #   mgp = c(3,2.5,0))


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
