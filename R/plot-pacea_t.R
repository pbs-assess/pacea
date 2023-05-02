##' Plot a pacea time series object
##'
##' Temporal plot for a pacea time series (`pacea_t`) object, with options for
##' display style.
##'
##' @param obj a `pacea_t` object, which is a time series.
##' @param value which column of `obj` to plot
##' @param xlab x-axis label
##' @param ylab y-axis label, the default is an attribute of the `pacea_t`
##'   object. TODO Note that this isn't automated yet to refer to anomaly or absolute
##'   values, though for each time series there is probably only one choice
##' @param smooth_over_year logical to smooth monthly values over each calendar
##'   year (as per Tetjana Ross' plots, see `?oni` for reference). Note that the
##'   corresponding `date` is for 1st January of each year. TODO when
##'   smoothed the red-blue figure isn't quite right (it isn't for monthly also,
##'   but that's not as obvious). See Issue #15.
##' @param type usual argument for `plot()`
##' @param style what style of plot -- HERE TODO mention don't all work properly
##'   but can tweak lwd
##'   for "red_blue_bar" (default) for red bars above 0 and
##'   blue bars below 0, "red_blue" for colouring red above 0 and
##'   blue below (TODO needs splines or similar to smooth), TODO to implement:
##'   "goa" for Gulf of Alaska Ecosystem Report style plots; "plain"
##'   for just a line.
##' @param y_tick increment for y-axis ticks
##' @param x_tick_extra_years number of extra years to expand around the range
##'   of data for which to add annual tick marks
##' @param uncertainty_bar_col colour for uncertainty bars for certain types of
##'   plot (e.g. estimated fish recruitment)
##' @param y_max maximum y value for certain types of plot (use this if you get
##'   the error when specifying `ylim`
##' @param ... optional arguments passed onto `plot()`. Note that the x-axis is
##'   constructed using a lubridate `date` object, so `xlim` needs to be a
##'   `date` object (see example).
##' @param ytick interval between minor tick marks on y-axis
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
##'      value = "val")
##' # TODO add hake examples once in package
##' }
plot.pacea_t <- function(obj,
                         value = "anom",
                         xlab = "Date",
                         ylab = attr(obj, "axis_name"),
                         smooth_over_year = FALSE,
                         type = "l",
                         style = "red_blue_bar",
                         y_tick = 0.25,
                         x_tick_extra_years = 20,
                         uncertainty_bar_col = "blue",
                         y_max = NULL,
                         ...
                         ){
  stopifnot("value must be a column of the pacea_t object" =
            value %in% names(obj))

  if(smooth_over_year){
    stopifnot("to smooth over year you need monthly data (if you have daily we can adapt the code
               to use that); set smooth_over_year = FALSE" =
              "month" %in% names(obj))

    obj_lub <- dplyr::group_by(obj,
                               year) %>%
      dplyr::summarise(across(-month,
                              mean))  # Replace val, anom, and any other
                                              #  non-year non-month column with their
                                              #  annual mean

    obj_lub <- dplyr::mutate(obj_lub,
                             date = lubridate::ymd(year,
                                                   truncated = 2))
                             # sets date to 1st Jan of that year to give a valid
                             #  date; could change to middle of year, but a
                             #  little confusing. year column still retained
                             #  (but object not returned so okay).
  } else {
    if("month" %in% names(obj)){

      # TODO extract date-related columns automatically and create the date column correctly
      #  This works for oni, may need a switch (or function, since may want for
      #  pacea_st also) for years-only. And if make function then use for the
      #  obj_lub line above also.

      obj_lub <- dplyr::mutate(obj,
                               date = paste(year,
                                            month,
                                            sep = "-"))
      obj_lub$date <- lubridate::ym(obj_lub$date)

    } else {
    obj_lub <- dplyr::mutate(obj,
                             date = lubridate::ymd(year,
                                                   truncated = 2))
    }
  }

  if(all(c("low", "high") %in% names(obj))){   # we have uncertainties so plot them
    style = "uncertainty"
  }

  if(style == "red_blue"){
    plot_red_blue(obj_lub,
                  value = value,
                  xlab = xlab,
                  ylab = ylab,
                  type = type,
                  y_tick = y_tick,
                  x_tick_extra_years = x_tick_extra_years,
                  ...)
  } else if(style == "red_blue_bar") {
    plot_red_blue_bar(obj_lub,
                      value = value,
                      xlab = xlab,
                      ylab = ylab,
                      type = type,
                      y_tick = y_tick,
                      x_tick_extra_years = x_tick_extra_years,
                      ...)
  } else if(style == "uncertainty"){
    plot_with_uncertainty(obj_lub,
                          value = value,
                          xlab = xlab,
                          ylab = ylab,
                          type = type,
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



##' Plot estimated recruitment with uncertainty bars for stock assessment
##' results time series; internal function called from `plot.pacea_t()`.
##'
##' Adapted from `make.mcmc.recruitment.plot()` from Pacific Hake assessment.
##'
##' @param obj_lub obj a `pacea_t` object, which is a time series, with a date
##'   column that is the lubridate `date` class.
##' @param value see `plot.pacea_t()`
##' @param xlab see `plot.pacea_t()`
##' @param ylab see `plot.pacea_t()`
##' @param type see `plot.pacea_t()`
##' @param y_tick see `plot.pacea_t()`
##' @param x_tick_extra_years see `plot.pacea_t()`
##' @param uncertainty_bar_col see `plot.pacea_t()`
##' @param y_max see `plot.pacea_t()`
##' @param ... see `plot.pacea_t()`
##' @return plot of time series
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' # see plot.pacea_t()
##' }
plot_with_uncertainty <- function(obj_lub,
                                  value,
                                  xlab,
                                  ylab,
                                  type,
                                  y_tick,
                                  x_tick_extra_years,
                                  uncertainty_bar_col,
                                  y_max,
                                  ...){

  if(is.null(y_max)){
    y_max = max(obj_lub$high)
  }

  plot(obj_lub$date,
       obj_lub[[value]], # [[]] returns a vector not a tibble
       xlab = xlab,
       ylab = ylab,
       pch = 20,
       ylim = c(0, y_max),   # specifying ylim in main plot call won't override this
       ...)

  abline(h = 0, col = "lightgrey")

  segments(x0 = obj_lub$date,
           y0 = obj_lub$low,
           x1 = obj_lub$date,
           y1 = obj_lub$high,
           col = uncertainty_bar_col)

  points(obj_lub$date,
         obj_lub[[value]], # [[]] returns a vector not a tibble
         pch = 20)         # plot points again to be on top of bars

  invisible()
#  if(deparse(substitute(obj)) **contains recruitment
}
