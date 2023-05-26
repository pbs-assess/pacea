##' Plot a pacea climatic or oceanographic index
##'
##' Temporal plot for a pacea index (`pacea_index`) object, with options for
##' display style.
##'
##' @param obj a `pacea_index` object, which is a time series.
##' @param value which column of `obj` to plot
##' @param xlab x-axis label
##' @param ylab y-axis label, the default is an attribute of the `pacea_index`
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
##'   * "red_blue_bar" (default) for red bars above 0 and
##'   blue bars below 0, but need to manually adjust `lwd` for width of bars
##'   TODO check that
##'   * "red_blue" for colouring red above 0 and
##'   blue below (TODO needs splines or similar to smooth),
##'   TODO to implement:
##'   "goa" for Gulf of Alaska Ecosystem Report style plots; "plain"
##'   for just a line.
##' @param y_tick_by increment for y-axis ticks
##' @param y_tick_start where to start y-axis tickmarks, set automatically if not
##'   specified (may need to occasionally specify)
##' @param y_tick_end where to end y-axis tickmars, as for `y_tick_start`
##' @param x_tick_extra_years number of extra years to expand around the range
##'   of data for which to add annual tick marks (does not expand the axis)
##' @param start_decade_ticks where to start tickmarks for decades (defaults to
##'   1800 as hard to automate)
##' @param ... optional arguments passed onto `plot()`. Note that the x-axis is
##'   constructed using a lubridate `date` object, so `xlim` needs to be a
##'   `date` object (see example).
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
##' }
plot.pacea_index <- function(obj,
                             value = "anomaly",
                             xlab = "Date",
                             ylab = attr(obj, "axis_name"),
                             smooth_over_year = FALSE,
                             type = "l",
                             style = "red_blue_bar",
                             y_tick_by = 0.25,
                             y_tick_start = NULL,
                             y_tick_end = NULL,
                             x_tick_extra_years = 20,
                             start_decade_ticks = lubridate::ymd("1800-01-01",
                                                                 truncated = 2),
                             ...
                             ){
  stopifnot("value must be a column of the pacea_index object" =
            value %in% names(obj))

  obj_lub <- lubridate_pacea_series(obj = obj,
                                    smooth_over_year = smooth_over_year)

  if(style == "red_blue"){
    plot_red_blue(obj_lub,
                  value = value,
                  xlab = xlab,
                  ylab = ylab,
                  type = type,
                  ...)
  } else if(style == "red_blue_bar") {
    plot_red_blue_bar(obj_lub,
                      value = value,
                      xlab = xlab,
                      ylab = ylab,
                      type = type,
                      ...)
  } else {
    plot.default(obj_lub$date,
                 obj_lub[[value]], # [[]] returns a vector not a tibble
                 xlab = xlab,
                 ylab = ylab,
                 type = type,
                 ...)
  }

  add_tickmarks(obj_lub,
                y_tick_by = y_tick_by,
                y_tick_start = y_tick_start,
                y_tick_end = y_tick_end,
                x_tick_extra_years = x_tick_extra_years,
                start_decade_ticks = start_decade_ticks)
}

##' Plot the red/blue style of anomaly plot, linearly interpolating to make
##' smooth (needed when crossing x-axis); internal function called from `plot-pacea_index()`.
##'
##' Adapted from
##' https://stackoverflow.com/questions/74902499/shading-below-line-graph-in-r/74903305#74903305
##' and interpolated.
##'
##' @param obj_lub obj a `pacea_index` object, which is a time series, with a date
##'   column that is the lubridate `date` class.
##' @inherit plot.pacea_index
##' @return plot of time series
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' # see plot.pacea_index()
##' }
plot_red_blue <- function(obj_lub,
                          value,
                          xlab,
                          ylab,
                          type,
                          ...){
  # TODO check if 0 within range, or test it works for all positive anomalies

  # Need to interpolate (do to 1 day) to make the crossing of time axis smooth,
  #  else get red and blue for same point in time:
  obj_lub_interp_list <- approx(x = obj_lub$date,
                                y = obj_lub[[value]],
                                xout = seq(min(obj_lub$date),
                                           max(obj_lub$date),
                                           "days"))

  obj_lub_interp <- tibble::tibble(date = obj_lub_interp_list$x,
                                         y = obj_lub_interp_list$y)
  names(obj_lub_interp)[2] <- value

  obj_lub_interp$y_pos <- ifelse(obj_lub_interp[[value]] >= 0,
                          obj_lub_interp[[value]],
                          0)
  obj_lub_interp$y_neg <- ifelse(obj_lub_interp[[value]] < 0,
                          obj_lub_interp[[value]],
                          0)

  plot(obj_lub_interp$date,
       obj_lub_interp[[value]], # [[]] returns a vector not a tibble
       type = type,
       xlab = xlab,
       ylab = ylab,
       ...)
  abline(h = 0)

  polygon(c(obj_lub_interp$date[1],
            obj_lub_interp$date,
            tail(obj_lub_interp$date, 1)),
          c(0,
            obj_lub_interp$y_pos,
            0),
          col = "red")

  polygon(c(obj_lub_interp$date[1],
            obj_lub_interp$date,
            tail(obj_lub_interp$date, 1)),
          c(0,
            obj_lub_interp$y_neg,
            0),
          col = "blue")
  invisible()
}


##' Plot the red/blue style of anomaly plot as barplots without any smoothing; internal function called from `plot.pacea_index()`.
##'
##' Adapted from `plot_red_blue()`.
##'
##' @param obj_lub obj a `pacea_index` object, which is a time series, with a date
##'   column that is the lubridate `date` class.
##' @inherit plot.pacea_index
##' @return plot of time series
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' # see plot.pacea_index()
##' }
plot_red_blue_bar <- function(obj_lub,
                              value,
                              xlab,
                              ylab,
                              type,
                              ...){
  # TODO check if 0 within range

  obj_lub$y_pos <- ifelse(obj_lub[[value]] >= 0,
                          obj_lub[[value]],
                          0)
  obj_lub$y_neg <- ifelse(obj_lub[[value]] < 0,
                          obj_lub[[value]],
                          0)
  bar_col <- ifelse(obj_lub[[value]] >= 0,
                    "red",
                    "blue")

  plot(obj_lub$date,
       obj_lub[[value]], # [[]] returns a vector not a tibble
       type = "h",
       xlab = xlab,
       ylab = ylab,
       col = bar_col,
       lend = 1,
       ...)
  abline(h = 0)

  # GOA code:
  # segments(topX,topY,topX,e_md+e_sd,lwd=2*SC,col="#FFCC00",lend="square" )
  # TODO. They use spline also, which will likely work for me also; unless we
  # want just single bars for annual values, I'd rather avoid smoothing. Maybe
  # do spline for monthly ones.

  ## polygon(c(obj_lub$date[1],
  ##           obj_lub$date,
  ##           tail(obj_lub$date, 1)),
  ##         c(0,
  ##           obj_lub$y_pos,
  ##           0),
  ##         col = "red")

  ## polygon(c(obj_lub$date[1],
  ##           obj_lub$date,
  ##           tail(obj_lub$date, 1)),
  ##         c(0,
  ##           obj_lub$y_neg,
  ##           0),
  ##         col = "blue")
  invisible()
}
