##' Plot a pacea time series object
##'
##'  <desc>
##'
##' @param obj a `pacea_t` object, which is a time series.
##' @param value which column of `obj` to plot
##' @param xlab x-axis label
##' @param ylab y-axis label, the default is an attribute of the `pacea_t`
##'   object. TODO Note that this isn't automated yet to refer to anomaly or absolute
##'   values, though for each time series there is probably only one choice
##' @param smooth_over_year logical to smooth monthly values over each calendar
##'   year (as per Tetjana Ross' plots, see `?oni` for reference). TODO when
##'   smoothed the red-blue figure isn't quite right (it isn't for monthly also,
##'   but that's not as obvious). See Issue #15.
##' @param type usual argument for `plot()`
##' @param style what style of plot -- "red_blue" for colouring red above 0 and
##'   blue below, "goa" for Gulf of Alaska Ecosystem Report style plots, "plain"
##'   for just a line.
##' @param y_tick increment for y-axis ticks
##' @param x_tick_extra_years number of extra years to expand around the range
##'   of data for which to add annual tick marks
##' @param ... optional arguments passed onto `plot()`. Note that the x-axis is
##'   constructed using a lubridate `date` object, so `xlim` needs to be a
##'   `date` object (see example).
##' @param ytick interval between minor tick marks on y-axis
##' @return plot of the time series
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' plot(oni)
##' plot(oni,
##'      xlim = c(lubridate::dmy(01011950),
##'      lubridate::dmy(01012040))) # to expand x-axis
##' }
plot.pacea_t <- function(obj,
                         value = "anom",
                         xlab = "Date",
                         ylab = attr(obj, "axis_name"),
                         smooth_over_year = FALSE,
                         type = "l",
                         style = "red_blue",
                         y_tick = 0.25,
                         x_tick_extra_years = 20,
                         ...
                         ){
  stopifnot("value must be a column of obj" =
            value %in% names(obj))

  if(smooth_over_year){
    stopifnot("to smooth over year you need monthly data (if you have daily we can adapt the code
               to use that; set smooth_over_year = FALSE" =
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
                             # sets year to 1st Jan of that year to give a valid
                             #  date; could change to middle of year, but a
                             #  little confusing.
    } else {

      # TODO extract date-related columns automatically and create the date column correctly
      #  This works for oni, may need a switch (or function, since may want for
      #  pacea_st also) for years-only. And if make function then use for the
      #  obj_lub line above also.

      obj_lub <- dplyr::mutate(obj,
                               date = paste(year,
                                            month,
                                            sep = "-"))
      obj_lub$date <- lubridate::ym(obj_lub$date)

    }



#data %>%
#  mutate(day = floor_date(datetime, "day")) %>%
#  group_by(day) %>%
#  summarize(avg = mean(sed))

  if(style == "red_blue"){

    plot.red_blue(obj_lub,
                  value = value,
                  xlab = xlab,
                  ylab = ylab,
                  type = type,
                  y_tick = y_tick,
                  x_tick_extra_years = x_tick_extra_years,
                  ...)
  } else {
    plot.default(obj_lub$date,
                 obj_lub[[value]], # [[]] returns a vector not a tibble
                 xlab = xlab,
                 ylab = ylab,
                 type = type,
                 ...)
  }
}

##' Plot the red/blue style of anomaly plot; internal function called from `plot-pacea_t()`.
##'
##' Adapted from
##' https://stackoverflow.com/questions/74902499/shading-below-line-graph-in-r/74903305#74903305
##'
##' @param obj_lub obj a `pacea_t` object, which is a time series, with a date
##'   column that is the lubridate `date` class.
##' @param value see `plot.pacea_t()`
##' @param xlab see `plot.pacea_t()`
##' @param ylab see `plot.pacea_t()`
##' @param type see `plot.pacea_t()`
##' @param y_tick see `plot.pacea_t()`
##' @param x_tick_extra_years see `plot.pacea_t()`
##' @param ... see `plot.pacea_t()`
##' @return plot of time series
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' # see plot.pacea_t()
##' }
plot.red_blue <- function(obj_lub,
                          value,
                          xlab,
                          ylab,
                          type,
                          y_tick,
                          x_tick_extra_years,
                          ...){
  # TODO check if 0 within range
  obj_lub$y_pos <- ifelse(obj_lub[[value]] > 0,
                          obj_lub[[value]],
                          0)
  obj_lub$y_neg <- ifelse(obj_lub[[value]] < 0,
                          obj_lub[[value]],
                          0)

  plot(obj_lub$date,
       obj_lub[[value]], # [[]] returns a vector not a tibble
       type = type,
       xlab = xlab,
       ylab = ylab,
       ...)
  abline(h = 0)

  # GOA code:
  # segments(topX,topY,topX,e_md+e_sd,lwd=2*SC,col="#FFCC00",lend="square" )
  # TODO. They use spline also, which will likely work for me also; unless we
  # want just single bars for annual values, I'd rather avoid smoothing. Maybe
  # do spline for monthly ones.

  polygon(c(obj_lub$date[1],
            obj_lub$date,
            tail(obj_lub$date, 1)),
          c(0,
            obj_lub$y_pos,
            0),
          col = "red")

  polygon(c(obj_lub$date[1],
            obj_lub$date,
            tail(obj_lub$date, 1)),
          c(0,
            obj_lub$y_neg,
            0),
          col = "blue")

  min <- min(lubridate::floor_date(obj_lub$date, unit = "year")) - lubridate::years(x_tick_extra_years)
  max <- max(lubridate::ceiling_date(obj_lub$date, unit = "year")) + lubridate::years(x_tick_extra_years)

  axis(1,
       seq(min,
           max,
           by = "years"),
       labels = FALSE,
       tcl = -0.2)
  axis(2,
       seq(floor(par("usr")[3]),
           ceiling(par("usr")[4]),
           by = y_tick),
       labels = FALSE,
       tcl = -0.2)
}
