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
##' @param style what style of plot -- "red_blue" for colouring red above 0 and
##'   blue below, "goa" for Gulf of Alaska Ecosystem Report style plots, "plain"
##'   for just a line.
##' @param y_tick
##' @param x_tick_extra_years number of extra years to expand around the range
##'   of data for which to add annual tick marks
##' @param ... optional arguments passed onto `plot()`. Note that the x-axis is
##'   constructed using a lubridate `date` object, so `xlim` needs to be a
##'   `date` object (see example).
##' @param ytick interval between minor tick marks on y-axis
##' @return
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
                         type = "l",
                         style = "red_blue",
                         y_tick = 0.25,
                         x_tick_extra_years = 20,
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

  if(style == "red_blue"){
    # TODO check if 0 within range
    # Adapted from https://stackoverflow.com/questions/74902499/shading-below-line-graph-in-r/74903305#74903305
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

  } else {

    plot.default(obj_lub$date,
                 obj_lub[[value]], # [[]] returns a vector not a tibble
                 xlab = xlab,
                 ylab = ylab,
                 type = type,
                 ...)
  }
}



## axis(side = 1, at = seq(1, 123, 10), labels = seq(1900, 2020, 10), las = 1)
## axis(side = 2, at = seq(-6, 6, 0.5), labels = seq(-6, 6, 0.5))
