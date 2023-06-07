##' Plot a pacea climatic or oceanographic index
##'
##' Temporal plot for a pacea index (`pacea_index`) object, with options for
##' display style, and adding in times of other events (to ask questions such as
##' 'does this rare event coincide with El Niño?'). See examples and vignette TODO.
##'
##' @param obj a `pacea_index` object, which is a time series.
##' @param value which column of `obj` to plot
##' @param xlab x-axis label
##' @param ylab y-axis label, the default is an attribute of the `pacea_index`
##'   object.
##' @param smooth_over_year logical to smooth monthly values over each calendar
##'   year (as per Tetjana Ross' plots, see `?oni` for reference). Note that the
##'   corresponding `date` is for 1st January of each year.
##' @param type usual argument for `plot()`
##' @param style what style of plot:
##' \describe{
##'   \item{"red_blue_bar" (TODO default):}{for red bars above 0 and
##'   blue bars below 0, but need to manually adjust the thickness `lwd` for
##'   width of bars to look good for your specific plot}
##'   \item{"red_blue":}{for a line with filled in colouring for red above 0 and
##'   blue below 0}
##'   \item{"goa":}{TODO (not implemented yet) for Gulf of Alaska Ecosystem
##'   Report style plots}
##'   \item{"plain":}{just a plain line}
##' }
##' @param y_tick_by increment for y-axis ticks
##' @param y_tick_start where to start y-axis tickmarks, set automatically if not
##'   specified (may need to occasionally specify)
##' @param y_tick_end where to end y-axis tickmars, as for `y_tick_start`
##' @param x_tick_extra_years number of extra years to expand around the range
##'   of data for which to add annual tick marks (does not expand the axis)
##' @param start_decade_ticks where to start tickmarks for decades (defaults to
##'   1800 as hard to automate)
##' @param event_years years of the event occurring (e.g. sighting of a rare
##'   shark) to add to the plot (sensible for rare-ish events, to quickly see if
##'   they coincide with the values of the index);
##'   points will be shown for 1st July (Oh Canada!) of the given year, and at
##'   the value of the index to make it easy to quickly spot any
##'   correlation. Assumes `event_years` values are unique.
##'   For more control instead use `event_lub`.
##' @param event_lub dates of events as lubridate objects (only `event_years` or
##'   `event_lub` can be specified). Assumes `event_lub` values are unique.
##' @param event_pch `pch` for events
##' @param event_cex `cex` for events
##' @param event_col `col` for events
##' @param ... optional arguments passed onto `plot()`. Note that the x-axis is
##'   constructed using a lubridate `date` object, so `xlim` needs to be a
##'   `date` object (see example).
##'  @return plot of the time series to the current device (returns nothing)
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' plot(oni)  # TODO add all examples, and wrapper for plotting multiple
##' plot(oni,
##'      xlim = c(lubridate::dmy(01011950),
##'      lubridate::dmy(01012040))) # to expand x-axis
##' plot(npi_monthly,
##'      value = "value")
##'
##' TODO Move this to vignette, keeping final one for the xample
##' The overlaying of events idea was inspired from a lunchtime conversation regarding a
##' rare sighting of a Bluntnose Sixgill Shark by divers in Port Alberni, that garnered
##' [media attention](https://www.timescolonist.com/local-news/divers-encounter-deep-water-shark-in-alberni-inlet-7102038).
##' A lunchtime conversation with Maxime Veilleux led to the question of whether catches
##' of these sharks in BC waters are related to El Niño. Specifically, as the data from
##' the International Pacific Halibut Commission annual longline survey are readily available in the [gfiphc](https://github.com/pbs-assess/gfiphc) package, we looked at years which caught a Bluntnose Sixgill Shark (at a standard station, outside of the Strait of Georgia).
##' library(gfiphc)     # Check runs okay
##' sp_set_counts <- iphc_get_calc_plot_full("bluntnose sixgill shark")
##' bluntnose_caught_years <- unique(filter(sp_set_counts$set_counts, N_it > 0, standard == "Y")$year)
##' plot(oni,
##'      event_years = bluntnose_caught_years,
##'      xlim = c(lubridate::dmy(01011995), lubridate::dmy(01012024)),
##'      lwd = 2)
##' This is not meant to be a definitive analysis, but a demonstration of the plotting options in pacea.
##'
##' TODO keep this one for example,
##' plot(oni,
##'      event_years = c(1996, 2003, 2004, 2006, 2007, 2008, 2009, 2010, 2014, 2016, 2017, 2019),
##'      xlim = c(lubridate::dmy(01011995), lubridate::dmy(01012024)),
##'      lwd = 2)
##' # event_years here relate to years that the IPHC survey caught
##' Bluntnose Sixgill Sharks in BC waters (outside of Strait of Georgia). See vignette
##' for more details and motivation.
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
                             event_years = NULL,
                             event_lub = NULL,
                             event_pch = 20,
                             event_cex = 3,
                             event_col = "grey",
                             ...
                             ){
  stopifnot("value must be a column of the pacea_index object" =
              value %in% names(obj))

  stopifnot("Cannot specify both event_years and event_lub" =
              !(!is.null(event_years) & !is.null(event_lub)))

  stopifnot("event_lub needs to be a Date class (created using lubridate); can use event_years instead for annual events" =
              "Date" %in% class(event_lub) | is.null(event_lub))

  obj_lub <- lubridate_pacea_series(obj = obj,
                                    smooth_over_year = smooth_over_year)

  if(!is.null(event_years)){
    event_lub <- lubridate::ymd(event_years, truncated = 2) + months(6)
  }

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

  if(!is.null(event_lub)){              # Plot the event dates and equivalent y-value

    if(identical(filter(obj_lub,
                           date %in% event_lub)$date,
                    event_lub)){
      # All dates in event_lub appear in obj_lub, so can easily find y-values
      ind <- which(obj_lub$date %in% event_lub)
      points(event_lub,
             obj_lub[ind, ][[value]],    # TODO only checked with $anomaly
             pch = event_pch,
             cex = event_cex,
             col = event_col)
    } else {
      # Need to interpolate, just do to 1 day, as in plot_red_blue()
      obj_lub_interp_list <- approx(x = obj_lub$date,
                                    y = obj_lub[[value]],
                                    xout = seq(min(obj_lub$date),
                                               max(obj_lub$date),
                                               "days"))

      obj_lub_interp <- tibble::tibble(date = obj_lub_interp_list$x,
                                       y = obj_lub_interp_list$y)
      names(obj_lub_interp)[2] <- value
      ind_interp <- which(obj_lub_interp$date %in% event_lub)

      points(event_lub,
             obj_lub_interp[ind_interp, ][[value]],
             pch = event_pch,
             cex = event_cex,
             col = event_col)
    }
  }
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
