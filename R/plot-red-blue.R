##' Plot the red/blue style of anomaly plot, linearly interpolating to make
##' smooth (needed when crossing x-axis); internal function called from `plot.pacea_index()`.
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
