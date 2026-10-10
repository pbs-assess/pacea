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
