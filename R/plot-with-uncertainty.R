##' Plot medians with uncertainty bars, such as for stock assessment
##' recruitment; internal function called from `plot.pacea_recruitment()`.
##'
##' Adapted from `make.mcmc.recruitment.plot()` from Pacific Hake
##' assessment.
##'
##' @param obj_lub obj a `pacea_recruitment` object, which is a time series, with a date
##'   column that is the lubridate `date` class. Can be absolute or relative
##'   recruitment (scaled by a particular year's recruitment or by assumed
##'   unfished equilibrium recruitment).
##' @inherit plot.pacea_recruitment
##' @return plot of time series with median as circle and bars for uncertainty.
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' # see plot.pacea_recruitment()
##' }
plot_with_uncertainty <- function(obj_lub,
                                  value,
                                  xlab,
                                  ylab,
                                  y_tick,
                                  x_tick_extra_years,
                                  uncertainty_bar_col,
                                  y_max,
                                  add_line_at_1,
                                  add_line_at_1_col,
                                  add_line_at_1_lty,
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

  if(add_line_at_1){
    abline(h = 1,
           col = add_line_at_1_col,
           lty = add_line_at_1_lty)
  }

  invisible()
}
