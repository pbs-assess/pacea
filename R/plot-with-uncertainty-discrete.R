##' Plot medians with uncertainty bars, such as for stock assessment
##' recruitment; called from `plot.pacea_recruitment()`.
##'
##' Used for data for which the years are kind of discrete and so should not be
##' joined up (like for recruitment, as opposed to biomass); helps distinguish
##' plots also. Adapted from `make.mcmc.recruitment.plot()` from Pacific Hake
##' assessment.
##'
##' @param obj_lub obj a `pacea_recruitment` object, which is a time series, with a date
##'   column that is the lubridate `date` class. Can be absolute or relative
##'   recruitment (scaled by a particular year's recruitment or by assumed
##'   unfished equilibrium recruitment) or recruitment deviations.
##' @inherit plot.pacea_recruitment
##' @export
##' @return plot of time series with median as circle and bars for uncertainty.
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' # see plot.pacea_recruitment()
##' }
plot_with_uncertainty_discrete <- function(obj_lub,
                                           value,
                                           xlab,
                                           ylab,
                                           x_tick_extra_years,
                                           uncertainty_bar_col,
                                           y_min = 0,
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
       ylim = c(y_min, y_max),   # specifying ylim in main plot call won't override this
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
