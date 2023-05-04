##' Add tickmarks to an existing plot
##'
##' Add sensible smaller (unlabelled) tickmarks to both axes of an existing
##' pacea temporal plot. Called from `plot.pacea_index()`,
##' `plot.pacea_recruitment()` etc. Is exported but unlikely to be needed externally.
##'
##' @param obj_lub obj a `pacea_index` object, which is a time series, with a date
##'   column that is the lubridate `date` class.
##' @inherit plot.pacea_index
##' @return adds tickmarks to an existing plot
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' plot.pacea_index(oni)   # see end of that function for usage
##' }
add_tickmarks <- function(obj_lub,
                          y_tick_by,
                          y_tick_start,
                          y_tick_end,
                          x_tick_extra_years,
                          start_decade_ticks){
  min <- min(lubridate::floor_date(obj_lub$date,
                                   unit = "year")) -
    lubridate::years(x_tick_extra_years)

  max <- max(lubridate::ceiling_date(obj_lub$date, unit = "year")) +
    lubridate::years(x_tick_extra_years)

  if(is.null(y_tick_start)){
    y_tick_start <- floor(par("usr")[3])
  }
  if(is.null(y_tick_end)){
    y_tick_end  <- ceiling(par("usr")[4])
  }


  # Small ticks every year
  axis(1,
       seq(min,
           max,
           by = "years"),
       labels = FALSE,
       tcl = -0.2)

  # Slightly larger ticks every decade (since not all get labelled automatically)
  axis(1,
       seq(start_decade_ticks,
           max,
           by = "10 years"),
       labels = FALSE,
       tcl = -0.3)

  axis(2,
       seq(y_tick_start,
           y_tick_end,
           by = y_tick_by),
       labels = FALSE,
       tcl = -0.2)
}
