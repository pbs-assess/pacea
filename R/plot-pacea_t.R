##' Plot a pacea time series object
##'
##'  <desc>
##'
##' @param obj a `pacea_t` object, which is a time series.
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'
##' }
plot.pacea_t <- function(obj,
                         value = val,
                         xlab = "Date",
                         ylab = "Value",
                        ...
                         ){
  # TODO extract date-related columns and create the date column correctly
  #  This works for oni

  obj_lub <- dplyr::mutate(obj,
                           date = paste(year,
                                        month,
                                        sep = "-"))
  obj_lub$date <- lubridate::ym(obj_lub$date)

  # TODO could automate name for ylab, by adding it as an attribute (?) to each
  # object.

  plot.default(obj_lub$date,
               obj_lub$val,
               xlab = xlab,
               ylab = ylab,
              ...)
}
