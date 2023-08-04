##' Plot buoy SST data for one or multiple stations
##'
##' Plot buoy SST data for either one station or multiple stations, with various options.
##'
##' @param obj buoy SST data, of class `pacea_buoy_sst`
##' @param station single station to plot, or multiple station ids (as given by
##'   `stn_id` column in `buoy_metadata`). Or "all" to plot all stations. TODO
##'
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' }
plot.pacea_buoy <- function(obj,
                            station = "C46146",    # plot one station if specified
                            years = NULL,    # plot a given year, or range, else all
                            year_highlight = lubridate::year(lubridate::today())
                            ){
  if(length(station == 1)){
    plot_buoy_sst_single(obj,
                         station = station,
                         years = years,
                         year_highlight = year_highlight
                         )
  } else {
    plot_buoy_sst_multiple(obj,
                           station = station,
                           years = years,
                           year_highlight = year_highlight
                           )
  }
}



##' Plot buoy SST data for a single station; called by `plot.pacea_buoy_sst()`
##'  <desc>
##'
##' @param obj
##' @param years
##' @param ...
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' @ontrun{
##' @
##' @}
plot_buoy_sst_single <- function(obj,
                                 station,
                                 years,
                                 year_highlight
                                 ){
  g <- ggplot(obj,
              aes(lubridate::yday(date),
                  sst,
                  group = factor(lubridate::year(date)),
                  colour = lubridate::year(date)))

  h <- g + geom_line() +
    labs(x = "Julian Day",    # TODO make month, currently Julian day
         colour = "Year") +
    scale_colour_viridis_c() +
    theme_classic() +
    ylab(expression("Mean daily SST " ( degree*C)))

  # Highlight year_highlight:
  hh <- h +
    geom_line(data = filter(one,
                            lubridate::year(date) ==
                            year_highlight),    # TODO add option for specifying year
              # (so not always current year, esp in January)
              aes(x = lubridate::yday(date),
                  sst),
              colour = "red",
              linewidth = 2)
  hh
# looks good, needs tome tidying up but good so far
}


##' Plot buoy SST data for multiple stations; called by `plot.pacea_buoy_sst()`
##'
##' TODO Develop once done single station. See buoy.Rmd vignette for some of
##' Andrea's code, probably have to combine that with single station code (not
##' just loop round single station).
##'
##' @param obj
##' @param years
##' @param ...
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' @ontrun{
##' @
##' @}
plot_buoy_sst_multiple <- function(obj,
                                   station,
                                   years,
                                   year_highlight
                                   ){
}
