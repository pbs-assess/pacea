##' Plot buoy SST data for one or multiple stations
##'
##' Plot buoy SST data for either one station or multiple stations, with various options.
##'
##' @param obj buoy SST data, of class `pacea_buoy_sst`
##' @param stn_id single station to plot, or multiple station ids (as given by
##'   `stn_id` column in `buoy_metadata`). Or "all" to plot all stations. TODO
##'
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' }
plot.pacea_buoy <- function(obj,
                            stn_id = "C46146",    # plot one station if
                                        # specified  # NOT IMPLEMENTED YET, and
                                        # make stn_id
                            years = NULL,    # plot a given vector of years (can
                                        # be just one year), els
                                        # all available if NULL TODO check range works
                            year_highlight = lubridate::year(lubridate::today())
                            ){
  station <- stn_id       # Can't use stn_id == stn_id in upcoming filter
  if(length(station) == 1){
    obj_one_stn <- dplyr::filter(obj,
                                 stn_id == station)
    if(!is.null(years)){
      obj_one_stn <- dplyr::filter(obj_one_stn,
                                   lubridate::year(date) %in% years)
    }
    plot_buoy_sst_single(obj_one_stn,
                         year_highlight = year_highlight)

  } else {
    plot_buoy_sst_multiple(obj,   # TODO
                           stn_id = station,
                           years = years,
                           year_highlight = year_highlight
                           )
  }
}

##' Plot buoy SST data for a single station; called by `plot.pacea_buoy()`
##'
##' <desc>
##'
##' @param obj
##' @param year_highlight
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'
##' }
plot_buoy_sst_single <- function(obj,
                                 year_highlight
                                 ){
  stopifnot("obj input for plot_buoy_sst_single() must just be for one stn_id" =
              length(unique(obj$stn_id)) == 1)

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
    geom_line(data = filter(obj,
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
                                   stn_id,
                                   years,
                                   year_highlight
                                   ){
}
