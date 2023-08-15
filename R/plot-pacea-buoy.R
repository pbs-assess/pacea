##' Plot buoy SST data for one or multiple stations
##'
##' Plot buoy SST data for either one station or multiple stations, with various
##' options, with a certain year (defaults to the current year), highlighted in
##' red.
##'
##' @param obj buoy SST data, of class `pacea_buoy_sst`
##' @param stn_id single station to plot, as given by
##'   `stn_id` column in `buoy_metadata`. Default is station C46146 (Halibut
##'   Bank in the Strait of Georgia).
##' @param years vector of given years to plot. If left as NULL then plots all
##'   available years.
##' @param year_highlight numeric of the year to highlight; defaults to current year.
##' @return a ggplot object of the plot
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' plot(buoy_sst)
##' plot(buoy_sst, stn_id = "C46185")
##' }
plot.pacea_buoy <- function(obj,
                            stn_id = "C46146",
                            years = NULL,
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
    stop("`stn_id` has to be a single station; if doing multiple in a panel plot would be useful then contact us and we can implement it")
    #plot_buoy_sst_multiple(obj,
    #                       stn_id = station,
    #                       years = years,
    #                       year_highlight = year_highlight
    #                       )
  }
}

##' Plot buoy SST data for a single station; called by `plot.pacea_buoy()`
##'
##' @param obj buoy SST data, of class `pacea_buoy_sst` for just a single station, called from `plot.pacea_buoy()`
##' @param year_highlight
##' @param title text string for title of plot; defaults to `stn_id` followed by
##'   the buoy's name (e.g. C46146 Halibut Bank), like Andrea Hilborn has.
##' @return ggplot object of the plot
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' # Used when called from plot.pacea_buoy()
##' }
plot_buoy_sst_single <- function(obj,
                                 year_highlight,
                                 title = NULL
                                 ){
  stopifnot("obj input for plot_buoy_sst_single() must just be for one stn_id" =
              length(unique(obj$stn_id)) == 1)

  if(is.null(title)){
     title <- paste(obj[1, ]$stn_id,
                    dplyr::filter(buoy_metadata,
                                  stn_id == obj[1, ]$stn_id)$name)
  }

  g <- ggplot(obj,
              aes(lubridate::yday(date),
                  sst,
                  group = factor(lubridate::year(date)),
                  colour = lubridate::year(date)))

  h <- g + geom_line(na.rm = TRUE) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          strip.background = element_rect(colour = "grey70",
                                          fill = "grey95")) +
    labs(x = "Julian Day",
         colour = "Year",
         title = title) +
  # TODO make month, currently Julian day, see attempts:
  # Also see https://www.r-bloggers.com/2020/04/lubridate-ggplot-date-helpers/
    #    scale_x_date(date_labels = "%b") +   # TODO label by month
    # scale_x_date(labels = date_format("%b")) +
#    scale_x_date(date_breaks="months",
#                 date_labels="%b") +
    scale_colour_viridis_c() +
#     theme_classic() +
    ylab(expression("Mean daily SST " ( degree*C)))

  # Highlight year_highlight:
  hh <- h +
    geom_line(data = filter(obj,
                            lubridate::year(date) ==
                            year_highlight),
              aes(x = lubridate::yday(date),
                  sst),
              colour = "red",
              linewidth = 2)
  hh
}


##' Plot buoy SST data for multiple stations; called by `plot.pacea_buoy_sst()`
##'
##' TODO Develop once done single station. See buoy.Rmd vignette for some of
##' Andrea's code, probably have to combine that with single station code (not
##' just loop round single station).
##'
##' @param obj TODO
##' @param stn_id TODO
##' @param years TODO
##' @param year_highlight TODO
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'
##' }
#plot_buoy_sst_multiple <- function(obj,
#                                   stn_id,
#                                   years,
#                                   year_highlight
#                                   ){
#}
