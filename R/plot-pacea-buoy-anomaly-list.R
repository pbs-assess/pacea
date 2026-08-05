##' Plot anomalies of the buoy sea-surface temperature data
##'
##' TODO Need to clearly explain methods and run them by someone. And put checks
##' in for there being enough data in each month. e.g. months = 12 has empty for
##' C46132 but not for months 11:12, in year 2015. Get code working then figure
##' out the many caveats.
##'
##' @param pacea_buoy_anomaly_list object of class `pacea_buoy_anomaly_list`
##' obtained from running `caclulate_anomaly()` on buoy data.
##' @param stn_id_to_plot character vector of station IDs from the `buoy_sst` data
##' object. If `NULL` (default), anomalies for all buoys (in
##' `pacea_buoy_anomaly_list_object`) are plotted. If specified, only the
##' buoys in this vector are plotted. If `length(stn_id_to_plot == 1)` then anomalies
##' for each month are shown, with January at the top and December at the bottom.
##' @param months numeric vector of months to include (1-12). If not specified
##' then defaults to 4
##' (April), except when only one `stn_id_to_plot` when all months are plotted (unless specified). Can have more than one month, e.g. 6:9. For a 'winter' average,
##' say Nov-Mar, specify the months as `c(11, 12, 1, 2, 3)`. The winter average
##' anomaly will be calculated and named for the year in which January falls. TODO
##' @param main title for the plot, if `NULL` then created automatically,
##' including detailing the months selected. May need to manually specify `main`
##' if many non-consecutive months are chosen (which seems unlikely).
##' @param xlab x-axis label
##' @param ylab y-axis label
##' @return a ggplot object
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' all_buoys_anomalies <- calculate_anomaly(buoy_sst,
##'                              climatology_time = "month")
##' all_buoys_plot <- plot.pacea_buoy_anomaly_list(all_buoys_anomalies)
##' all_buoys_plot
##'
##' all_buoys_plot <- plot.pacea_buoy_anomaly_list(all_buoys_anomalies, months =
##' 1:4)
##' # TODO figure out that
##' # all_buoys_plot <- plot(all_buoys_anomalies, months = 1:12)
##' # and
##' # all_buoys_plot_2 <- plot(all_buoys_anomalies, months = 12:1)
##' # are correctly different. Latter should really give an error.
##' # TODO currently gives the title correctly, but need to change the function
##' }
plot.pacea_buoy_anomaly_list <- function(pacea_buoy_anomaly_list,
                                         stn_id_to_plot = NULL,
                                         months = NULL,
                                         main = NULL,
                                         xlab = "Year",
                                         ylab = "Buoy"){
                                         # number_shades = 16){ see TODO below

  # Add a stop() condition that if months are not sequential (Dec Jan is okay) then
  # main needs to be specified TODO Also 12:1 should not be allowed.

  if(length(stn_id_to_plot) == 1){
    if(is.null(months)){
      months = 1:12         # Default to plot all months
    }

    anomaly_plot <- plot_pacea_buoy_anomaly_single(pacea_buoy_anomaly_list = pacea_buoy_anomaly_list,
                                   stn_id_to_plot = stn_id_to_plot,
                                   months = months,
                                   main = main,
                                   xlab = xlab,
                                   ylab = "Month")
    return(anomaly_plot)
  }

  if(is.null(months)){
    months = 4         # Default plot of April values, but still allow the
    # single stn_id_to_plot function above to have months specified.
  }

  if(is.null(stn_id_to_plot)){                 # Plot all of them available
    stn_id_to_plot = unique(pacea_buoy_anomaly_list$anomaly$stn_id)
  }

  if(which.max(months) == length(months)){
    # months are increasing and so are in the same year

    if(is.null(main)){
      main =
        paste0("Annual sea-surface temperature anomalies for ",
               summarise_months(months),
               " from buoys using climatology from ",
               min(pacea_buoy_anomaly_list$climatology_years),
               " to ",
               max(pacea_buoy_anomaly_list$climatology_years),
               " when available")
    }

    plot_data <- pacea_buoy_anomaly_list$anomaly %>%
      dplyr::filter(stn_id %in% stn_id_to_plot,
                    month %in% months,
                    !is.na(sst_anomaly)) %>%
      dplyr::group_by(stn_id,
                      year) %>%
      # sst_anomaly becomes the average over the specified months, no need to keep
      # month column

      dplyr::summarise(sst_anomaly = mean(sst_anomaly)) %>%
      dplyr::ungroup()
  } else {
    # Months are not increasing, for which it is implied a winter average is
    # being calculated that includes Dec and Jan. TODO think about missing
    # months, need a condition for having enough (as elsewhere)

    if(is.null(main)){
      main =
        paste0("Annual sea-surface temperature anomalies for winter months (",
               summarise_months(months),
               ") from buoys using climatology from ",
               min(pacea_buoy_anomaly_list$climatology_years),
               " to ",
               max(pacea_buoy_anomaly_list$climatology_years),
               " when available; year is the year of the Jan")
    }

    plot_data <- pacea_buoy_anomaly_list$anomaly %>%
      dplyr::filter(stn_id %in% stn_id_to_plot,
                    month %in% months,
                    !is.na(sst_anomaly)) %>%
      dplyr::mutate(year_of_january = (year + 1) * (month >= months[1]) +
                      year * (month < months[1])) %>%    # the year of the january
    # for the winter
      dplyr::group_by(stn_id,
                      year_of_january) %>%
      # sst_anomaly becomes the average over the specified months, no need to keep
      # month column
      dplyr::summarise(sst_anomaly = mean(sst_anomaly)) %>%
      dplyr::ungroup() %>%
      dplyr::rename(year = year_of_january)
  }

  max_abs <- max(abs(plot_data$sst_anomaly),
                 na.rm = TRUE)
  year_range <- seq(min(plot_data$year,
                        na.rm = TRUE),
                    max(plot_data$year,
                        na.rm = TRUE))

  anomaly_plot <-
    plot_data %>%
    ggplot(aes(x = year,
               y = stn_id)) +
    geom_tile(aes(fill = sst_anomaly),
              colour = "black") +
    scale_fill_gradientn(colours = pals::ocean.balance(20)[3:18],
    # TODO tried this to generalise it, but gives different colour bar and some
    # washed out grey; not bothering for now, colours are good.
    # scale_fill_gradientn(colours = pals::ocean.balance(20)[seq(3, 18, length.out = number_shades)],
                         limits = c(-max_abs, max_abs),
                         name = bquote("SST anomaly ("*degree*C*")")) +
    ggplot2::scale_x_continuous(expand = c(0,0), name = xlab,
                       breaks = year_range) +
    ggplot2::scale_y_discrete(expand = c(0,0), name = ylab) +
    theme(legend.position = "bottom",
          strip.background = element_blank(),
          strip.text = element_text(face = "bold",
                                    size = 12),
          panel.spacing.y = grid::unit(0.1,
                                       "lines"),
          panel.background = element_rect(fill = "white", colour = NA),
          panel.grid = element_blank()) +
    ggplot2::geom_text(aes(label = round(sst_anomaly, 1)),
                       size = 3.5) +
    guides(fill = guide_colorbar(barwidth = 15, title.position = "top",
                                 frame.colour = "black",
                                 ticks.colour = "black")) +
    labs(title = main)
         # caption = "Ooh look at me")

  anomaly_plot
}
