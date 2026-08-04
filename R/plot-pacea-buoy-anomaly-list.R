##' Plot anomalies of the buoy sea-surface temperature data
##'
##' TODO
##'
##' @param pacea_buoy_anomaly_list object of class `pacea_buoy_anomaly_list`
##' obtained from running `caclulate_anomaly()` on buoy data.
##' @param months numeric vector of months to include (1-12). Default is 4 (April).
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
##' 1:4)   # TODO currently gives the title correctly, but need to change the function
##' }
plot.pacea_buoy_anomaly_list <- function(pacea_buoy_anomaly_list,
                                         months = 4,
                                         main = NULL,
                                         xlab = "Year",
                                         ylab = "Buoy"){
                                         # number_shades = 16){ see TODO below


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
    dplyr::filter(month %in% months,
                  !is.na(sst_anomaly))

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
                         name = bquote("Annual SST anomaly ("*degree*C*")")) +
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

}
