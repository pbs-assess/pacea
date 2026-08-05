##' Plot anomalies for a single buoy, showing months on the y-axis.
##' Gets called from `plot.pacea_buoy_anomaly_list()` if `stn_id_to_plot` argument for
##' that has length 1.
##'
##' @param pacea_buoy_anomaly_list object of class `pacea_buoy_anomaly_list`
##' obtained from running `calculate_anomaly()` on buoy data.
##' @param stn_id_to_plot station ID to plot
##' @param main title for the plot
##' @param xlab x-axis label
##' @param ylab y-axis label
##' @return a ggplot object
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' TODO
##' all_buoys_anomalies <- calculate_anomaly(buoy_sst,
##'                              climatology_time = "month")
##' single_buoy_plot <- plot_pacea_buoy_anomaly_single(all_buoys_anomalies,
##'                                                    stn_id_to_plot = "C46132")
##' single_buoy_plot
##' }
plot_pacea_buoy_anomaly_single <- function(pacea_buoy_anomaly_list,
                                           stn_id_to_plot,
                                           months,
                                           main,
                                           xlab,
                                           ylab){
  # TODO climatology needed for the explicit buoy, and tweak main
  if(is.null(main)){
    main =
      paste0("Monthly sea-surface temperature anomalies for buoy ",
             stn_id_to_plot,
             " using climatology from ",
             min(pacea_buoy_anomaly_list$climatology_years),
             " to ",
             max(pacea_buoy_anomaly_list$climatology_years))
  }

  plot_data <- pacea_buoy_anomaly_list$anomaly %>%
    dplyr::filter(stn_id %in% stn_id_to_plot,
                  month %in% months,
                  !is.na(sst_anomaly)) %>%
    dplyr::mutate(month_as_factor = factor(month,
                                           levels = rev(sort(months)),
                                           labels = rev(month.abb[sort(months)])))
  # TODO do check that again in detail, do some tests

  max_abs <- max(abs(plot_data$sst_anomaly),
                 na.rm = TRUE)
  year_range <- seq(min(plot_data$year,
                        na.rm = TRUE),
                    max(plot_data$year,
                        na.rm = TRUE))

  anomaly_plot <-
    plot_data %>%
    ggplot(aes(x = year,
               y = month_as_factor)) +
    geom_tile(aes(fill = sst_anomaly),
              colour = "black") +
    scale_fill_gradientn(colours = pals::ocean.balance(20)[3:18],
                         limits = c(-max_abs, max_abs),
                         name = bquote("Monthly SST anomaly ("*degree*C*")")) +
    ggplot2::scale_x_continuous(expand = c(0,0),
                                name = xlab,
                                breaks = year_range) +
    ggplot2::scale_y_discrete(expand = c(0,0),
                              name = ylab) +
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

  anomaly_plot
}

