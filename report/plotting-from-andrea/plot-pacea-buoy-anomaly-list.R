##' Plot anomalies of the buoy seasurface temperature data
##'
##'
##' @param pacea_buoy_anomaly_list object of class `pacea_buoy_anomaly_list`
##' obtained from running `caclulate_anomaly()` on buoy data.
##' @param months numeric vector of months to include (1-12). Default is 4 (April).
##' @return a ggplot object
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' all_buoys_anomalies <- calculate_anomaly(buoy_sst,
##'                              climatology_time = "month")
##' all_buoys_plot <- plot.pacea_buoy_anomaly_list(all_buoys_anomalies)
##' all_buoys_plot
##' }
plot.pacea_buoy_anomaly_list <- function(pacea_buoy_anomaly_list,
                                         months = 4){
                                         # number_shades = 16){ see TODO below
  plot_data <- pacea_buoy_anomaly_list$anomaly %>%
    filter(month %in% months,
           !is.na(sst_anomaly))

  max_abs <- max(abs(plot_data$sst_anomaly),
                 na.rm = TRUE)
  year_range <- seq(min(plot_data$year,
                        na.rm = TRUE),
                    max(plot_data$year,
                        na.rm = TRUE))

  anomaly_plot <-
    plot_data %>%
    ggplot(aes(x = year, y = stn_id)) +
    geom_tile(aes(fill = sst_anomaly),
              colour = "black") +
    scale_fill_gradientn(colours = pals::ocean.balance(20)[3:18],
    # TODO tried this to generalise it, but gives different colour bar and some
    # washed out grey; not bothering for now, colours are good.
    # scale_fill_gradientn(colours = pals::ocean.balance(20)[seq(3, 18, length.out = number_shades)],
                         limits = c(-max_abs, max_abs),
                         name = bquote("Annual SST anomaly ("*degree*C*")")) +
    scale_x_continuous(expand = c(0,0), name = NULL,
                       breaks = year_range) +
    scale_y_discrete(expand = c(0,0), name = NULL) +
    theme(legend.position = "bottom",
          strip.background = element_blank(),
          strip.text = element_text(face = "bold",
                                    size = 12),
          panel.spacing.y = unit(0.1,
                                 "lines")
          ) +
    geom_text(aes(label = round(sst_anomaly, 1)),
              size = 3.5) +
    guides(fill = guide_colorbar(barwidth = 15, title.position = "top",
                                 frame.colour = "black",
                                 ticks.colour = "black")) +
    labs(caption = "Ooh look at me")
  # Baseline period: 1991-2020\nDataset: CoralTemp 5km SST")
}
