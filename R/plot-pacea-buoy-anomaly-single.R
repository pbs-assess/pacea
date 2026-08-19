##' Plot anomalies for a single buoy, showing months on the y-axis.
##' Gets called from `plot.pacea_buoy_anomaly_list()` if `stn_id_to_plot` argument for
##' that has length 1.
##'
##' @param pacea_buoy_anomaly_list object of class `pacea_buoy_anomaly_list`
##' obtained from running `calculate_anomaly()` on buoy data.
##' @param stn_id_to_plot single `stn_id` to plot
##' @rdname plot.pacea_buoy_anomaly_list
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
                                           ylab,
                                           use_stn_id_name,
                                           sst_plot = "anomaly",
                                           count_breaks = NULL,
                                           return_results = FALSE,
                                           scale_limits = NULL){

  # Validate sst_plot parameter
  sst_plot <- match.arg(sst_plot, c("anomaly", "mean", "count"))


    if(use_stn_id_name){
      stn_id_to_plot_name <- buoy_metadata$name[match(stn_id_to_plot,
                                                      buoy_metadata$stn_id)]
    } else {
      stn_id_to_plot_name <- stn_id_to_plot
    }

  # Set default count_breaks, plotting all months specified so want the
  # one-month colour bar
  if(is.null(count_breaks)){
    count_breaks <- c(0, 10, 15, 20, 31)
  }

  # Create legend label based on sst_plot
  legend_label <- switch(sst_plot,
                        "anomaly" = bquote("Monthly SST anomaly ("*degree*C*")"),
                        "mean" = bquote("Mean SST ("*degree*C*")"),
                        "count" = "Count of daily SST values")

  if(is.null(main)){
    main_suffix <- switch(sst_plot,
                         "anomaly" = "anomalies",
                         "mean" = "mean values",
                         "count" = "count of observations")
    if(sst_plot == "anomaly"){
      main =
        paste0("Monthly sea-surface temperature ",
               main_suffix,
               " for the buoy at ",
               stn_id_to_plot_name,
               " using climatology from ",
               min(pacea_buoy_anomaly_list$climatology_years),
               " to ",
               max(pacea_buoy_anomaly_list$climatology_years))
    } else {
      main =
        paste0("Monthly sea-surface temperature ",
               main_suffix,
               " for the buoy at ",
               stn_id_to_plot_name)
    }
  }

  # Determine which column to filter NA values from and which column for plotting
  na_check_col <- switch(sst_plot,
                        "anomaly" = "sst_anomaly",
                        "mean" = "sst_mean",
                        "count" = "sst_n")

  plot_data <- pacea_buoy_anomaly_list$anomaly %>%
    dplyr::filter(stn_id %in% stn_id_to_plot,
                  month %in% months,
                  !is.na(.data[[na_check_col]]),
                  if(sst_plot == "count") sst_n > 0 else TRUE) %>%
    dplyr::mutate(month_as_factor = factor(month,
                                           levels = rev(sort(months)),
                                           labels = rev(month.abb[sort(months)])),
                  sst_plot_value = .data[[na_check_col]])
  # TODO do check that again in detail, do some tests

  # For non-anomaly plots, use different color scale (not symmetric)
  if(is.null(scale_limits)){
    if(sst_plot == "anomaly"){
      max_abs <- max(abs(plot_data$sst_plot_value),
                     na.rm = TRUE)
      scale_limits <- c(-max_abs,
                        max_abs)
    } else if(sst_plot == "mean"){
      max_abs <- max(plot_data$sst_plot_value,
                     na.rm = TRUE)
      scale_limits <- c(0,
                        max_abs)
    }
  }

  # Determine which colour scale to use based on sst_plot
  if(sst_plot == "count"){
    # Create a custom color palette: very light yellow to green to navy
    # Number of colors = length(count_breaks) - 1 (number of bins)
    n_colors <- length(count_breaks) - 1
    # Define the palette endpoints and let colorRampPalette generate intermediate colors
    count_colors <- grDevices::colorRampPalette(c("#FFFFE0", "#ADFF2F", "#32CD32", "#00008B"))(n_colors)
    color_scale <- ggplot2::scale_fill_stepsn(colours = count_colors,
                                              breaks = count_breaks,
                                              name = legend_label,
                                              limits = c(min(count_breaks), max(count_breaks)))
  } else {
    color_scale <- ggplot2::scale_fill_gradientn(colours = pals::ocean.balance(20)[3:18],
                                                 limits = scale_limits,
                                                 name = legend_label)
  }

  year_range <- seq(min(plot_data$year,
                        na.rm = TRUE),
                    max(plot_data$year,
                        na.rm = TRUE))

  # Add label column for text display
  if(sst_plot == "count"){
    plot_data <- plot_data %>%
      dplyr::mutate(label = as.character(round(sst_plot_value, 0)))
  } else {
    plot_data <- plot_data %>%
      dplyr::mutate(label = as.character(round(sst_plot_value, 1)))
  }

  anomaly_plot <-
    plot_data %>%
    ggplot(aes(x = year,
               y = month_as_factor)) +
    geom_tile(aes(fill = sst_plot_value),
              colour = "black") +
    color_scale +
    ggplot2::scale_x_continuous(expand = c(0, 0),
                                name = xlab,
                                breaks = year_range) +
    ggplot2::scale_y_discrete(expand = c(0, 0),
                              name = ylab) +
    theme(legend.position = "bottom",
          strip.background = element_blank(),
          strip.text = element_text(face = "bold",
                                    size = 12),
          panel.spacing.y = grid::unit(0.1,
                                       "lines"),
          panel.background = element_rect(fill = "white",
                                          colour = NA),
          panel.grid = element_blank()) +
    ggplot2::geom_text(aes(label = label),
                       size = 3.5) +
    guides(fill = guide_colorbar(barwidth = 15,
                                 title.position = "top",
                                 frame.colour = "black",
                                 ticks.colour = "black")) +
    labs(title = main)

  if(return_results){
    print(anomaly_plot)
    list(plot = anomaly_plot,
         results = plot_data)
  } else {
    return(anomaly_plot)
  }
}

