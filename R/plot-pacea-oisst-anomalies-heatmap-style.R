##' Plot anomalies of OISST data averaged over a defined area, shown with
##' months on the y-axis in a heatmap style plot.
##'
##' Gets called from `plot.pacea_oisst_anomalies_list()` if `heatmap_style ==
##' TRUE`.
##'
##' @param dat `pacea_oisst_anomalies_list` class object from running
##' `calculate_anomalies()` on an object of class `pacea_oi` (the `sst_month`
##' data).
##' @param area sf object of the area to average the SST over to get a single
##' value for each month (uses any cell of the OISST data that intersects with `area`.
##' @param area_name character string of the name of the area to use in the
##' title of the plot. If `NULL` then no area is specified (but we recommend
##' specifying one for clarity).
##' @param main title for the plot, if left as `NULL` then
##' created automatically, including using `area_name`. May need to manually
##' specify `main` as it is hard to fully automate.
##' @param months numeric vector of months (1-12) to show; default (`NULL`)
##' shows all 12 months.
##' @param xlab x-axis label
##' @param ylab y-axis label
##' @param return_results logical, if `FALSE` (default) returns (plots) the ggplot object only.
##' If `TRUE`, prints the plot and returns a list with both `plot` and `results`.
##'
##' @param scale_limits numeric vector of length 2 specifying the limits for the colour scale.
##' If `NULL` (default), limits are calculated automatically to give a symmetric
##' scale based on the anomalies. If any plotted values are outside of `scale_limits`
##' then they are coloured grey.
##' @return a ggplot object (when `return_results = FALSE`) or a list with `plot` and `results`
##' (when `return_results = TRUE`)
##' @export
##' @author Andrew Edwards and Andrea Hilborn
##' @examples
##' \dontrun{
##' # See the vignette for explanations and use of the options.
##' oisst_anomalies <- calculate_anomalies(oisst_month)
##' plot(oisst_anomalies)         # spatial map of anomalies for most recent much
##' plot(oisst_anomalies, heatmap_style = TRUE)   # mean anomaly for each month
##'                                    # averaged over the whole spatial domain
##' }
plot_pacea_oisst_anomalies_heatmap_style <- function(dat,
                                                     area = NULL,
                                                     area_name = NULL,
                                                     main = NULL,
                                                     months = NULL,
                                                     xlab = "Year",
                                                     ylab = "Month",
                                                     return_results = FALSE,
                                                     scale_limits = NULL){

  legend_label <- bquote("Monthly SST anomaly ("*degree*C*")")

  if(is.null(main)){
      main =
        paste0("Monthly sea-surface temperature anomalies",
               if(!is.null(area_name)){
                 paste0(" averaged over ",
                        area_name)
               },
               " using climatology from ",
               min(dat$climatology_years),
               " to ",
               max(dat$climatology_years))
  }

  if(is.null(months)){      # Or just plot the ones asked for
    months = 1:12
  }

  plot_data <- dat$anomalies %>%
    dplyr::filter(month %in% months) %>%
    dplyr::select(-c("sst",
                     "start_date",
                     "end_date",
                     "clim_value"))

  if(!is.null(area)){
    plot_data <- sf::st_filter(plot_data,
                               area,
                               .predicate = sf::st_intersects)
  }

  plot_data <- sf::st_drop_geometry(plot_data)    # no need for spatial any more

  plot_data <- plot_data %>%
    dplyr::group_by(year,
                    month) %>%
    dplyr::summarise(monthly_anomaly = mean(anom)) %>%  # should not have any
    # NA's, can spot in a figure if any come through
    dplyr::ungroup() %>%
    dplyr::mutate(month_as_factor = factor(month,
                                           levels = rev(sort(months)),
                                           labels =
                                             rev(month.abb[sort(months)]))) %>%
    dplyr::select(-c("month"))
# browser()

  # TODO can remove more for simplicity if any left

  if(is.null(scale_limits)){
      max_abs <- max(abs(plot_data$monthly_anomaly),
                     na.rm = TRUE)
      scale_limits <- c(-max_abs,
                        max_abs)
  }

  color_scale <- ggplot2::scale_fill_gradientn(colours = pals::ocean.balance(20)[3:18],
                                               limits = scale_limits,
                                               name = legend_label)

  year_range <- seq(min(plot_data$year,
                        na.rm = TRUE),
                    max(plot_data$year,
                        na.rm = TRUE))

  # Add label column for text display
  plot_data <- plot_data %>%
    dplyr::mutate(label = as.character(round(monthly_anomaly, 1)))

  anomalies_plot <-
    plot_data %>%
    ggplot(aes(x = year,
               y = month_as_factor)) +
    geom_tile(aes(fill = monthly_anomaly),
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
    print(anomalies_plot)
    list(plot = anomalies_plot,
         results = plot_data)
  } else {
    return(anomalies_plot)
  }
}

