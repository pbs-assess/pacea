##' Plot multiple indices, that have been created by the user, together in a similar heatmap style to the buoy SST plot.
##'
##' Takes any number standardised annual indices created using [create_index()] and
##' plots a heatmap style plot.
##'
##'
##' @param ... the indices you want to plot, using names you gave them when
##' using [create_index()], e.g. hake_recruitment_index, etc. They are plotted
##' from top to bottom in the order given.
##' @param years numeric vector of the years to show, default is the range that
##' encompasses all the indices. If some years of an index are not plotted (but
##' were used to create the standardised index), then the mean of the values
##' shown in the plot for that index will obviously not be 0.
##' @param angle_year_labels logical, whether to put the year labels at 45
##' degrees, which is needed to avoid overlapping when there are many years
##' @rdname plot.pacea_buoy_anomalies_list TODO maybe
##' @return a ggplot object (when `return_results = FALSE`) or a list with `plot` and `results`
##' (when `return_results = TRUE`)
##' @export
##' @author Andrew Edwards and Andrea Hilborn
##' @examples
##' \dontrun{
##' # See the vignette for explanations and use of the options.
##' hake_recruitment_index <- create_index(hake_recruitment)   # so standardises it
##' herring_wcvi_recruitment_index <- create_index(herring_recruitment)   # default is WCVI
##' plot_indices(hake_recruitment_index,
##'              herring_wcvi_recruitment index)
##' # hake_rec_over_2010_index <- create_index(hake_recruitment_over_2010,
##' # index_label = "Rec over 2010")   # Just to get it working, values should be
##' # close TODO think about why 2021 values don't match, others seem to
##'
##' # Do recruitment for each herring region, and show on one plot
##' her_wcvi <- create_index(herring_recruitment)
##' her_cc <- create_index(herring_recruitment, herring_region = "CC")
##' her_hg <- create_index(herring_recruitment, herring_region = "HG")
##' her_prd <- create_index(herring_recruitment, herring_region = "PRD")
##' her_sog <- create_index(herring_recruitment, herring_region = "SOG")
##' plot_indices(her_hg, her_prd, her_cc, her_sog, her_wcvi)
##'
##' # Do example with:
##' # return_results = TRUE)
##'
##' }
plot_indices <- function(...,
                         years = NULL,
                         main = NULL,
                         xlab = "Year",
                         ylab = "Index",
                         return_results = FALSE,
                         scale_limits = NULL,
                         angle_year_labels = TRUE){

  # Make a long tibble from the indices in ..., in a similar way to what we did
  # for buoy_sst data, so plotting can be similar.
  index_list <- list(...)
  names(index_list) <- sapply(substitute(list(...))[-1],
                              deparse)

  # Validate that all arguments are of class pacea_standardised_index
  invalid_i <- which(!sapply(index_list,
                             inherits,
                             "pacea_standardised_index"))
  if(length(invalid_i) > 0){
    invalid_names <- names(index_list)[invalid_i]
    stop("Argument(s) ",
         paste0(invalid_names,
                collapse = ", "),
         " must be of class 'pacea_standardised_index'")
  }

  combined_indices <- index_list %>%
    dplyr::bind_rows(.id = "index") %>%
    dplyr::mutate(index = factor(index,
                                 levels = names(index_list)))

  # Get unique index_labels in the order of the index factor levels
  index_label_levels <- combined_indices %>%
    dplyr::distinct(index,
                    index_label) %>%
    dplyr::arrange(index) %>%
    dplyr::pull(index_label)

  combined_indices <- combined_indices %>%
    dplyr::mutate(index_label = factor(index_label,
                                       levels = index_label_levels))

  if(is.null(main)){
    main = paste0("Ecosystem summary plot showing standardised value of each index")
  }

  if(is.null(years)){
    years <- seq(min(combined_indices$year,
                     na.rm = TRUE),
                 max(combined_indices$year,
                     na.rm = TRUE))
  }

  plot_data <- combined_indices %>%
    dplyr::filter(year %in% years,
                  !is.na(value))

  # Create legend label based on sst_plot
  legend_label <- "Standardised anomaly for each index"

  if(is.null(scale_limits)){
    max_abs <- max(abs(plot_data$value),
                   na.rm = TRUE)
    scale_limits <- c(-max_abs,
                      max_abs)
  }


  # Add label column for text display
  plot_data <- plot_data %>%
    dplyr::mutate(label = as.character(round(value, 1)))

  # Colour scale
  color_scale <- ggplot2::scale_fill_gradientn(colours = pals::ocean.balance(20)[3:18],
                                               limits = scale_limits,
                                               name = legend_label)

  indices_plot <-
    plot_data %>%
    ggplot(aes(x = year,
               y = index_label)) +
    geom_tile(aes(fill = value),
              colour = "black") +
    color_scale +
    ggplot2::scale_x_continuous(expand = c(0, 0),
                                name = xlab,
                                breaks = years) +
    ggplot2::scale_y_discrete(expand = c(0, 0),
                              name = ylab,
                              limits = rev) +
    theme(legend.position = "bottom",
          strip.background = element_blank(),
          strip.text = element_text(face = "bold",
                                    size = 12),
          panel.spacing.y = grid::unit(0.1,
                                       "lines"),
          panel.background = element_rect(fill = "white",
                                          colour = NA),
          panel.grid = element_blank()) +
    if (angle_year_labels) {
      theme(axis.text.x = element_text(angle = 45,
                                       hjust = 1,
                                       vjust = 1))
    } +
    ggplot2::geom_text(aes(label = label),
                       size = 3.5) +
    guides(fill = guide_colorbar(barwidth = 15,
                                 title.position = "top",
                                 frame.colour = "black",
                                 ticks.colour = "black")) +
    labs(title = main)
         # caption = "Ooh look at me")

  if(return_results){
    print(indices_plot)
    return(list(plot = indices_plot,
                results = plot_data))
  } else {
    return(indices_plot)
  }
}
