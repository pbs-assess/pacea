##' Plot multiple indices, that have been created by the user, together in a similar heatmap style to the buoy SST plot.
##'
##' Takes any number standardised annual indices created using [create_index()] and
##' plots a heatmap style plot. To display square tiles for each data point,
##' manually adjust your figure
##' dimensions when saving.
##'
##' See the `ecosystem_summary_heatmaps` vignette (best viewed as the rendered
##' version on GitHub) for lots of example uses.
##'
##' @param ... the indices you want to plot, using names you gave them when
##'   using [create_index()], e.g. hake_recruitment_index, etc. They are plotted
##'   from top to bottom in the order given. When the arguments `flip` and
##'   `shift` are used, it is assumed that the first index given is the one of
##'   interest, that may be influenced by the other indices.
##' @param years numeric vector of the years to show, default is the range that
##'   encompasses all the indices. If some years of an index are not plotted (but
##'   were used to create the standardised index), then the mean of the values
##'   shown in the plot for that index will obviously not be 0.
##' @param xlab x-axis label, default is "Year". If `shift` is specified then
##'   automatically becomes "Year of " followed by the `index_label` of the index
##'   specified in `shift[1]`, as it is assumed this is the focus of interest
##'   (e.g. recruitment of a fish stock).
##' @param angle_year_labels logical, whether to put the year labels at 45
##'   degrees, which is needed to avoid overlapping when there are many
##'   years. TODO maybe need the next rdname:
##' @param gap_after character vector (or NULL, the default) of index names
##'   to add a gap after in the plot. For example,
##'   `gap_after = "herring_recruitment_sog_index"` (which is an index you have
##'   created and named using `create_index()`) will add a gap after that index.
##'   Multiple indices can be specified: `gap_after = c("index1", "index2")`.
##' @param gap_size numeric value controlling the size of the gaps in cm
##'   (default 0.2, roughly 1/5 of the box height).
##' @param heatmap_colours numeric vector of colours to use for the heatmap.
##'   Default is `pals::ocean.balance(20)[2:19]`, which provides 18 colours.
##' @param flip character vector (or NULL, the default) of index names. This is
##'   for a stock-specific summary, when the first index represents, say,
##'   recruitment of the stock of interest. The remaining indices are thought to
##'   possibly influence this recruitment. Indices in `flip` are for when a
##'   negative value of the index is thought to have a positive values on the
##'   recruitment. The colours in the heatmap are then reversed for this index,
##'   so that red is 'good for recruitment'. An asterisk is added to the index
##'   label in the figure to identify this. The numeric values shown in the
##'   figure are those of the original index. See the vignette for an
##'   example. Note that if `return_results = TRUE` then results are given in
##'   this way, with the `value` column being the negative of the original index
##'   for those indices given in `flip`.
##' @param shift character vector (or NULL, the default) of index names to
##'   shift along the year axis by the respective value in `shift_years`. Each
##'   index name must be one of those provided in `...`. The idea is that if an
##'   `index_1` is influenced by the value of `index_2` from the previous year,
##'   then we shift `index_2` along by a year so that in the figure the years
##'   line up, making it easy to spot relationships. A plus symbol as added to
##'   the index label to identify this (one plus symbol is added for each year
##'   in the relevant component of `shift_years`). If not NULL, then The
##'   x-axis label is also automatically changed to say `Year of <index_label for
##'   index_x_>`. See the vignette for an example.
##' @param shift_years numeric vector (or NULL, the default) of the same length
##'   as `shift`, specifying the number of years to shift each index in `shift`.
##'   Positive values shift the index forward in time (e.g. a value of 1 means
##'   the value for 1990 will now appear as 1991). If NULL but `shift` is given,
##'   then `shift_years` defaults to `rep(1, length(shift)`, assuming a one-year
##'   shift for each specified index.
##' @return a ggplot object (when `return_results = FALSE`) or a list with `plot` and `results`
##' (when `return_results = TRUE`)
##' @export
##' @author Andrew Edwards and Andrea Hilborn
##' @examples
##' \dontrun{
##' # See the vignette for explanations and use of the options.
##' hake_recruitment_index <- create_index(hake_recruitment)
##' herring_wcvi_recruitment_index <- create_index(herring_recruitment)   # default is WCVI
##' plot_indices(hake_recruitment_index,
##'              herring_wcvi_recruitment_index)
##' # hake_rec_over_2010_index <- create_index(hake_recruitment_over_2010,
##' # index_label = "Rec over 2010")   # Just to get it working, values should be
##' # close TODO think about why 2021 values don't match, others seem to
##'
##'
##' }
plot_indices <- function(...,
                         years = NULL,
                         main = NULL,
                         xlab = "Year",
                         ylab = "Index",
                         return_results = FALSE,
                         scale_limits = NULL,
                         angle_year_labels = TRUE,
                         gap_after = NULL,
                         gap_size = 0.2,
                         heatmap_colours = pals::ocean.balance(20)[2:19],
                         flip = NULL,
                         shift = NULL,
                         shift_years = NULL){

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

  # Shift years for specified indices
  if(!is.null(shift)){
    if(is.null(shift_years)){;
      shift_years <- rep(1,
                         length(shift))
    }

    if(length(shift) != length(shift_years)){
      stop("shift and shift_years must be the same length")
    }

    if(!all(shift %in% names(index_list))){
      stop("All values in shift must be index names from those provided to plot_indices()")
    }

    shift_tibble <- tibble::tibble(index = shift,
                                   shift_years = shift_years)

    combined_indices <- combined_indices %>%
      dplyr::left_join(shift_tibble,
                       by = "index") %>%
      dplyr::mutate(year = dplyr::if_else(!is.na(shift_years),
                                          year + shift_years,
                                          year))

    # Change xlab if user hasn't specified something different from the default
    if(xlab == "Year"){
      xlab = paste0("Year of ",
                    pull(combined_indices[1, "index_label"]))
    }
  }

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

  # Create legend label
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

  # Flip sign of values (so red is 'good for species of interest' but the shown
  #  'label' number is still the value of the orginal index)
  #  and add asterisk for 'flipped' indices
  if(!is.null(flip)){
    if(!all(flip %in% combined_indices$index)){
      stop("flip needs to contain only index names from those provided to plot_indices()")
    }

    plot_data <- plot_data %>%
      dplyr::mutate(value = dplyr::if_else(index %in% flip,
                                           -value,
                                           value),
                    index_label = dplyr::if_else(index %in% flip,
                                                 paste0(index_label, "*"),
                                                 index_label))
  }

  # Add superscript + for shifted indices
  if(!is.null(shift)){
    plot_data <- plot_data %>%
      dplyr::mutate(shift_symbols = dplyr::if_else(!is.na(shift_years),
                                                   strrep("+", shift_years),
                                                   ""),
                    index_label = paste0(index_label,
                                         shift_symbols)) %>%
      dplyr::select(-shift_years,
                    -shift_symbols)
  }

  # Recreate index_label factor levels to preserve order after adding symbols
  if(!is.null(flip) || !is.null(shift)){
    new_levels <- plot_data %>%
      dplyr::distinct(index, index_label) %>%
      dplyr::mutate(index = forcats::fct_inorder(index)) %>%
      dplyr::arrange(index) %>%
      dplyr::pull(index_label)

    plot_data <- plot_data %>%
      dplyr::mutate(index_label = factor(index_label,
                                         levels = new_levels))
  }

  # Create grouping for gaps if gap_after is specified
  if(!is.null(gap_after)){
    if(!all(gap_after %in% combined_indices$index)){
      stop("gap_after needs to be one of your prescribed indices in plot_indices()")
    }
    # Get unique indices with their labels and assign gap groups
    index_groups <- plot_data %>%
      dplyr::distinct(index, index_label) %>%
      dplyr::mutate(
        gap_group = 1 + cumsum(dplyr::lag(index %in% gap_after,
                                          default = FALSE))
      )

    plot_data_with_groups <- plot_data %>%
      dplyr::left_join(index_groups,
                       by = c("index", "index_label"))
  } else {
    plot_data_with_groups <- plot_data %>%
      dplyr::mutate(gap_group = 1)
  }

  # Colour scale
  color_scale <- ggplot2::scale_fill_gradientn(colours = heatmap_colours,
                                               limits = scale_limits,
                                               name = legend_label)

  indices_plot <-
    plot_data_with_groups %>%
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
    {
      if(!is.null(gap_after)){
        ggplot2::facet_grid(gap_group ~ .,
                            scales = "free_y",
                            space = "free_y")
      } else {
        NULL
      }
    } +
    theme(legend.position = "bottom",
          strip.background = element_blank(),
          strip.text = element_text(face = "bold",
                                    size = 12),
          strip.text.y = element_blank(),
          panel.spacing.y = grid::unit(if(!is.null(gap_after)) gap_size else 0.1,
                                       if(!is.null(gap_after)) "cm" else "lines"),
          panel.background = element_rect(fill = "white",
                                          colour = NA),
          panel.grid = element_blank()) +
    {
      if(!is.null(shift) || !is.null(flip)){
        theme(axis.ticks.y = element_blank())
      } else {
        theme()
      }
    } +
    {
      if(angle_year_labels){
        theme(axis.text.x = element_text(angle = 45,
                                         hjust = 1,
                                         vjust = 1))
      } else {
        theme()
      }
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
