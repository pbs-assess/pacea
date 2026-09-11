##' Plot multiple indices together in a similar heatmap style to the buoy SST plot.
##'
##' Takes any number of the climatological and oceanographic indices, such as those in
##' `pacea_indices`. For plotting standard time series of one index just use
##' [plot()] which then calls [plot.pacea_index()].
##'
##' Not currently set up to use `npi_monthly` and `npi_annual` as
##' they are absolute values and so need a climatology defined (contact Andrew
##' Edwards if you want that implemented; they also have the only `NAs`, namely
##' Dec 1944 and 1899 respectively). Note that `bi` and `alpi` are annual
##' values (others are monthly), and so only get shown if `months` is
##' `1:12`. Also, since `alpi` has absolute units (of millions of square km) it
##' is normalised here (subtract the mean and divide by the standard deviation,
##' calculate over the years selected). Though other indices are not normalised
##' in this way -- this is likely something to be discussed with a wider audience.
##'
##' @param ... the indices you want to plot, e.g. oni, pdo, etc., any of
##' those listed in `pacea_indices`.
##' @param years numeric vector of the years to show (note that PDO goes back to
##' 1854 to we selected something more recent for the default start value);
##' default end is the current year
##' @rdname plot.pacea_buoy_anomalies_list
##' @return a ggplot object (when `return_results = FALSE`) or a list with `plot` and `results`
##' (when `return_results = TRUE`)
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' # See the vignette for explanations and use of the options.
##' plot_pacea_indices(oni, pdo, mei)
##' xx <- plot_pacea_indices(oni, pdo, mei, alpi, bi, months = 1:12, return_results = TRUE)
##' xx
##' }
plot_pacea_indices <- function(...,
                               months = NULL,
                               years = 1970:(lubridate::year(lubridate::today())),
                               main = NULL,
                               xlab = "Year",
                               ylab = "Index",
                               return_results = FALSE,
                               require_requested_months = NULL,
                               scale_limits = NULL){

  # Check months are consecutive except Dec to Jan
  if(!is.null(months)){
    diffs <- diff(months)
    if(length(diffs) > 0){
      # All diffs should be 1, except possibly one -11 (wrapping over end of year)
      invalid_diffs <- diffs[!(diffs == 1 | diffs == -11)]
      n_wrap_around <- sum(diffs == -11)
      if(length(invalid_diffs) > 0 || n_wrap_around > 1){
        stop("'months' must be consecutive, e.g. 1:5, or like c(11, 12, 1, 2) for wrapping over the end of the year.")
      }
    }
  }


  # Make a long tibble from the indices in ..., in a similar way to what we did
  # for buoy_sst data, so plotting can be similar.
  index_list <- list(...)
  names(index_list) <- sapply(substitute(list(...))[-1],
                              deparse)

  if("npi_monthly" %in% names(index_list) |
       "npi_annual" %in% names(index_list)){
    stop("`plot_index_list()` not set up yet to use `npi_monthly` or `npi_annual`
       because they are absolute values not calculated anomalies; contact lead author if you want to use it")
  }

  combined_indices <- index_list %>%
    dplyr::bind_rows(.id = "index") %>%
    select(index,
           year,
           month,
           # value,# npi_monthly (e.g.) does not have anomaly, so would need value
           anomaly)
  combined_indices

  # If just one index then plot months on the y-axis
  if(length(unique(combined_indices$index)) == 1){
    if(is.null(months)){
      months = 1:12         # Default to plot all months
    }

    anomalies_plot_or_list <- plot_pacea_indices_single(
      # TODO
      pacea_buoy_anomalies_list = pacea_buoy_anomalies_list,
      stn_id_to_plot = stn_id_to_plot,
      months = months,
      main = main,
      xlab = xlab,
      ylab = "Month",
      use_stn_id_name = use_stn_id_name,
      sst_plot = sst_plot,
      count_breaks = count_breaks,
      return_results = return_results,
      scale_limits = scale_limits)

    return(anomalies_plot_or_list)
  }

  if(is.null(months)){
    months = 4         # Default plot of April values, but still allow the
    # single TODO stn_id_to_plot function above to have months specified.
  }

  # Set default require_requested_months
  if(is.null(require_requested_months)){
    require_requested_months <- length(months)
  } else {
    if(require_requested_months > length(months)){
      stop("Need `require_requested_months <= length(months)`")
    }
  }

  # Automate main title
  if(which.max(months) == length(months)){
    # months are increasing and so are in the same year

    if(is.null(main)){
        main =
          ifelse(length(months) == 12,
                 paste0("Average of each index for the full year"),
                 paste0("Average of each index for ",
                        summarise_months(months)))
    }

    plot_data <- combined_indices %>%
      dplyr::filter(month %in% months,
                    year %in% years) %>%
      dplyr::group_by(index,
                      year) %>%
      # plot_value becomes the average over the specified months, no need to keep month column
      dplyr::summarise(
        n_available = sum(!is.na(.data[["anomaly"]])),
        index_plot_value = ifelse(
          n_available >= require_requested_months,
          mean(anomaly,
               na.rm = TRUE),
          NA)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(index_plot_value)) %>%
      dplyr::select(-n_available)

    # If doing the full year then include alpi and bi which are annual, so
    # append in the right format
    if(length(months) == 12){
      if("alpi" %in% names(index_list)){
        alpi_to_add <- alpi %>%
          dplyr::filter(year %in% years)
        # alpi has absolute units of 10^6 km^2, so standardise the anomalies
        # (won't be exactly the same idea as other indices, but brings value more
        # similar to other indices)
        alpi_to_add <- alpi_to_add %>%
          dplyr::mutate(index_plot_value = standardise(anomaly),
                        index = "alpi") %>%
          dplyr::select(index,
                        year,
                        index_plot_value)

        plot_data <- plot_data %>%
          dplyr::bind_rows(alpi_to_add)
      }

      if("bi" %in% names(index_list)){
        bi_to_add <- bi %>%
          dplyr::filter(year %in% years) %>%
          dplyr::mutate(index_plot_value = anomaly,
                        index = "bi") %>%
          dplyr::select(index,
                        year,
                        index_plot_value)

        plot_data <- plot_data %>%
          dplyr::bind_rows(bi_to_add)
      }
    }
  } else {
    # Months are not increasing, for which it is implied a winter average is
    # being calculated that includes Dec and Jan.

    if(is.null(main)){
      main = paste0("Average of each index over the winter months ",
                    summarise_months(months),
                    "; year is that of the Jan")
    }
    plot_data <- combined_indices %>%
      dplyr::filter(month %in% months) %>%
      dplyr::mutate(year_of_january = (year + 1) * (month >= months[1]) +
                      year * (month < months[1])) %>%    # the year of the january for the winter
      dplyr::group_by(index,
                      year_of_january) %>%
      # plot_value becomes the average over the specified months, no need to keep month column
      dplyr::summarise(
        n_available = sum(!is.na(.data[["anomaly"]])),
        index_plot_value = ifelse(
          n_available >= require_requested_months,
          mean(anomaly,
               na.rm = TRUE),
          NA)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(index_plot_value)) %>%
      dplyr::select(-n_available) %>%
      dplyr::rename(year = year_of_january)
  }

  # Create legend label based on sst_plot
  legend_label <- "Anomaly for each index"

  if(is.null(scale_limits)){
    max_abs <- max(abs(plot_data$index_plot_value),
                   na.rm = TRUE)
    scale_limits <- c(-max_abs,
                      max_abs)
  }

  year_range <- seq(min(plot_data$year,
                        na.rm = TRUE),
                    max(plot_data$year,
                        na.rm = TRUE))

  # Add label column for text display
  plot_data <- plot_data %>%
    dplyr::mutate(label = as.character(round(index_plot_value, 1)))

  # Colour scale
  color_scale <- ggplot2::scale_fill_gradientn(colours = pals::ocean.balance(20)[3:18],
                                               limits = scale_limits,
                                               name = legend_label)

  anomalies_plot <-
    plot_data %>%
    dplyr::mutate(index = ifelse(index %in% pacea_indices$Object,
                                 toupper(index),
                                 index)) %>% # capitalise for plotting
    ggplot(aes(x = year,
               y = index)) +
    geom_tile(aes(fill = index_plot_value),
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
         # caption = "Ooh look at me")

  if(return_results){
    print(anomalies_plot)
    return(list(plot = anomalies_plot,
                results = plot_data))
  } else {
    return(anomalies_plot)
  }
}
