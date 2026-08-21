##' Plot anomalies of the buoy sea-surface temperature data
##'
##' TODO Need to write this.
##' ##'
##' @param pacea_buoy_anomalies_list object of class `pacea_buoy_anomalies_list`
##' obtained from running `caclulate_anomalies()` on buoy data.
##' @param stn_id_to_plot character vector of station IDs (`stn_id` values OR
##' `name` values ) from the `buoy_sst` data
##' object. If `NULL` (default), anomalies for all buoys (in
##' `pacea_buoy_anomalies_list_object`) are plotted. If specified, only the
##' buoys in this vector are plotted. If `length(stn_id_to_plot == 1)` then anomalies
##' for each month are shown, with January at the top and December at the
##' bottom.
##' @param months numeric vector of months (1-12) to include. If not specified
##' then defaults to 4
##' (April), except when only one `stn_id_to_plot` when all months are plotted (unless specified). Can have more than one month, e.g. 6:9. For a 'winter' average,
##' say Nov-Mar, specify the months as `c(11, 12, 1, 2, 3)`. The winter average
##' anomaly will be calculated and named for the year in which January
##' falls. `months` must be consecutive (except for the overwintering `12, 1` as
##' in the above example).
##' @param main title for the plot, if `NULL` then created automatically,
##' including detailing the months selected. May need to manually specify `main`
##' if many non-consecutive months are chosen (which seems unlikely).
##' @param xlab x-axis label
##' @param ylab y-axis label
##' @param use_stn_id_name logical, whether to use the name of the buoy
##' (e.g. `Middle NOMAD`) or the `stn_id` (e.g. C46004). See `buoy_metadata` for
##' the names.
##' @param sst_plot character, one of:
##'   * `"anomalies"` (default) - plots SST anomalies
##'   * `"mean"` - plots mean SST values for each month
##'   * `"count"` - plots count of daily SST values used to calculate monthly mean
##' @param count_breaks numeric vector of break points for the count plot colour scale.
##' Only used when `sst_plot = "count"`. Default of `NULL` actually defaults to
##' `c(0, 10, 15, 20, 31) * length(months)` if `months` is specified, else it is
##' `c(0, 10, 15, 20, 31)`.
##' @param return_results logical, if `FALSE` (default) returns the plot object only.
##' If `TRUE`, prints the plot and returns a list with both `plot` and `results`.
##' @param require_requested_months numeric, number of months that must be available
##' out of the requested `months` to compute an average anomaly. If `NULL` (default),
##' defaults to `length(months)`. Only used when `length(months) > 1` and
##' `length(stn_id_to_plot) > 1`.
##' @param scale_limits numeric vector of length 2 specifying the limits for the colour scale.
##' If `NULL` (default), limits are calculated automatically based on the data. Only used
##' for non-count plots. If any plotted values are outside of `scale_limits`
##' then they are coloured grey.
##' @return a ggplot object (when `return_results = FALSE`) or a list with `plot` and `results`
##' (when `return_results = TRUE`)
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' # See the vignette for explanations and use of the options.
##' all_buoys_anomalies <- calculate_anomalies(buoy_sst,
##'                                            climatology_time = "month")
##' plot(all_buoys_anomalies)
##' }
plot.pacea_buoy_anomalies_list <- function(pacea_buoy_anomalies_list,
                                           stn_id_to_plot = NULL,
                                           months = NULL,
                                           main = NULL,
                                           xlab = "Year",
                                           ylab = "Buoy",
                                           use_stn_id_name = TRUE,
                                           sst_plot = "anomalies",
                                           count_breaks = NULL,
                                           return_results = FALSE,
                                           require_requested_months = NULL,
                                           scale_limits = NULL){
                                           # number_shades = 16){ see TODO below

  # Validate sst_plot parameter
  sst_plot <- match.arg(sst_plot, c("anomalies", "mean", "count"))

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

  # If stn_id_to_plot[1] does not start with "C4" then all values in
  # stn_id_to_plot vector are assumed to be names. If this is the case then
  # replace each element of stn_id_to_plot with its respective stn_id code (of
  # the form "C4...."). These are found in the data object buoy_metadata, with
  # the name column being the name and stn_id the required stn_id.
  if(!is.null(stn_id_to_plot) && !startsWith(stn_id_to_plot[1],
                                             "C4")){
    stn_id_to_plot_new <- buoy_metadata$stn_id[match(stn_id_to_plot,
                                                     buoy_metadata$name)]
    if(any(is.na(stn_id_to_plot_new))){
      stop("You have mis-spelled at least one buoy name in stn_id")
    }
    stn_id_to_plot <- stn_id_to_plot_new
  }

  # If just one stn_id then plot months on the y-axis
  if(length(stn_id_to_plot) == 1){
    if(is.null(months)){
      months = 1:12         # Default to plot all months
    }

    anomalies_plot_or_list <- plot_pacea_buoy_anomalies_single(
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
    # single stn_id_to_plot function above to have months specified.
  }

  # Set default require_requested_months
  if(is.null(require_requested_months)){
    require_requested_months <- length(months)
  } else {
    if(require_requested_months > length(months)){
      stop("Need `require_requested_months <= length(months)`")
    }
  }


  # Set default count_breaks based on number of months
  if(is.null(count_breaks)){
    count_breaks <- c(0, 10, 15, 20, 31) * ifelse(!is.null(months),
                                                  length(months),
                                                  1)
  }

  # Plot all available stn_id's if not specified
  if(is.null(stn_id_to_plot)){
    stn_id_to_plot = unique(pacea_buoy_anomalies_list$anomalies$stn_id)
  }

  # Automate main title
  if(which.max(months) == length(months)){
    # months are increasing and so are in the same year

    if(is.null(main)){
      main_suffix <- switch(sst_plot,
                           "anomalies" = "anomalies",
                           "mean" = "mean values",
                           "count" = "count of observations")
      if(sst_plot == "anomalies"){
        main =
          paste0("Annual sea-surface temperature ", main_suffix, " for ",
                 summarise_months(months),
                 " from buoys using climatology from ",
                 min(pacea_buoy_anomalies_list$climatology_years),
                 " to ",
                 max(pacea_buoy_anomalies_list$climatology_years),
                 " when available")
      } else {
        main =
          paste0("Annual sea-surface temperature ", main_suffix, " for ",
                 summarise_months(months),
                 " from buoys")
      }
    }

    # Determine which column to filter NA values from
    na_check_col <- switch(sst_plot,
                          "anomalies" = "sst_anomaly",
                          "mean" = "sst_mean",
                          "count" = "sst_n")

    plot_data <- pacea_buoy_anomalies_list$anomalies %>%
      dplyr::filter(stn_id %in% stn_id_to_plot,
                    month %in% months,
                    if(sst_plot == "count") sst_n > 0 else TRUE) %>%
      dplyr::group_by(stn_id,
                      year) %>%
      # plot_value becomes the average over the specified months, no need to keep month column
      dplyr::summarise(
        n_available = sum(!is.na(.data[[na_check_col]])),
        sst_plot_value = ifelse(
          n_available >= require_requested_months,
          mean(if(sst_plot == "count"){
                 sst_n
               } else if(sst_plot == "anomalies"){
                 sst_anomaly
               } else {
                 sst_mean
               }, na.rm = TRUE),
          NA
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(sst_plot_value)) %>%
      dplyr::select(-n_available)
  } else {
    # Months are not increasing, for which it is implied a winter average is
    # being calculated that includes Dec and Jan. TODO think about missing
    # months.

    if(is.null(main)){
      main_suffix <- switch(sst_plot,
                           "anomalies" = "anomalies",
                           "mean" = "mean values",
                           "count" = "count of observations")
      if(sst_plot == "anomalies"){
        main =
          paste0("Annual sea-surface temperature ", main_suffix, " for winter months (",
                 summarise_months(months,
                                  just_first_to_last = TRUE),
                 ") from buoys using climatology from ",
                 min(pacea_buoy_anomalies_list$climatology_years),
                 " to ",
                 max(pacea_buoy_anomalies_list$climatology_years),
                 "; year is that of the Jan")    # no room for 'when available'
      } else {
        main =
          paste0("Annual sea-surface temperature ", main_suffix, " for winter months (",
                 summarise_months(months),
                 ") from buoys; year is the year of the Jan")
      }
    }

    # Determine which column to filter NA values from
    na_check_col <- switch(sst_plot,
                          "anomalies" = "sst_anomaly",
                          "mean" = "sst_mean",
                          "count" = "sst_n")

    plot_data <- pacea_buoy_anomalies_list$anomalies %>%
      dplyr::filter(stn_id %in% stn_id_to_plot,
                    month %in% months,
                    if(sst_plot == "count") sst_n > 0 else TRUE) %>%
      dplyr::mutate(year_of_january = (year + 1) * (month >= months[1]) +
                      year * (month < months[1])) %>%    # the year of the january for the winter
      dplyr::group_by(stn_id,
                      year_of_january) %>%
      # plot_value becomes the average over the specified months, no need to keep month column
      dplyr::summarise(
        n_available = sum(!is.na(.data[[na_check_col]])),
        sst_plot_value = ifelse(
          n_available >= require_requested_months,
          mean(if(sst_plot == "count"){
                 sst_n
               } else if(sst_plot == "anomalies"){
                 sst_anomaly
               } else {
                 sst_mean
               }, na.rm = TRUE),
          NA
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(sst_plot_value)) %>%
      dplyr::select(-n_available) %>%
      dplyr::rename(year = year_of_january)
  }

  # Create legend label based on sst_plot
  legend_label <- switch(sst_plot,
                        "anomalies" = bquote("SST anomaly ("*degree*C*")"),
                        "mean" = bquote("Mean SST ("*degree*C*")"),
                        "count" = "Count of daily SST values")

  # For non-anomaly plots, use different colour scale (not symmetric)
  if(is.null(scale_limits)){
    if(sst_plot == "anomalies"){
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

  if(use_stn_id_name){
    # Replace stn_id with it's name, preserving stn_id order
    # Create a mapping that preserves the original stn_id order  TODO hoping to
    # not need this if I get levels of factors fixed in buoy_metadata
    stn_id_order <- unique(plot_data$stn_id)
    plot_data <-
      dplyr::left_join(plot_data,
                       dplyr::select(buoy_metadata,
                                     stn_id,
                                     name),
                       by = join_by(stn_id)) %>%
      dplyr::select(-c("stn_id")) %>%
      dplyr::rename(stn_id = name) %>%
      dplyr::mutate(stn_id = factor(stn_id,
                                     levels = buoy_metadata$name[match(stn_id_order,
                                                                       buoy_metadata$stn_id)]))
  }

  # Reverse the order of stn_id to plot north to south from top to bottom
  plot_data <- plot_data %>%
    mutate(stn_id = factor(stn_id,
                           levels = rev(levels(stn_id))))

  # Determine which colour scale to use based on sst_plot
  if(sst_plot == "count"){
    # Create a custom color palette: very light yellow to green to navy
    # Number of colors = length(count_breaks) - 1 (number of bins)
    n_colors <- length(count_breaks) - 1
    # Define the palette endpoints and let colorRampPalette generate intermediate colors
    # Use very pale/bright yellow at low end, navy at high end, with greens in middle
    count_colors <- grDevices::colorRampPalette(c("#FFFFE0", "#ADFF2F", "#32CD32", "#00008B"))(n_colors)
    color_scale <- ggplot2::scale_fill_stepsn(colours = count_colors,
                                              breaks = count_breaks,
                                              name = legend_label,
                                              limits = c(min(count_breaks), max(count_breaks)))
  } else {
    color_scale <- ggplot2::scale_fill_gradientn(colours = pals::ocean.balance(20)[3:18],
    # TODO tried this to generalise it, but gives different colour bar and some
    # washed out grey; not bothering for now, colours are good.
    # scale_fill_gradientn(colours = pals::ocean.balance(20)[seq(3, 18, length.out = number_shades)],
                                                limits = scale_limits,
                                                name = legend_label)
  }

  anomalies_plot <-
    plot_data %>%
    ggplot(aes(x = year,
               y = stn_id)) +
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
         # caption = "Ooh look at me")

  if(return_results){
    print(anomalies_plot)
    return(list(plot = anomalies_plot,
                results = plot_data))
  } else {
    return(anomalies_plot)
  }
}
