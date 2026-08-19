##' Plot anomalies of the buoy sea-surface temperature data
##'
##' TODO Need to clearly explain methods and run them by someone. And put checks
##' in for there being enough data in each month. e.g. months = 12 has empty for
##' C46132 but not for months 11:12, in year 2015. Get code working then figure
##' out the many caveats.
##'
##' @param pacea_buoy_anomaly_list object of class `pacea_buoy_anomaly_list`
##' obtained from running `caclulate_anomaly()` on buoy data.
##' @param stn_id_to_plot character vector of station IDs (`stn_id` values OR
##' `name` values ) from the `buoy_sst` data
##' object. If `NULL` (default), anomalies for all buoys (in
##' `pacea_buoy_anomaly_list_object`) are plotted. If specified, only the
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
##'   * `"anomaly"` (default) - plots SST anomalies
##'   * `"mean"` - plots mean SST values for each month
##'   * `"count"` - plots count of daily SST values used to calculate monthly mean
##' @param count_breaks numeric vector of break points for the count plot colour scale.
##' Only used when `sst_plot = "count"`. Default of `NULL` actually defaults to
##' `c(0, 10, 15, 20, 31) * length(months)` if `months` is specified, else it is
##' `c(0, 10, 15, 20, 31)`.
##' @param return_results logical, if `FALSE` (default) returns the plot object only.
##' If `TRUE`, prints the plot and returns a list with both `plot` and `results`.
##' @return a ggplot object (when `return_results = FALSE`) or a list with `plot` and `results`
##' (when `return_results = TRUE`)
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
                                         ylab = "Buoy",
                                         use_stn_id_name = TRUE,
                                         sst_plot = "anomaly",
                                         count_breaks = NULL,
                                         return_results = FALSE){
                                         # number_shades = 16){ see TODO below

  # Validate sst_plot parameter
  sst_plot <- match.arg(sst_plot, c("anomaly", "mean", "count"))

  # Check months ar okay
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

    anomaly_plot_or_list <- plot_pacea_buoy_anomaly_single(
      pacea_buoy_anomaly_list = pacea_buoy_anomaly_list,
      stn_id_to_plot = stn_id_to_plot,
      months = months,
      main = main,
      xlab = xlab,
      ylab = "Month",
      use_stn_id_name = use_stn_id_name,
      sst_plot = sst_plot,
      count_breaks = count_breaks,
      return_results = return_results)
    return(anomaly_plot_or_list)
  }

  if(is.null(months)){
    months = 4         # Default plot of April values, but still allow the
    # single stn_id_to_plot function above to have months specified.
  }

  # Set default count_breaks based on number of months
  if(is.null(count_breaks)){
    count_breaks <- c(0, 10, 15, 20, 31) * ifelse(!is.null(months),
                                                  length(months),
                                                  1)
  }

  if(is.null(stn_id_to_plot)){                 # Plot all of them available
    stn_id_to_plot = unique(pacea_buoy_anomaly_list$anomaly$stn_id)
  }

  if(which.max(months) == length(months)){
    # months are increasing and so are in the same year

    if(is.null(main)){
      main_suffix <- switch(sst_plot,
                           "anomaly" = "anomalies",
                           "mean" = "mean values",
                           "count" = "count of observations")
      if(sst_plot == "anomaly"){
        main =
          paste0("Annual sea-surface temperature ", main_suffix, " for ",
                 summarise_months(months),
                 " from buoys using climatology from ",
                 min(pacea_buoy_anomaly_list$climatology_years),
                 " to ",
                 max(pacea_buoy_anomaly_list$climatology_years),
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
                          "anomaly" = "sst_anomaly",
                          "mean" = "sst_mean",
                          "count" = "sst_n")

    plot_data <- pacea_buoy_anomaly_list$anomaly %>%
      dplyr::filter(stn_id %in% stn_id_to_plot,
                    month %in% months,
                    !is.na(.data[[na_check_col]]),
                    if(sst_plot == "count") sst_n > 0 else TRUE) %>%
      dplyr::group_by(stn_id,
                      year) %>%
      # plot_value becomes the average over the specified months, no need to keep month column
      dplyr::summarise(sst_plot_value = if(sst_plot == "count"){
                                          sum(sst_n)
                                        } else {
                                          mean(if(sst_plot == "anomaly"){
                                                sst_anomaly
                                              } else {
                                                sst_mean
                                              })
                                        }) %>%
      dplyr::ungroup()
  } else {
    # Months are not increasing, for which it is implied a winter average is
    # being calculated that includes Dec and Jan. TODO think about missing
    # months, need a condition for having enough (as elsewhere)

    if(is.null(main)){
      main_suffix <- switch(sst_plot,
                           "anomaly" = "anomalies",
                           "mean" = "mean values",
                           "count" = "count of observations")
      if(sst_plot == "anomaly"){
        main =
          paste0("Annual sea-surface temperature ", main_suffix, " for winter months (",
                 summarise_months(months),
                 ") from buoys using climatology from ",
                 min(pacea_buoy_anomaly_list$climatology_years),
                 " to ",
                 max(pacea_buoy_anomaly_list$climatology_years),
                 " when available; year is the year of the Jan")
      } else {
        main =
          paste0("Annual sea-surface temperature ", main_suffix, " for winter months (",
                 summarise_months(months),
                 ") from buoys; year is the year of the Jan")
      }
    }

    # Determine which column to filter NA values from
    na_check_col <- switch(sst_plot,
                          "anomaly" = "sst_anomaly",
                          "mean" = "sst_mean",
                          "count" = "sst_n")

    plot_data <- pacea_buoy_anomaly_list$anomaly %>%
      dplyr::filter(stn_id %in% stn_id_to_plot,
                    month %in% months,
                    !is.na(.data[[na_check_col]]),
                    if(sst_plot == "count") sst_n > 0 else TRUE) %>%
      dplyr::mutate(year_of_january = (year + 1) * (month >= months[1]) +
                      year * (month < months[1])) %>%    # the year of the january for the winter
      dplyr::group_by(stn_id,
                      year_of_january) %>%
      # plot_value becomes the average over the specified months, no need to keep month column
      dplyr::summarise(sst_plot_value = if(sst_plot == "count"){
                                          sum(sst_n)
                                        } else {
                                          mean(if(sst_plot == "anomaly"){
                                                sst_anomaly
                                              } else {
                                                sst_mean
                                              })
                                        }) %>%
      dplyr::ungroup() %>%
      dplyr::rename(year = year_of_january)
  }

  # Create legend label based on sst_plot
  legend_label <- switch(sst_plot,
                        "anomaly" = bquote("SST anomaly ("*degree*C*")"),
                        "mean" = bquote("Mean SST ("*degree*C*")"),
                        "count" = "Count of daily SST values")

  # For non-anomaly plots, use different color scale (not symmetric)
  if(sst_plot == "anomaly"){
    max_abs <- max(abs(plot_data$sst_plot_value),
                   na.rm = TRUE)
    scale_limits <- c(-max_abs,
                      max_abs)
  } else {
    max_abs <- max(plot_data$sst_plot_value,
                   na.rm = TRUE)
    scale_limits <- c(0,
                      max_abs)
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

  anomaly_plot <-
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
    print(anomaly_plot)
    return(list(plot = anomaly_plot,
                results = plot_data))
  } else {
    return(anomaly_plot)
  }
}
