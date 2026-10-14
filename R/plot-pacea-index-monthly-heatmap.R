##' Plot monthly values of a pacea index as a heatmap
##'
##' Creates a heatmap-style plot with years on the x-axis and months on the y-axis,
##' showing monthly values or anomalies of a pacea_index object. Similar in style
##' to `plot_pacea_buoy_anomalies_single()` but for indices with monthly resolution.
##'
##' @param pacea_index_obj object of class `pacea_index` with monthly resolution
##'   (e.g., `oni`, `npi_monthly`). Must have columns: `year`, `month`, and the
##'   column specified in the `value` parameter.
##' @param value which column of `pacea_index_obj` to plot. Common options include
##'   "anomaly" and "value", but any numeric column other than year and month can
##'   be specified. Default is "anomaly".
##' @param months numeric vector of months (1-12) to include in the plot.
##'   Default includes all months (1:12).
##' @param years numeric vector of years to include in the plot. Default is NULL,
##'   which includes all years in the data.
##' @param main plot title. If NULL, an automatic title is generated based on the
##'   index name and value type.
##' @param xlab x-axis label (default: "Year")
##' @param ylab y-axis label (default: "Month")
##' @param return_results logical; if TRUE, returns a list with `plot` and
##'   `results` (the data used for plotting); if FALSE (default), returns just
##'   the ggplot object.
##' @param scale_limits vector of length 2 specifying the colour scale limits
##'   (e.g., c(-2, 2)). If NULL, limits are calculated automatically: for anomaly
##'   values, the scale is symmetric around 0; for other values, the scale ranges
##'   from the minimum to the maximum value.
##' @param neutral_oni_one_colour logical; if TRUE (and `value` is "anomaly" or "anom"),
##'   anomalies between -0.5 and 0.5 are coloured with the colour specified in
##'   `neutral_oni_colour` to indicate neutral conditions. Default is FALSE.
##'   This is for ONI where neutral ENSO conditions are
##'   defined as ±0.5°C anomalies.
##' @param neutral_oni_colour character string specifying the colour to use for
##'   neutral anomalies (between -0.5 and 0.5) when `neutral_oni_one_colour = TRUE`.
##'   Default is "grey60".
##' @param heatmap_colours numeric vector of colours to use for the heatmap.
##'   Default is `pals::ocean.balance(20)[2:19]`, which provides 18 colours.
##'   When `neutral_oni_one_colour = TRUE`, must have an even number of colours.
##'
##' @return a ggplot object (when `return_results = FALSE`) or a list with `plot`
##'   and `results` (when `return_results = TRUE`)
##' @export
##' @author Andrew Edwards and Andrea Hilborn
##' @examples
##' \dontrun{
##' # Plot ONI anomalies as a heatmap
##' plot_pacea_index_monthly_heatmap(oni)
##'
##' # Plot absolute ONI values instead of anomalies
##' plot_pacea_index_monthly_heatmap(oni, value = "value")
##'
##' # Plot NPI monthly values
##' plot_pacea_index_monthly_heatmap(npi_monthly, value = "value")
##'
##' # Plot ONI for specific years
##' plot_pacea_index_monthly_heatmap(oni, years = 2010:2020)
##'
##' # Plot ONI with neutral conditions (±0.5) as grey60 (default)
##' plot_pacea_index_monthly_heatmap(oni, neutral_oni_one_colour = TRUE)
##'
##' # Plot ONI with neutral conditions as white
##' plot_pacea_index_monthly_heatmap(oni, neutral_oni_one_colour = TRUE, neutral_oni_colour = "white")
##'
##' # Get the plot and underlying data
##' result <- plot_pacea_index_monthly_heatmap(oni, return_results = TRUE)
##' }
plot_pacea_index_monthly_heatmap <- function(pacea_index_obj,
                                             value = "anomaly",
                                             months = 1:12,
                                             years = NULL,
                                             main = NULL,
                                             xlab = "Year",
                                             ylab = "Month",
                                             return_results = FALSE,
                                             scale_limits = NULL,
                                             neutral_oni_one_colour = FALSE,
                                             neutral_oni_colour = "grey60",
                                             heatmap_colours = pals::ocean.balance(20)[2:19]){

  # Validate input is a pacea_index object
  stopifnot("Input must be a pacea_index object" =
              inherits(pacea_index_obj,
                       "pacea_index"))

  # Check that the object has year and month columns
  stopifnot("pacea_index_obj must have 'year' column" =
              "year" %in% names(pacea_index_obj))
  stopifnot("pacea_index_obj must have 'month' column" =
              "month" %in% names(pacea_index_obj))

  # Validate value parameter
  stopifnot("value must be a column of the pacea_index object" =
              value %in% names(pacea_index_obj))

  # Determine the index name from the object's axis_name attribute if available
  index_name <- attr(pacea_index_obj, "axis_name")
  if(is.null(index_name)){
    # Use a generic name if no axis_name attribute
    index_name <- "Index"
  }

  # Filter data for specified months
  plot_data <- pacea_index_obj %>%
    dplyr::filter(month %in% months,
                  if(!is.null(years)) year %in% years else TRUE) %>%
    dplyr::mutate(month_as_factor = factor(month,
                                           levels = rev(sort(months)),
                                           labels = rev(month.abb[sort(months)])))

  # Check that we have data after filtering
  stopifnot("No data available for the specified months" =
              nrow(plot_data) > 0)

  # Generate default title if not provided
  if(is.null(main)){
    value_label <- switch(value,
                          "anomaly" = "anomalies",
                          "anom" = "anomalies",
                          "value" = "values",
                          "val" = "values",
                          value)  # Fall back to the column name itself

    main <- paste0("Monthly ",
                   index_name,
                   " ",
                   value_label)
  }

  # Determine colour scale limits
  if(is.null(scale_limits)){
    # Check if the value column name suggests it's an anomaly
    is_anomaly <- value %in% c("anomaly", "anom")

    if(is_anomaly){
      # For anomalies, use symmetric scale around 0
      max_abs <- max(abs(plot_data[[value]]),
                     na.rm = TRUE)
      scale_limits <- c(-max_abs,
                        max_abs)
    } else {
      # For absolute values, scale from min to max
      min_val <- min(plot_data[[value]],
                     na.rm = TRUE)
      max_val <- max(plot_data[[value]],
                     na.rm = TRUE)
      scale_limits <- c(min_val,
                        max_val)
    }
  }

  # Create legend label based on value type and index name
  legend_label <- switch(value,
                        "anomaly" = paste0(index_name, " anomaly"),
                        "anom" = paste0(index_name, " anomaly"),
                        "value" = paste0(index_name, " (absolute)"),
                        "val" = paste0(index_name, " (absolute)"),
                        index_name)

  # Define colour scale
  if(neutral_oni_one_colour && value %in% c("anomaly", "anom")){
    # Validate that heatmap_colours has an even number of colours
    n_colours <- length(heatmap_colours)
    if(n_colours %% 2 != 0){
      stop("When neutral_oni_one_colour = TRUE, heatmap_colours must have an even number of colours. ",
           "The palette you provided has ", n_colours, " colours. ",
           "Please use a palette with an even number of colours (e.g., pals::ocean.balance(20)[2:19] has 18 colours).")
    }

    # Custom scale with specified colour for ±0.5, but same colours as the original palette outside that range
    min_val <- scale_limits[1]
    max_val <- scale_limits[2]

    # Calculate where -0.5 and +0.5 map to in the [0, 1] value space
    position_neg_05 <- scales::rescale(-0.5,
                                  to = c(0, 1),
                                  from = c(min_val, max_val))
    position_pos_05 <- scales::rescale(0.5,
                                  to = c(0, 1),
                                  from = c(min_val, max_val))

    # Split palette in half: blue/cool colours on left, red/warm colours on right
    mid_point <- n_colours / 2
    cool_colours <- heatmap_colours[1:mid_point]
    warm_colours <- heatmap_colours[(mid_point + 1):n_colours]

    # Use the blue and red halves of the palette, with specified colour in the middle
    colour_scale <- ggplot2::scale_fill_gradientn(
      colours = c(cool_colours,
                  neutral_oni_colour,
                  neutral_oni_colour,
                  warm_colours),
      values = c(scales::rescale(seq(min_val,
                                     -0.5,
                                     length.out = mid_point),
                                 to = c(0,
                                        position_neg_05 - 0.000001)),
                 position_neg_05,
                 position_pos_05,
                 scales::rescale(seq(0.5,
                                     max_val,
                                     length.out = mid_point),
                                 to = c(position_pos_05 + 0.000001, 1))),
      limits = scale_limits,
      name = legend_label)
  } else {
    # Standard scale for non-anomalies or when neutral_oni_one_colour is FALSE
    colour_scale <- ggplot2::scale_fill_gradientn(colours = heatmap_colours,
                                                  limits = scale_limits,
                                                  name = legend_label)
  }

  # Determine x-axis breaks
  year_range <- seq(min(plot_data$year,
                        na.rm = TRUE),
                    max(plot_data$year,
                        na.rm = TRUE))

  # Create label column for text display
  plot_data <- plot_data %>%
    dplyr::mutate(label = as.character(round(.data[[value]], 1)))

  # Create the plot
  monthly_heatmap <-
    plot_data %>%
    ggplot2::ggplot(ggplot2::aes(x = year,
                                 y = month_as_factor)) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data[[value]]),
                       colour = "black") +
    colour_scale +
    ggplot2::scale_x_continuous(expand = c(0, 0),
                                name = xlab,
                                breaks = year_range) +
    ggplot2::scale_y_discrete(expand = c(0, 0),
                              name = ylab) +
    ggplot2::theme(legend.position = "bottom",
                   strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(face = "bold",
                                                      size = 12),
                   panel.spacing.y = grid::unit(0.1,
                                                "lines"),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                             colour = NA),
                   panel.grid = ggplot2::element_blank()) +
    ggplot2::geom_text(ggplot2::aes(label = label),
                       size = 3.5) +
    ggplot2::guides(fill = ggplot2::guide_colorbar(barwidth = 15,
                                                   title.position = "top",
                                                   frame.colour = "black",
                                                   ticks.colour = "black")) +
    ggplot2::labs(title = main)

  if(return_results){
    print(monthly_heatmap)
    return(list(plot = monthly_heatmap,
                results = plot_data))
  } else {
    return(monthly_heatmap)
  }
}
