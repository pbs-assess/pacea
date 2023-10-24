##' Plot the Pacific Harbour Seals abundance estimates
##'
##' Default is to plot all seven regions plus the coastwide estimate, to mostly replicate
##' Figure 3 of DFO (2022; see `?harbour_seals`) in a `ggplot`
##' style. If a single region is
##' indicated then a time series plot is shown, in the same style as
##' plots of object class `pacea_biomass` (such as `hake_biomass`).
##' Note that the values are means and confidence intervals (not medians and
##' confidence intervals like other objects); see `?harbour_seals`.
##'
##' @param obj `pacea_harbour_seals`, as this is of class `pacea_harbour_seals`
##'   and the plotting is tailored for this data set.
##' @param region which region to plot for a single plot; must be one of SOG,
##'   WCVI, QCS, DP, CMC, NMC, HG, or coastwide. If `NULL` (the default) then do a panel
##'   plot showing all seven regions.
##' @param include_coastwide logical, whether to include the coastwide population in the
##'   eight panel plot (keep `region = NULL`).
##' @param value the column to plot as a solid line, in this case always the
##'   default `mean`
##' @param y_max maximum y value for certain types of plot (use this if you get
##'   an error when specifying `ylim`)
##' @param uncertainty_shade_col colour of shading for uncertainty
##' @param uncertainty_line_col colour of line for uncertainty
##' @param uncertainty_line_lty lty of line for uncertainty
##' @param median_type `type` of plot ("o", "p", etc.) for means (note: we have
##'   used median elsewhere so kept with that name, but the seal estimates are
##'   means not medians)
##' @param median_pch pch for median
##' @param median_line_col col for median
##' @param median_line_lty lty for median
##' @param median_line_lwd lwd for median
##' @inherit plot.pacea_index
##' @return plot of the time series as median with bars showing uncertainty (if
##'   `low` and `high` are columns of `obj) to the current device; returns nothing.
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' plot(harbour_seals)
##' plot(harbour_seals, region = "SOG")
##' plot(harbour_seals, include_coastwide = FALSE)
##' }
plot.pacea_harbour_seals <- function(obj,
                                     region = NULL,
                                     value = "mean",
                                     include_coastwide = TRUE,
                                     xlab = "Year",
                                     ylab = attr(obj, "axis_name"),
                                     y_tick_by = 1000,
                                     x_tick_extra_years = 20,
                                     start_decade_ticks = lubridate::ymd("1800-01-01",
                                                                         truncated = 2),
                                     y_max = NULL,
                                     uncertainty_shade_col = rgb(0, 0, 1, .1),
                                     uncertainty_line_col = "blue",
                                     uncertainty_line_lty = 3,
                                     median_type = "l",
                                     median_pch = 16,
                                     median_line_col = "black",
                                     median_line_lty = 1,
                                     median_line_lwd = 2){

  stopifnot("value must be a column of the object in the first argument" =
            value %in% names(obj))

  stopifnot("region must be one of SOG, WCVI, QCS, DP, CMC, NMC, HG, Coastwide" =
            region %in% c("SOG", "WCVI", "QCS", "DP", "CMC", "NMC", "HG", "Coastwide"))

  if(!is.null(region)){
    region_choice <- region     # Else region == region in next line does not work
    obj_region <- dplyr::filter(obj,
                                region == region_choice)

    plot_with_uncertainty_continuous(obj_region,
                                     value = value,
                                     xlab = xlab,
                                     ylab = ylab,
                                     uncertainty_shade_col = rgb(0, 0, 1, .1),
                                     uncertainty_line_col = "blue",
                                     uncertainty_line_lty = 3,
                                     y_max = NULL,
                                     median_type = "l",
                                     median_pch = 16,
                                     median_line_col = "black",
                                     median_line_lty = 1,
                                     median_line_lwd = 2,
                                     main = region_choice)

    add_tickmarks(obj_region,
                  y_tick_by = y_tick_by,
                  y_tick_start = 0, #floor(par("usr")[3]),
                  y_tick_end = ceiling(par("usr")[4]),
                  x_tick_extra_years = x_tick_extra_years,
                  start_decade_ticks = start_decade_ticks)
  } else {
    if(!include_coastwide){
      obj <- dplyr::filter(obj, region != "Coastwide")
    }

    ggplot2::ggplot(data = obj,
           aes(x = date,
               y = mean)) +
      ggplot2::geom_ribbon(aes(ymin = low,
                      ymax = high),
                  fill = uncertainty_shade_col) +
      ggplot2::geom_line(col = uncertainty_line_col) +
      # Add back in if data get included
      # geom_point(data = harbour_seals_data,
      #        aes(x = make_date(year),
      #           y = mean)) +
      ggplot2::facet_wrap(~region,
                 scales = "free_y",
                 ncol = 2)
  }
}
