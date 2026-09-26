#' Plot OISST data
#'
#' Plot for NOAA OISST sf objects using `ggplot()`. A quick visualization of data, specifying month(s) and year(s). For more options and configurable plots see vignette.
#'
#' @param x a OISST `pacea_oi` object, which is an `sf` object.
#' @param weeks.plot numeric vector to indicate which weeks to plot. Defaults to current week (or most recent) available.
#' @param months.plot character or numeric vector to indicate which months to plot (e.g. `c(1, 2)`, `c("April", "may")`, `c(1, "April")`). Defaults to current month (or most recent) available.
#' @param years.plot numeric vector to indicate which years to plot. Defaults to current year (or most recent) available.
#' @param bc logical. Should BC coastline layer be plotted?
#' @param eez logical. Should BC EEZ layer be plotted?
#' @param restrict_plot logical. Should the plot be restricted to the spatial 
#'   domain of the data? If TRUE, the plot extent matches the data bounds and 
#'   bc_coast and bc_eez layers will not expand the plot limits. Default is FALSE.
#' @param buffer_proportion numeric. Buffer proportion to add around the data 
#'   bounds when restrict_plot is TRUE. Default is 0.2 (20% in each direction).
#' @param x_axis_labels numeric vector. X-axis breaks to label when restrict_plot 
#'   is TRUE. If NULL, automatically hides overlapping labels.
#' @param y_axis_labels numeric vector. Y-axis breaks to label when restrict_plot 
#'   is TRUE. If NULL, automatically hides overlapping labels.
#' @param ... other arguments to be passed on, but not currently used (`?ggplot`
#'   says the same thing); this should remove a R-CMD-check warning.
#' @return plot of the spatial data to the current device (returns nothing)
#'
#' @importFrom sf st_coordinates
#' @importFrom lubridate week month
#' @importFrom dplyr filter mutate rename left_join bind_cols
#' @importFrom ggplot2 ggplot aes theme_bw theme geom_tile geom_sf scale_fill_gradientn guides guide_colorbar guide_legend labs facet_grid facet_wrap xlab ylab
#' @importFrom pals jet
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pdata <- oisst_7day
#' plot(pdata)
#' }
plot.pacea_oi <- function(x,
                          weeks.plot,
                          months.plot,
                          years.plot,
                          bc = TRUE,
                          eez = TRUE,
                          restrict_plot = FALSE,
                          buffer_proportion = 0.2,
                          x_axis_labels = NULL,
                          y_axis_labels = NULL,
                          ...) {

  stopifnot("'x' must have 'week' or 'month' as column name" = any(c("week", "month") %in% colnames(x)))

  # month reference table
  month_table <- data.frame(month.name = month.name,
                            month.abb = month.abb,
                            month.num = 1:12)

  # object units attribute
  obj_unit <- attributes(x)$units

  # filter years to plot
  if(missing(years.plot))  years.plot <- max(x$year)
  tobj <- x %>% dplyr::filter(year %in% years.plot)

  stopifnot("'years.plot' not found in data, enter valid numeric value" = nrow(tobj)>0)

  # weekly data
  if("week" %in% colnames(x)){

    # set week to current week if missing
    if(missing(weeks.plot)) {
      weeks.plot <- lubridate::week(Sys.Date())
      if(!(weeks.plot %in% tobj$week)) {
        weeks.plot <- max(tobj$week)
      }
    }

    stopifnot("'weeks.plot' week numbers are invalid - must be values specifying weeks '1:53'" = all(as.numeric(weeks.plot) %in% 1:53))

    ind <- as.numeric(weeks.plot)
    ind <- ind[order(ind)]

    # filter data and create factors for plotting
    tobj <- tobj %>%
      rename(tunit = week) %>%
      dplyr::filter(tunit %in% ind) %>%
      mutate(plot.tunit = paste0("Week ", tunit),
             plot.date = paste0(year, " - Week ", tunit))

    # set levels to order
    tobj$plot.tunit <- factor(tobj$plot.tunit, levels = unique(tobj$plot.tunit))
    tobj$plot.date <- factor(tobj$plot.date, levels = unique(tobj$plot.date))

    # set facets
    if(all(length(weeks.plot) > 1, length(years.plot) > 1)){
      tfacet <- facet_grid(as.factor(year) ~ plot.tunit)
    } else {
      tfacet <- facet_wrap(.~plot.date)
    }
  }

  # monthly data
  if("month" %in% colnames(x)){
    if(missing(months.plot)) {
      months.plot <- lubridate::month(Sys.Date())
      if(!(months.plot %in% tobj$month)) {
        months.plot <- max(tobj$month)    # nocov        # hard to test since
                                          # depends on Sys.Date()
      }
    }

    m_ind <- month_match(months.plot)
    ind <- m_ind[order(m_ind)]

    # filter data join month names and create factors for plotting
    tobj <- tobj %>%
      rename(tunit = month) %>%
      dplyr::filter(tunit %in% ind) %>%
      left_join(month_table, by = join_by(tunit == month.num)) %>%
      mutate(plot.tunit = month.name,
             plot.date = paste(year, month.name, sep = " "))

    # set levels to order
    tobj$plot.tunit <- factor(tobj$plot.tunit, levels = unique(tobj$plot.tunit))
    tobj$plot.date <- factor(tobj$plot.date, levels = unique(tobj$plot.date))

    # set facets
    if(all(length(months.plot) > 1, length(years.plot) > 1)){
      tfacet <- facet_grid(year ~ plot.tunit)
    } else {
      tfacet <- facet_wrap(.~plot.date)
    }
  }

  # stop if no data extracted
  stopifnot("Date combinations specified do not exist in current dataset" = nrow(tobj) > 0)

  # warning if year-timeunit combination not available
  yind <- paste0(unique(tobj$year),ind)
  testyind <- tobj %>%
    mutate(yind = paste0(year,tunit))

  if(!all(yind %in% testyind$yind)) {warning("Not all date combinations entered available for the years specified")}

  # parameters for plotting
  pfill <- "Temperature\n(\u00B0C)"
  pcol <- pals::jet(50)
  plimits <- c(floor(min(x$sst)), ceiling(max(x$sst)))

  # Get data bounds for restrict_plot functionality
  if(restrict_plot){
    data_bbox <- sf::st_bbox(tobj)
    xmin <- as.numeric(data_bbox["xmin"])
    xmax <- as.numeric(data_bbox["xmax"])
    ymin <- as.numeric(data_bbox["ymin"])
    ymax <- as.numeric(data_bbox["ymax"])
    
    # Add buffer in each direction
    x_width <- xmax - xmin
    y_height <- ymax - ymin
    data_coords <- c(xmin = xmin - buffer_proportion * x_width,
                     xmax = xmax + buffer_proportion * x_width,
                     ymin = ymin - buffer_proportion * y_height,
                     ymax = ymax + buffer_proportion * y_height)
  }

  tplot <- tobj %>%
    bind_cols(st_coordinates(tobj)) %>%
    ggplot() + theme_bw() +
    theme(strip.background = element_blank()) +
    geom_tile(aes(x = X, y= Y, fill = sst)) +
    scale_fill_gradientn(colours = pcol, limits = plimits) +
    guides(fill = guide_colorbar(barheight = 12,
                                 ticks.colour = "grey30", ticks.linewidth = 0.5,
                                 frame.colour = "black", frame.linewidth = 0.5,
                                 order = 1),
           colour = guide_legend(override.aes = list(linetype = NA), order = 2)) +
    labs(fill = pfill) + xlab(NULL) + ylab(NULL) +
    tfacet

  # eez and bc layers
  if(eez == TRUE){
    tplot <- tplot +
      geom_sf(data = bc_eez, fill = NA, lty = "dotted")
  }
  if(bc == TRUE){
    tplot <- tplot +
      geom_sf(data = bc_coast, fill = "darkgrey")
  }

  # Apply coordinate limits if restrict_plot is TRUE
  if(restrict_plot){
    tplot <- tplot +
      ggplot2::coord_sf(xlim = c(data_coords["xmin"], data_coords["xmax"]),
                        ylim = c(data_coords["ymin"], data_coords["ymax"]),
                        expand = FALSE)
    
    if(is.null(x_axis_labels)){
      tplot <- tplot +
        ggplot2::scale_x_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE))
    } else {
      tplot <- tplot +
        ggplot2::scale_x_continuous(breaks = x_axis_labels, 
                                    labels = paste0(abs(x_axis_labels), "°W"))
    }
    
    if(!is.null(y_axis_labels)){
      tplot <- tplot +
        ggplot2::scale_y_continuous(breaks = y_axis_labels, 
                                    labels = paste0(y_axis_labels, "°N"))
    }
  }

  suppressWarnings(print(tplot))
}
