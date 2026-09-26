#' Plot anomaly of OISST spatiotemporal data layer
#'
#' Gets called from `pacea_oisst_anomalies_list()`, which uses the newer format
#' of climatology and anomalies TODO. Ensure consistent.
#'
#' @param x an OISST `pacea_oianom` object; output from using `calc_anom()` of `oisst_7day` or `oisst_month` data
#' @param weeks.plot weeks to plot. Defaults to current week (if available)
#' @param months.plot months to plot. Defaults to current month (if available)
#' @param years.plot years to plot. Defaults to current year (if available)
#' @param clim.dat climatology data, obtained from using `calc_clim()`. If used, contours of deviations from climatology mean will be plotted
#' @param eez logical. Should BC EEZ layer be plotted? Can only be plotted with one plot layer.
#' @param bc logical. Should BC coastline layer be plotted? Can only be plotted with one plot layer.
#' @param restrict_plot logical. Should the plot be restricted to the spatial
#'   domain of the data? If TRUE, the plot extent matches the data bounds and
#'   bc_coast and bc_eez layers will not expand the plot limits. Default is FALSE.
#' @param buffer_proportion numeric. Buffer proportion to add around the data
#'   bounds when restrict_plot is TRUE. Default is 0.2 (20% in each
#' direction). Note that locations may be based on the centres of the grid
#' cells, hence you may need to play with `buffer_proportion` depending on your plot.
#' @param x_axis_labels numeric vector. X-axis breaks to label when restrict_plot 
#'   is TRUE. If NULL, automatically hides overlapping labels. Allows fine control 
#'   over which longitude values are displayed.
#' @param y_axis_labels numeric vector. Y-axis breaks to label when restrict_plot 
#'   is TRUE. If NULL, automatically hides overlapping labels. Allows fine control 
#'   over which latitude values are displayed.
#' @param ... other arguments to be passed on, but not currently used (`?ggplot`
#'   says the same thing); this should remove a R-CMD-check warning.
#'
#' @return plot of the spatial data to the current device (returns nothing)
#'
#' @importFrom lubridate year month week
#' @importFrom dplyr select filter rename mutate arrange left_join join_by bind_cols
#' @importFrom sf st_drop_geometry st_coordinates
#' @importFrom ggplot2 ggplot theme_bw theme element_blank aes geom_tile scale_fill_gradientn guides guide_colorbar labs xlab ylab geom_contour scale_colour_manual facet_grid facet_wrap geom_sf
#'
#' @export
#'
#' @examples
#' \dontrun{
#' anom_data <- calc_anom(oisst_7day)
#' plot(anom_data)
#' }
plot.pacea_oianom <- function(x,
                              weeks.plot,
                              months.plot,
                              years.plot,
                              clim.dat,
                              bc = TRUE,
                              eez = TRUE,
                              restrict_plot = FALSE,
                              buffer_proportion = 0.2,
                              x_axis_labels = NULL,
                              y_axis_labels = NULL,
                              ...) {

  # create new names for plot
  month_table <- data.frame(month.name = month.name,
                            month.abb = month.abb,
                            month.num = 1:12)

  # stop errors
  stopifnot("'x' must be of class `sf`" =
              "sf" %in% class(x))

  # set year to plot
  if(missing(years.plot)) {
    years.plot <- lubridate::year(Sys.Date())
    if(!(years.plot %in% unique(x$year))){
      years.plot <- max(x$year)
    }
  }

  # stop errors
  stopifnot("Must enter valid numerals for 'years'" = !any(is.na(suppressWarnings(as.numeric(years.plot)))))
  stopifnot("Invalid 'years.plot' specified" = suppressWarnings(as.numeric(years.plot)) %in% unique(x$year))

  # month/week plot if missing
  if("week" %in% colnames(x)){
    get.tunit <- "week"
    obj_names <- x %>% st_drop_geometry() %>% dplyr::select(year, week)
    obj_names <- obj_names[-which(duplicated(obj_names)),]

    if(missing(weeks.plot)) {
      weeks.plot <- lubridate::week(Sys.Date())
      subobj_names <- obj_names %>%
        filter(year %in% years.plot)
      if(!(weeks.plot %in% unique(subobj_names$week))){
        weeks.plot <- max(subobj_names$week)
      }
    }

    tunits.plot <- weeks.plot

    # stop errors
    stopifnot("Invalid 'weeks.plot' specified" = suppressWarnings(as.numeric(weeks.plot)) %in% unique(obj_names$week))

    # subset data
    tobj <- x %>%
      dplyr::filter(year %in% years.plot,
                    week %in% tunits.plot) %>%
      rename(tunit = week) %>%
      mutate(tunit.name = paste0("Week ", tunit),
             plot.date = paste(year, tunit.name, sep = " ")) %>%
      arrange(year, tunit)
  }

  if("month" %in% colnames(x)){
    get.tunit <- "month"
    obj_names <- x %>% st_drop_geometry() %>% dplyr::select(year, month)
    obj_names <- obj_names[-which(duplicated(obj_names)),]

    # set month to plot if missing
    if(missing(months.plot)) {
      months.plot <- lubridate::month(Sys.Date())
      subobj_names <- obj_names %>%
        filter(year %in% years.plot)
      if(!(months.plot %in% unique(subobj_names$month))){
        months.plot <- max(subobj_names$month)
      }
    }

    m_ind <- month_match(months.plot)
    tunits.plot <- m_ind

    # stop errors
    stopifnot("Invalid 'months.plot' specified" = suppressWarnings(as.numeric(m_ind)) %in% unique(obj_names$month))

    # subset data
    tobj <- x %>%
      dplyr::filter(year %in% years.plot,
                    month %in% tunits.plot) %>%
      left_join(month_table, by = join_by(month == month.num)) %>%
      rename(tunit = month,
             tunit.name = month.name) %>%
      mutate(plot.date = paste(year, tunit.name, sep = " ")) %>%
      arrange(year, tunit)
  }

  # get coordinates
  tobj2 <- tobj %>%
    bind_cols(st_coordinates(tobj))

  # create factor for correct order of plotting
  tobj2$tunit.namef <- factor(tobj2$tunit.name, levels = c(unique(tobj2$tunit.name)))
  tobj2$plot.date.f <- factor(tobj2$plot.date, levels = c(unique(tobj2$plot.date)))

  # object units attribute
  obj_unit <- attributes(x)$units

  # merge with clim.dat (if available)
  if(!missing(clim.dat)) {
    stopifnot("'clim.dat' must be of class 'pacea_oiclim'" =
                "pacea_oiclim" %in% class(clim.dat))
    stopifnot("'clim.dat' variable for time units do not equal that of 'x' object (e.g. both must have 'month' column)" =
                get.tunit %in% colnames(clim.dat))
    colnames(clim.dat)[which(colnames(clim.dat) == get.tunit)] <- "tunit"

    # match clim.dat tunits
    stopifnot("'clim.dat' must be of class 'pacea_oiclim'" =
                "pacea_oiclim" %in% class(clim.dat))
    if(!all(tunits.plot %in% unique(clim.dat$tunit))) warning("Not all values for 'months.plot' or 'weeks.plot' were found in clim.dat")

    tclim <- clim.dat %>% filter(tunit %in% tunits.plot) %>%
      mutate(tgeo = as.character(geometry)) %>% st_drop_geometry()

    tobj2 <- tobj2 %>%
      mutate(tgeo = as.character(geometry)) %>%
      left_join(tclim, by = join_by(tunit == tunit, tgeo == tgeo)) %>%
      mutate(lon = st_coordinates(tobj2)[,1],
             lat = st_coordinates(tobj2)[,2],
             sd_1.3_pos = clim_sd * 1.282,
             sd_2.3_pos = clim_sd * 2.326,
             sd_above1.3 = anom - sd_1.3_pos,
             sd_above2.3 = anom - sd_2.3_pos)
  }

  # Plot Aesthetics:
  gmt_jet <- c("#000080", "#0000bf", "#0000FF", "#007fff", "#00FFFF", "#7fffff",
                        "#FFFFFF",
                        "#FFFF7F", "#FFFF00", "#ff7f00", "#FF0000", "#bf0000", "#820000")

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

  # parameters for plotting
  pfill <- obj_unit
  pcol <- gmt_jet
  plimits <- c(-3, 3)
  pbreaks <- 1

  # main plot
  tplot <- tobj2 %>%
    ggplot() + theme_bw() +
    theme(strip.background = element_blank()) +
    geom_tile(aes(x = X, y = Y, fill = anom)) +
    scale_fill_gradientn(colours = pcol, limits = plimits, breaks = seq(plimits[1], plimits[2], pbreaks)) +
    guides(fill = guide_colorbar(barheight = 12,
                                 ticks.colour = "grey30", ticks.linewidth = 0.5,
                                 frame.colour = "black", frame.linewidth = 0.5,
                                 order = 1)) +
    labs(fill = pfill) + xlab(NULL) + ylab(NULL)

  if(!missing(clim.dat)){
    tplot <- tplot +
      geom_contour(aes(x = X, y = Y, z = sd_above1.3, colour = "sd_above1.3"), linewidth = 0.5, breaks = 0) +
      geom_contour(aes(x = X, y = Y, z = sd_above2.3, colour = "sd_above2.3"), linewidth = 0.5, breaks = 0) +
      scale_colour_manual(name = NULL, guide = "legend",
                          values = c("sd_above1.3" = "grey60",
                                     "sd_above2.3" = "black"),
                          labels = c("+90th %-ile", c("+99th %-ile")))
  }

  # facet based on year * month combination
  if(all(length(tunits.plot) > 1, length(years.plot) > 1)){
    tplot <- tplot +
      facet_grid(year ~ tunit.namef)
  } else {
    tplot <- tplot +
      facet_wrap(.~plot.date.f)
  }

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
