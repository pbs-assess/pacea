#' Plot anomaly of OISST spatiotemporal data layer
#'
#' @param x an OISST `pacea_oianom` object; output from using `calc_anom()` of `oisst_7day` or `oisst_month` data
#' @param weeks.plot weeks to plot. Defaults to current week (if available)
#' @param months.plot months to plot. Defaults to current month (if available)
#' @param years.plot years to plot. Defaults to current year (if available)
#' @param clim.dat climatology data, obtained from using `calc_clim()`. If used, contours of deviations from climatology mean will be plotted
#' @param eez logical. Should BC EEZ layer be plotted? Can only be plotted with one plot layer.
#' @param bc logical. Should BC coastline layer be plotted? Can only be plotted with one plot layer.
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

  suppressWarnings(print(tplot))
}
