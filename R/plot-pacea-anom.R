
#' Plot anomaly of BCCM spatiotemporal data layer
#' 
#' Plot for BCCM ROMS anomaly objects using `ggplot()`. A quick visualization of anomaly data (relative to climatology), specifying month(s) and year(s). For more options and configurable plots see vignette. 
#'
#' @param x a BCCM ROMS `pacea_stanom` object; output from using `calc_anom()`
#' @param months.plot months to plot. Defaults to current month (if available)
#' @param years.plot years to plot.
#' @param clim.dat climatology data, obtained from using `calc_clim()`. If used, contours of deviations from climatology will be plotted
#' @param eez logical. Should BC EEZ layer be plotted? Can only be plotted with one plot layer.
#' @param bc logical. Should BC coastline layer be plotted? Can only be plotted with one plot layer.
#'
#' @return plot of the spatial data to the current device (returns nothing)
#' 
#' @importFrom tidyr pivot_longer last_col
#' @importFrom lubridate year month week
#' @importFrom dplyr left_join join_by mutate arrange filter bind_cols
#' @importFrom sf st_centroid st_drop_geometry
#' @importFrom pals brewer.prgn ocean.curl brewer.rdgy brewer.brbg brewer.piyg
#' @importFrom ggplot2 ggplot theme_bw theme element_blank geom_sf aes scale_fill_gradientn guides guide_colorbar labs xlab ylab geom_contour scale_colour_manual facet_grid facet_wrap
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' pdata <- bccm_surface_temperature()
#' anom_data <- calc_anom(pdata)
#' plot(anom_data)
#' }
plot.pacea_stanom <- function(x,
                              months.plot,
                              years.plot,
                              clim.dat,
                              bc = TRUE, 
                              eez = TRUE) {
  
  requireNamespace("terra", quietly = TRUE)
  
  # create new names for plot
  month_table <- data.frame(month.name = month.name,
                            month.abb = month.abb,
                            month.num = 1:12)
  
  # stop errors
  stopifnot("'x' must be of class `sf`" =
              "sf" %in% class(x))
  
  # check if years are available in data
  obj_names <- x %>% st_drop_geometry() %>%
    colnames() %>% strsplit(split = "_") %>%
    unlist() %>% as.numeric() %>%
    matrix(ncol = 2, byrow = TRUE) %>% as.data.frame()

  # set month and year to plot
  if(missing(years.plot)) years.plot <- max(obj_names[,1])
  if(missing(months.plot)) {
    months.plot <- lubridate::month(Sys.Date())
    subobj_names <- obj_names %>% 
      filter(V1 %in% years.plot)
    if(!(months.plot %in% unique(subobj_names$V2))){
      months.plot <- max(subobj_names$V2)
    }
  }
  
  if(ncol(x) == 2){
    years.plot <- as.numeric(substr(colnames(x)[1], 1, 4))
    months.plot <- as.numeric(substr(colnames(x)[1], 6, 7))
  }
  
  m_ind <- month_match(months.plot)
  
  # stop errors
  stopifnot("Invalid 'months.plot' specified" = suppressWarnings(as.numeric(m_ind)) %in% unique(obj_names$V2))
  stopifnot("Must enter valid numerals for 'years'" = !any(is.na(suppressWarnings(as.numeric(years.plot)))))
  stopifnot("Invalid 'years.plot' specified" = suppressWarnings(as.numeric(years.plot)) %in% unique(obj_names$V1))
  
  # object units attribute
  obj_unit <- attributes(x)$units
  
  # subset year_month columns
  tobj <- subset_pacea_ym(data = x, months = months.plot, years = years.plot)
  
  # convert to long format
  tobj2 <- tobj %>%
    tidyr::pivot_longer(cols = !last_col(), cols_vary = "slowest", names_to = "date", values_to = "value") %>%
    mutate(year = as.numeric(substr(date, 1, 4)),
           month.num = as.numeric(substr(date, 6,7))) %>%
    left_join(month_table, by = join_by(month.num == month.num)) %>%
    mutate(plot.date = paste(year, month.name, sep = " ")) %>%
    arrange(year, month.num)
  
  # create factor for correct order of plotting
  tobj2$month.f <- factor(tobj2$month.name, levels = c(unique(tobj2$month.name)))
  tobj2$plot.date.f <- factor(tobj2$plot.date, levels = c(unique(tobj2$plot.date)))
  
  # merge with clim.dat (if available)
  if(!missing(clim.dat)) {
    
    # months.plot match clim.dat months
    stopifnot("'clim.dat' must be of class 'pacea_stclim'" = 
                "pacea_stclim" %in% class(clim.dat))
    if(!all(m_ind %in% unique(clim.dat$month))) warning("Not all values found for 'months.plot' in clim.dat")
    
    tclim <- clim.dat %>% 
      mutate(lon = st_coordinates(suppressWarnings(st_centroid(clim.dat)))[,1],
             lat = st_coordinates(suppressWarnings(st_centroid(clim.dat)))[,2]) %>%
      filter(month %in% m_ind) %>%
      st_drop_geometry()
    
    tclim.x <- tobj2 %>%
      mutate(lon = st_coordinates(suppressWarnings(st_centroid(tobj2)))[,1],
             lat = st_coordinates(suppressWarnings(st_centroid(tobj2)))[,2]) %>%
      st_drop_geometry() %>%
      left_join(tclim, by = join_by(month.num == month, lon == lon, lat == lat)) %>%
      mutate(sd_1.3_pos = clim_sd * 1.282,
             sd_2.3_pos = clim_sd * 2.326,
             sd_above1.3 = value - sd_1.3_pos,
             sd_above2.3 = value - sd_2.3_pos,
             sd_below1.3 = value + sd_1.3_pos,
             sd_below2.3 = value + sd_2.3_pos) %>%
      as.data.frame()
    
    tgrid <- terra::rast(tobj2, resolution = 6000)
    
    contour.dat <- data.frame()
    for(j in 1:length(unique(tclim.x$plot.date.f))){
      
      jind <- unique(tclim.x$plot.date.f)[j]
      
      tdat <- tclim.x %>%
        filter(plot.date.f == jind)
      
      tmeta <- tdat[!duplicated(tdat[, c("year", "month.f", "plot.date.f")]), c("year", "month.f", "plot.date.f")]
      
      #contour +90%
      tclim.x1 <- tdat[, c("lon", "lat", "sd_above1.3")]
      trast <- terra::rasterize(terra::vect(tclim.x1), tgrid, field = "sd_above1.3", fun = mean, na.rm = TRUE) 
      tcontour1 <- data.frame(terra::crds(trast, na.rm = FALSE),
                              z = as.vector(trast),
                              sd_var = "sd_above1.3") %>%
        bind_cols(tmeta)
      
      # contour +99%
      tclim.x1 <- tdat[, c("lon", "lat", "sd_above2.3")]
      trast <- terra::rasterize(terra::vect(tclim.x1), tgrid, field = "sd_above2.3", fun = mean, na.rm = TRUE) 
      tcontour2 <- data.frame(terra::crds(trast, na.rm = FALSE),
                              z = as.vector(trast),
                              sd_var = "sd_above2.3") %>%
        bind_cols(tmeta)
      
      #contour -90%
      tclim.x1 <- tdat[, c("lon", "lat", "sd_below1.3")]
      trast <- terra::rasterize(terra::vect(tclim.x1), tgrid, field = "sd_below1.3", fun = mean, na.rm = TRUE) 
      tcontour3 <- data.frame(terra::crds(trast, na.rm = FALSE),
                              z = as.vector(trast),
                              sd_var = "sd_below1.3") %>%
        bind_cols(tmeta)
      
      # contour +99%
      tclim.x1 <- tdat[, c("lon", "lat", "sd_below2.3")]
      trast <- terra::rasterize(terra::vect(tclim.x1), tgrid, field = "sd_below2.3", fun = mean, na.rm = TRUE) 
      tcontour4 <- data.frame(terra::crds(trast, na.rm = FALSE),
                              z = as.vector(trast),
                              sd_var = "sd_below2.3") %>%
        bind_cols(tmeta)
      
      contour.dat <- contour.dat %>% bind_rows(tcontour1, tcontour2, tcontour3, tcontour4)
    }
  }
  
  # Plot Aesthetics:
  gmt_jet <- c("#000080", "#0000bf", "#0000FF", "#007fff", "#00FFFF", "#7fffff", 
               "#FFFFFF", 
               "#FFFF7F", "#FFFF00", "#ff7f00", "#FF0000", "#bf0000", "#820000")
  
  # color pallete index table
  vars_units <- c("Temperature\nanomaly (\u00B0C)",
                  "Salinity\nanomaly (ppt)",
                  "Dissolved oxygen content\nanomaly (mmol-oxygen m^-3)",
                  "pH anomaly",
                  "Phytoplankton anomaly\n(mmol-nitrogen m^-2)",
                  "Total primary production\nanomaly (gC m^-2 d^-1)")
  colpal <- c(list(gmt_jet),
              list(pals::brewer.prgn(50)),
              list(rev(pals::ocean.curl(50))),
              list(pals::brewer.rdgy(50)),
              list(rev(pals::brewer.brbg(50))),
              list(pals::brewer.piyg(50)))
  limit_funs <- c(list(c(-3, 3)),
                  list(c(-ceiling(max(abs(tobj2$value))), ceiling(max(abs(tobj2$value))))),
                  list(c(-ceiling(max(abs(tobj2$value))), ceiling(max(abs(tobj2$value))))),
                  list(c(-0.2, 0.2)),
                  list(c(-30,30)),
                  list(c(-1, 1)))
  breaks_plot <- c(1, 1, 5, 0.05, 10, 0.5)
  
  # parameters for plotting
  pind <- grep(strsplit(obj_unit, " ")[[1]][1], vars_units)
  pfill <- vars_units[pind]
  pcol <- colpal[pind] %>% unlist()
  plimits <- limit_funs[pind] %>% unlist()
  pbreaks <- breaks_plot[pind]
  
  # main plot
  tplot <- tobj2 %>% 
    ggplot() + theme_bw() +
    theme(strip.background = element_blank()) +
    geom_sf(aes(fill = value), col = NA) +
    scale_fill_gradientn(colours = pcol, limits = plimits, breaks = seq(plimits[1], plimits[2], pbreaks)) + 
    guides(fill = guide_colorbar(barheight = 12,
                                 ticks.colour = "grey30", ticks.linewidth = 0.5,
                                 frame.colour = "black", frame.linewidth = 0.5,
                                 order = 1)) +
    labs(fill = pfill) + xlab(NULL) + ylab(NULL)

  if(!missing(clim.dat)) {
    
    # if positive or negative SD to draw contours
    if(pind %in% c(1)) {
      tcon1 <- contour.dat %>%
        dplyr::filter(sd_var == "sd_above1.3")
      tcon2 <- contour.dat %>%
        dplyr::filter(sd_var == "sd_above2.3")
      
      tplot <- tplot + 
        geom_contour(data = tcon1, aes(x = x, y = y, z = z, colour = "sd_above1.3"), linewidth = 0.5, breaks = 0) +
        geom_contour(data = tcon2, aes(x = x, y = y, z = z, colour = "sd_above2.3"), linewidth = 0.5, breaks = 0) +
        scale_colour_manual(name = NULL, guide = "legend",
                            values = c("sd_above1.3" = "grey60",
                                       "sd_above2.3" = "black"),
                            labels = c("+90th %-ile", c("+99th %-ile"))) 
    } else {
      
      tcon1 <- contour.dat %>%
        dplyr::filter(sd_var == "sd_below1.3")
      tcon2 <- contour.dat %>%
        dplyr::filter(sd_var == "sd_below2.3")
      
      tplot <- tplot + 
        geom_contour(data = tcon1, aes(x = x, y = y, z = z, colour = "sd_below1.3"), linewidth = 0.5, breaks = 0) +
        geom_contour(data = tcon2, aes(x = x, y = y, z = z, colour = "sd_below2.3"), linewidth = 0.5, breaks = 0) +
        scale_colour_manual(name = NULL, guide = "legend",
                            values = c("sd_below1.3" = "grey60",
                                       "sd_below2.3" = "black"),
                            labels = c("-90th %-ile", c("-99th %-ile"))) 
    }
  }
    
  # facet based on year * month combination
  if(all(length(months.plot) > 1, length(years.plot) > 1)){
    tplot <- tplot +
      facet_grid(year ~ month.f)
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
  
  suppressWarnings(tplot)
}


#' Plot anomaly of OISST spatiotemporal data layer
#' 
#' @param x an OISST `pacea_oianom` object; output from using `calc_anom()` of `oisst_7day` or `oisst_month` data
#' @param weeks.plot weeks to plot. Defaults to current week (if available)
#' @param months.plot months to plot. Defaults to current month (if available)
#' @param years.plot years to plot. Defaults to current year (if available)
#' @param clim.dat climatology data, obtained from using `calc_clim()`. If used, contours of deviations from climatology mean will be plotted
#' @param eez logical. Should BC EEZ layer be plotted? Can only be plotted with one plot layer.
#' @param bc logical. Should BC coastline layer be plotted? Can only be plotted with one plot layer.
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
                              eez = TRUE) {
  
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
  
  suppressWarnings(tplot)
}
