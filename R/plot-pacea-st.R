#' Plot a pacea spatiotemporal data layer
#'
#' Plot for `pacea_st` classes of objects using `ggplot()`. Objects can be BCCM
#'  outputs, OISST values (TODO no, but should work for the grid26 mapped values), or HOTSSEA outputs.
#' Gives a quick visualization of data, specifying month(s) and year(s). For
#'  more options and configurable plots see vignette.
#'
#' @param x a `pacea_st` object, which is an `sf` object
#' @param months.plot character or numeric vector to indicate which months to include (e.g. `c(1, 2)`, `c("April", "may")`, `c(1, "April")`)
#' @param years.plot vector of years to include, from 1993 to 2019
#' @param bc logical. Should BC coastline layer be plotted?
#' @param eez logical. Should BC EEZ layer be plotted?
#' @param single_current_vector logical. For plotting ocean currents as a total vector with arrows (FALSE), or individual vectors(TRUE)
#' @param ... other arguments to be passed on, but not currently used (`?ggplot`
#'   says the same thing); this should remove a R-CMD-check warning.
#'
#' @return plot of the spatial data to the current device (returns nothing)
#'
#' @importFrom sf st_drop_geometry st_transform
#' @importFrom dplyr left_join select mutate arrange
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes theme_bw theme geom_sf scale_fill_gradientn guides guide_colorbar guide_legend labs facet_grid facet_wrap xlab ylab
#' @importFrom pals jet cividis ocean.oxy plasma ocean.algae ocean.tempo
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pdata <- bccm_surface_temperature()
#' plot(pdata)
#' }
plot.pacea_st <- function(x,
                          months.plot = c("April"),
                          years.plot = c(2018),
                          bc = TRUE,
                          eez = TRUE,
                          single_current_vector = FALSE,
                          ...) {

  # month reference table
  month_table <- data.frame(month.name = month.name,
                            month.abb = month.abb,
                            month.num = 1:12)

  # stop errors
  stopifnot("'x' must be of class `sf`" =
              "sf" %in% class(x))
  stopifnot("Must enter valid numerals for 'years'" = !any(is.na(suppressWarnings(as.numeric(years.plot)))))

  # check if years are available in data
  obj_names <- x %>% st_drop_geometry() %>%
    colnames() %>% strsplit(split = "_") %>%
    unlist() %>% as.numeric() %>%
    matrix(ncol = 2, byrow = TRUE) %>% as.data.frame()
  stopifnot("Invalid 'years' specified" = suppressWarnings(as.numeric(years.plot)) %in% unique(obj_names$V1))
  rm(obj_names)

  # object units attribute
  obj_unit <- attributes(x)$units
  
  # object depth attribute
  obj_depth <- attributes(x)$depth

  # subset year_month columns
  tobj <- subset_pacea_ym(data = x, months = months.plot, years = years.plot)  ####MOVE THIS DOWN

  # Restrict axes to just data or not (BCCM and OISST are for full coast,
  #  HOTSSEA is just Salish Sea)
  if(is.null(attr(x, "restrict_plotting_range"))){
    restrict_plot <- FALSE
  } else {
    restrict_plot <- attr(x, "restrict_plotting_range")
  }
  stopifnot("restrict_plotting_ranage attribute needs to be NULL, TRUE, or FALSE" = is.logical(restrict_plot))

  # Salinity units for salinity plots (BCCM is ppt, hotssea has attribute
  #  indicating psu)
  if(is.null(attr(x, "salinity_unit"))){
    salinity_unit_for_label <- "Salinity\n(ppt)"
  } else {
    salinity_unit_for_label <- paste0("Salinity\n(",
                                      attr(x, "salinity_unit"),
                                      ")")
  }

  ##### OPTION 1
  # ggplotting

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

  # color pallete index table
  vars_units <- c("Temperature\n(\u00B0C)",
                  salinity_unit_for_label,
                  "Dissolved oxygen content\n(mmol-oxygen m^-3)",
                  "pH",
                  "Phytoplankton\n(mmol-nitrogen m^-2)",
                  "Total primary production\n(gC m^-2 d^-1)",
                  "Eastward Current velocity (m/s)",
                  "Northward Current velocity (m/s)",
                  "Current velocity (m/s)")
  colpal <- c(list(pals::jet(50)),
              list(pals::cividis(50)),
              list(pals::ocean.oxy(50)),
              list(pals::plasma(50)),
              list(rev(pals::ocean.algae(50))),
              list(pals::ocean.tempo(50)),
              list(pals::ocean.delta(50)),
              list(pals::ocean.delta(50)),
              list(pals::ocean.speed(50)))
  limit_funs <- c(list(c(floor(min(st_drop_geometry(x))), ceiling(max(st_drop_geometry(x))))),
                  list(c(floor(min(tobj2$value)), ceiling(max(tobj2$value)))),
                  list(c(floor(min(tobj2$value)), ceiling(max(tobj2$value)))),
                  list(c(floor(min(tobj2$value)*10)/10, ceiling(max(tobj2$value)*10)/10)),
                  list(c(floor(min(tobj2$value)), ceiling(max(tobj2$value)))),
                  list(c(floor(min(tobj2$value)), ceiling(max(tobj2$value)))),
                  list(c(-max(ceiling(abs(tobj2$value)), na.rm=TRUE), max(ceiling(abs(tobj2$value)), na.rm=TRUE))),
                  list(c(-max(ceiling(abs(tobj2$value)), na.rm=TRUE), max(ceiling(abs(tobj2$value)), na.rm=TRUE))),
                  list(c(0, ceiling(max(tobj2$value)))))

  # parameters for plotting, first line picks the right index.
  pind <- grep(strsplit(obj_unit, " ")[[1]][1], vars_units)
  pfill <- vars_units[pind]
  pcol <- colpal[pind] %>% unlist()
  plimits <- limit_funs[pind] %>% unlist()
  
  ############## FIXING - put in function
  arrow_data <- FALSE
  
  # read in complementary current data set - either angle or speed dataset
  if(pfill %in% c("Eastward Current velocity (m/s)", 
                  "Northward Current velocity (m/s)")){
    
    if(single_current_vector == FALSE){
      
      # adjust as needed
      arrow_res <- 20000
      arrow_length <- 15000
      
      
      # plotting total current vector
      
      message(paste("Plotting current velocity as total combined velocity (u and v vectors combined).",
                    "Use argument 'single_current_vector = TRUE' if you would like just the u or v current vector plotted."),
              sep = "\n")
      
      # if using u_current data
      if(pfill == "Eastward Current velocity (m/s)"){
        if(obj_depth == "surface"){
          tcur <- bccm_surface_v_currentvelocity_full()
        }
        if(obj_depth == "bottom"){
          tcur <- bccm_bottom_v_currentvelocity_full()
        }
        
        tcur <- subset_pacea_ym(data = tcur, months = months.plot, years = years.plot)
        
        # convert to long format
        tcur2 <- tcur %>%
          tidyr::pivot_longer(cols = !last_col(), cols_vary = "slowest", names_to = "date", values_to = "value") %>%
          mutate(year = as.numeric(substr(date, 1, 4)),
                 month.num = as.numeric(substr(date, 6,7))) %>%
          left_join(month_table, by = join_by(month.num == month.num)) %>%
          mutate(plot.date = paste(year, month.name, sep = " ")) %>%
          arrange(year, month.num)
        
        # create factor for correct order of plotting
        tcur2$month.f <- factor(tcur2$month.name, levels = c(unique(tcur2$month.name)))
        tcur2$plot.date.f <- factor(tcur2$plot.date, levels = c(unique(tcur2$plot.date)))
        
        # combine u and v vectors into one
        tcom2 <- tobj2 %>% rename(u = value) %>% mutate(v = tcur2$value)
      }
      
      # if using v_current data
      if(pfill == "Northward Current velocity (m/s)"){
        if(obj_depth == "surface"){
          tcur <- bccm_surface_u_currentvelocity_full()
        }
        if(obj_depth == "bottom"){
          tcur <- bccm_bottom_u_currentvelocity_full()
        }
        
        tcur <- subset_pacea_ym(data = tcur, months = months.plot, years = years.plot)
        
        # convert to long format
        tcur2 <- tcur %>%
          tidyr::pivot_longer(cols = !last_col(), cols_vary = "slowest", names_to = "date", values_to = "value") %>%
          mutate(year = as.numeric(substr(date, 1, 4)),
                 month.num = as.numeric(substr(date, 6,7))) %>%
          left_join(month_table, by = join_by(month.num == month.num)) %>%
          mutate(plot.date = paste(year, month.name, sep = " ")) %>%
          arrange(year, month.num)
        
        # create factor for correct order of plotting
        tcur2$month.f <- factor(tcur2$month.name, levels = c(unique(tcur2$month.name)))
        tcur2$plot.date.f <- factor(tcur2$plot.date, levels = c(unique(tcur2$plot.date)))
        
        # combine u and v vectors into one
        tcom2 <- tobj2 %>% rename(v = value) %>% mutate(u = tcur2$value)
      }
      
      # create overall speed value
      tcom2 <- tcom2 %>%
        mutate(value = sqrt(u^2 + v^2))
      
      # reduce grid size by arrow_res (arrow resolution)
      arrow_grid <- st_make_grid(tcom2, cellsize = arrow_res) |> 
        st_as_sf() |>
        mutate(grid_id = row_number())
      
      # arrow data
      arrow_data <- suppressWarnings(tcom2 %>%
        st_centroid() %>%
        st_join(arrow_grid) %>%
        filter(!is.na(grid_id)) %>%
        group_by(grid_id, year, month.num, month.name, month.abb, plot.date, month.f, plot.date.f) %>%
        summarise(
          u = mean(u, na.rm = TRUE),
          v = mean(v, na.rm = TRUE),
          speed_arrow = sqrt(u^2 + v^2),
          geometry = st_centroid(st_union(geometry)),
          .groups = "drop"
        ))
      
      # arrow length all equal
      arrow_data <- arrow_data %>%
        mutate(
          speed_arrow = sqrt(u^2 + v^2),
          u_unit = u / speed_arrow,
          v_unit = v / speed_arrow,
          x = st_coordinates(.)[,1],
          y = st_coordinates(.)[,2],
          xend = x + u_unit * arrow_length,
          yend = y + v_unit * arrow_length
        )
      
      # reduce number of columns and rename object to match plotting below
      tobj2 <- tcom2 %>% select(-u, -v)
      
      # reset plot settings
      pfill <- vars_units[9]
      pcol <- colpal[9] %>% unlist()
      plimits <- c(0, ceiling(max(tobj2$value)))
      
    } else {
      # plotting single current vector
    }
  }
  
  ############## END FIXING
  
  # main plot
  tplot <- tobj2 %>%
    ggplot() + theme_bw() +
    theme(strip.background = element_blank()) +
    geom_sf(aes(fill = value), col = NA) +
    scale_fill_gradientn(colours = pcol, limits = plimits) +
    guides(fill = guide_colorbar(barheight = 12,
                                 ticks.colour = "grey30", ticks.linewidth = 0.5,
                                 frame.colour = "black", frame.linewidth = 0.5,
                                 order = 1),
           colour = guide_legend(override.aes = list(linetype = NA), order = 2)) +
    labs(fill = pfill) + xlab(NULL) + ylab(NULL)
  
  # add arrows for current velocity
  if(is.data.frame(arrow_data)){
    tplot <- tplot + 
      geom_segment(
        data = st_drop_geometry(arrow_data),
        aes(x = x, y = y, xend = xend, yend = yend),
        arrow = arrow(length = unit(0.08, "cm")),
        linewidth = 0.25
      ) 
  }

  # facet based on year * month combination
  if(all(length(months.plot) > 1, length(years.plot) > 1)){
    tplot <- tplot +
      facet_grid(year ~ month.f)
  } else {
    tplot <- tplot +
    facet_wrap(.~plot.date.f)
  }

  # Sometimes (e.g. hotssea output) want to restrict the plotting range to the
  # area of interest, but ggplot expands it to the full BC coast when we add in
  # the coast. So here get the current default axes ranges and then reapply them
  # at the end.
  if(restrict_plot){
    x_lim <- ggplot2::ggplot_build(tplot)$layout$panel_scales_x[[1]]$range$range
    # TODO make shortcut function as useful
    y_lim <- ggplot2::ggplot_build(tplot)$layout$panel_scales_y[[1]]$range$range
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

  if(restrict_plot){
    tplot <- tplot +
      ggplot2::coord_sf(xlim = x_lim,
                        ylim = y_lim)
  }

  tplot
}


#' function to index months and years from geospatial ROMS data
#' @noRd
subset_pacea_ym <- function(data, years = years, months = months) {

  # indexing time for selection of plots to show
  dat_names <- as.data.frame(matrix(as.numeric(unlist(strsplit(names(st_drop_geometry(data)), split = "_"))), ncol = 2, byrow = TRUE))


  month_table <- data.frame(month.name = month.name,
                            month.abb = month.abb,
                            month.num = 1:12)

  m_ind <- month_match(months)

  # indexing month and year
  tind <- dat_names[dat_names[,1] %in% as.numeric(years) &
                      dat_names[,2] %in% m_ind, , drop = FALSE]
  tind <- merge(tind, month_table, by.x = "V2", by.y = "month.num", sort = FALSE)[,c("V1", "V2")] %>%
    arrange(V1, V2)

  tind1 <- do.call(paste, c(tind[c("V1", "V2")], sep = "_"))

  tdat <- data %>% dplyr::select(all_of(tind1))

  names(tdat) <- c(tind1, "geometry")
  class(tdat) <- c("pacea_st", "sf", "data.frame")
  attr(tdat, "units") <- attributes(data)$units

  return(tdat)
}

#' function to get months from a vector
#' @noRd
month_match <- function(mths) {
  month_table <- data.frame(month.name = month.name,
                            month.abb = month.abb,
                            month.num = 1:12)

  index_out <- vector()

  for(imonth in mths) {
    if(is.na(suppressWarnings(as.numeric(imonth)))){
      tind <- as.vector(unlist(apply(month_table, 2, function(x) {
        grep(pattern = imonth, x = x, ignore.case = TRUE)
      })))

      if(length(unique(tind)) == 0) stop("month names are invalid - must be full names, abbreviations, or numeric")
      if(length(unique(tind)) > 1) stop(paste0("'", imonth, "'", " month name incorrect or abbreviation too short - more than one name matched"))

      index_out <- c(index_out, unique(tind))
    } else {
      tind <- which(month_table$month.num == as.numeric(imonth))

      if(length(unique(tind)) == 0) stop(paste0("'", imonth, "'", " is not a valid month."))

      index_out <- c(index_out, unique(tind))
    }
  }

  return(index_out)
}
