#' Plot a pacea spatiotemporal data layer for hotssea output
#'
#' TODO using plot.pacea_st() as template, then will see if can just adapt that
#' function to cover this. Probably can by adding an attribute to the hotssea results.
#'
#' Plot for BCCM ROMS sf objects using `ggplot()`. A quick visualization of data, specifying month(s) and year(s). For more options and configurable plots see vignette.
#'
#' @param x a BCCM ROMS `pacea_st` object, which is an `sf` object
#' @param months.plot character or numeric vector to indicate which months to include (e.g. `c(1, 2)`, `c("April", "may")`, `c(1, "April")`)
#' @param years.plot vector of years to include, from 1993 to 2019
#' @param bc logical. Should BC coastline layer be plotted?
#' @param eez logical. Should BC EEZ layer be plotted?
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
plot.pacea_st_hotssea <- function(x,
                                  months.plot = c("April"),
                                  years.plot = c(2018),
                                  bc = TRUE,
                                  eez = TRUE,
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

  # subset year_month columns
  tobj <- subset_pacea_ym(data = x, months = months.plot, years = years.plot)  ####MOVE THIS DOWN

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

  # color pallete index table # adapting for hotssea
  vars_units <- c("Temperature\n(\u00B0C)",
                  "Salinity\n(ppt)",
                  "Dissolved oxygen content\n(mmol-oxygen m^-3)",
                  "pH",
                  "Phytoplankton\n(mmol-nitrogen m^-2)",
                  "Total primary production\n(gC m^-2 d^-1)")
  colpal <- c(list(pals::jet(50)),
              list(pals::cividis(50)),
              list(pals::ocean.oxy(50)),
              list(pals::plasma(50)),
              list(rev(pals::ocean.algae(50))),
              list(pals::ocean.tempo(50)))
  limit_funs <- c(list(c(floor(min(st_drop_geometry(x))), ceiling(max(st_drop_geometry(x))))),
                  list(c(floor(min(tobj2$value)), ceiling(max(tobj2$value)))),
                  list(c(floor(min(tobj2$value)), ceiling(max(tobj2$value)))),
                  list(c(floor(min(tobj2$value)*10)/10, ceiling(max(tobj2$value)*10)/10)),
                  list(c(floor(min(tobj2$value)), ceiling(max(tobj2$value)))),
                  list(c(floor(min(tobj2$value)), ceiling(max(tobj2$value)))))

  # parameters for plotting
  pind <- grep(strsplit(obj_unit, " ")[[1]][1], vars_units)
  pfill <- vars_units[pind]
  pcol <- colpal[pind] %>% unlist()
  plimits <- limit_funs[pind] %>% unlist()
browser()
  # main plot
  tplot <- tobj2 %>%
    ggplot() + theme_bw() +
    theme(strip.background = element_blank()) +
    geom_sf(aes(fill = value)) + # , col = NA) # +     # ERROR IS HERE
    scale_fill_gradientn(colours = pcol, limits = plimits) +
    guides(fill = guide_colorbar(barheight = 12,
                                 ticks.colour = "grey30", ticks.linewidth = 0.5,
                                 frame.colour = "black", frame.linewidth = 0.5,
                                 order = 1),
           colour = guide_legend(override.aes = list(linetype = NA), order = 2)) +
    labs(fill = pfill) + xlab(NULL) + ylab(NULL)

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
