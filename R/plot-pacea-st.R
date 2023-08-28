
#' Plot a pacea spatiotemporal data layer
#' 
#' Base plot for sf objects using `plot.sf()`. Quick visualization of data, specifying month(s) and year(s). For more options and configurable plots use `ggplot2`. 
#'
#' @param obj a `pacea_st` object, which is an `sf` object
#' @param months character or numeric vector to indicate which months to include (e.g. `c(1, 2)`, `c("April", "may")`, `c(1, "April")`)
#' @param years vector of years to include, from 1993 to 2019
#' @param bc logical. Should BC coastline layer be plotted? Can only be plotted with one plot layer.
#' @param eez logical. Should BC EEZ layer be plotted? Can only be plotted with one plot layer.
#' @param ... optional arguments passed on to `plot.sf()`
#'
#' @return plot of the spatial data to the current device (returns nothing)
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' dat <- roms_surface_temperature()
#' plot(dat)
#' }
plot.pacea_st <- function(obj,
                          months = c("April"),
                          years = c(1993, 1998, 2003, 2008, 2013, 2018),
                          bc = FALSE, 
                          eez = FALSE,
                          ...) {
  
  # create new names for plot
  month_table <- data.frame(month.name = month.name,
                            month.abb = month.abb,
                            month.num = 1:12)
  
  stopifnot("obj must be of class `sf`" =
              "sf" %in% class(obj))
  
  stopifnot("Must enter valid numerals for 'years'" = !any(is.na(suppressWarnings(as.numeric(years)))))
  
  stopifnot("'months' are invalid - must be full names, abbreviations, or numeric" = 
              as.character(months) %in% c(month.name, month.abb, 1:12))
  
  # check if years are available in data
  obj_years <- unique(obj$year)
  stopifnot("Invalid 'years' specified" = suppressWarnings(as.numeric(years)) %in% obj_years)
  rm(obj_years)
  
  # object units attribute
  obj_unit <- attributes(obj)$units
  
  # subset year_month columns
  tobj <- subset_pacea_ym(data = obj, months = months, years = years)  ####MOVE THIS DOWN
  
  # year-month combinations new names
  tobj <- tobj %>%
    merge(month_table, by.x = "month", by.y = "month.num", sort = FALSE) %>%
    mutate(newnames = paste(year, month.abb, sep = "_"))
  
  # wide format for base plot
  tobj <- tobj %>%
    tidyr::pivot_wider(id_cols = "geometry", names_from = "newnames", values_from = "value") %>%
    dplyr::relocate(geometry, .after = last_col()) 
  
  class(tobj) <- c("sf", "pacea_st", "data.frame", "tbl_df", "tbl")
  
  plot(tobj, border = NA, key.pos = 4, reset = FALSE, ...)
  
  if(ncol(tobj) == 2){
    mtext(text = obj_unit, side = 4, line = 0)
    if(eez == TRUE){
      tbc_eez <- st_transform(bc_eez, crs = "EPSG: 3005")
      plot(tbc_eez, border = "black", col = NA, lty = 2, add = TRUE)
    }
    if(bc == TRUE){
      tbc_coast <- st_transform(bc_coast, crs = "EPSG: 3005")
      plot(tbc_coast, border = "grey50", col = "grey80", add = TRUE,)
    }
  } else {
    mtext(text = obj_unit, side = 4, line = -4)
  }
}


#' function to index months and years from geospatial ROMS data
#' @noRd
subset_pacea_ym <- function(data, years = years, months = months) {
  
  month_table <- data.frame(month.name = month.name,
                            month.abb = month.abb,
                            month.num = 1:12)
  
  m_ind <- vector()
  
  for(imonth in months) {
    if(is.na(suppressWarnings(as.numeric(imonth)))){
      tind <- as.vector(unlist(apply(month_table, 2, function(x) {
        grep(pattern = imonth, x = x, ignore.case = TRUE)
      })))
      if(length(unique(tind)) != 1) stop("Month name incorrect")
      
      m_ind <- c(m_ind, unique(tind))
    } else {
      as.numeric(imonth)
      
      tind <- which(month_table$month.num == as.numeric(imonth))
      
      m_ind <- c(m_ind, unique(tind))
    }
  }
  
  # indexing month and year
  tdat <- data[data$year %in% as.numeric(years) & 
                 data$month %in% m_ind, , drop = FALSE]
  
  class(tdat) <- c("pacea_st", "sf", "data.frame", "tbl_df", "tbl")
  attr(tdat, "units") <- attributes(data)$units
  
  return(tdat)
}



