# plot pacea_st function

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
  obj_names <- obj %>% st_drop_geometry() %>%
    colnames() %>% strsplit(split = "_") %>%
    unlist() %>% as.numeric() %>%
    matrix(ncol = 2, byrow = TRUE) %>% as.data.frame()
  stopifnot("Invalid 'years' specified" = suppressWarnings(as.numeric(years)) %in% unique(obj_names$V1))
  rm(obj_names)
  
  # object units attribute
  obj_unit <- attributes(obj)$units
  
  # subset year_month columns
  tobj <- subset_pacea_ym(data = obj, months = months, years = years)  ####MOVE THIS DOWN
  
  # year-month combinations
  tobj_names <- as.data.frame(matrix(as.numeric(unlist(strsplit(names(st_drop_geometry(tobj)), split = "_"))), 
                                     ncol = 2, byrow = TRUE)) 
  tobj_names <- merge(tobj_names, month_table, by.x = "V2", by.y = "month.num", sort = FALSE)[,c("V1", "V2", "month.name", "month.abb")] %>%
    arrange(V1,V2)
  
  
  tnew_names <- do.call(paste, c(tobj_names[c("V1", "month.abb")], sep = "_"))
  
  
  names(tobj) <- c(tnew_names, "geometry")
  class(tobj) <- c("sf", "pacea_st", "data.frame")
  
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
  
  # indexing time for selection of plots to show
  dat_names <- as.data.frame(matrix(as.numeric(unlist(strsplit(names(st_drop_geometry(data)), split = "_"))), ncol = 2, byrow = TRUE))
  
  
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



