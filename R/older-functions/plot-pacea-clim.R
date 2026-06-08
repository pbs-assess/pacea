

#' Plot a pacea_st climatology layer
#'
#' Base plot for BCCM ROMS climatology sf objects using `plot.sf()`. Quick visualization of climatology, specifying month(s). For more options and configurable plots use `ggplot2`. 
#' 
#' @param x pacea data object output from 'calc_clim()' function 
#' @param clim_months climatology months to plot
#' @param bc logical. Should BC coastline layer be plotted? Can only be plotted with one plot layer.
#' @param eez logical. Should BC EEZ layer be plotted? Can only be plotted with one plot layer.
#'
#' @return plot of the spatial data to the current device (returns nothing)
#' @export
#'
#' @examples
#' \dontrun{
#' pdata <- bccm_surface_temperature()
#' clim_data <- calc_clim(pdata)
#' plot(clim_data)
#' }
plot.pacea_stclim <- function(x,
                              clim_months,
                              bc = TRUE, 
                              eez = TRUE) {
  
  # month table
  month_table <- data.frame(month.name = month.name,
                            month.abb = month.abb,
                            month.num = 1:12)
  
  # object units attribute
  obj_unit <- attributes(x)$units
  
  # extract months to plot
  if(missing(clim_months)) {
    clim_months <- lubridate::month(Sys.Date())
  } 
  
  m_ind <- vector()
  for(imonth in clim_months) {
    if(is.na(suppressWarnings(as.numeric(imonth)))){
      tind <- as.vector(unlist(apply(month_table, 2, function(x) {
        grep(pattern = imonth, x = x, ignore.case = TRUE)
      })))
      
      if(length(unique(tind)) == 0) stop("'clim_months' month names are invalid - must be full names, abbreviations, or numeric")
      if(length(unique(tind)) > 1) stop(paste0("'", imonth, "'", " month name incorrect or abbreviation too short - more than one name matched"))
      
      m_ind <- c(m_ind, unique(tind))
    } else {
      tind <- which(month_table$month.num == as.numeric(imonth))
      
      if(length(unique(tind)) == 0) stop(paste0("'", imonth, "'", " is not a valid month."))
      
      m_ind <- c(m_ind, unique(tind))
    }
  }
  
  tobj <- x[, m_ind[order(m_ind)]]
  
  # sf class for base plot
  class(tobj) <- c("sf", "data.frame")

  plot(tobj, border = NA, key.pos = 4, reset = FALSE, ...)

  # add legend units and extra layers
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
    
    if(any(bc,eez)) {
      message("Plotting EEZ and BC layers only available for single month plots.")
    }
  }
}



#' Plot a pacea_oi climatology layer
#'
#' Base plot for NOAA OISST climatology sf objects using `plot.sf()`. Quick visualization of climatology, specifying month(s). For more options and configurable plots use `ggplot2`. 
#'
#' @param obj pacea data object output from 'calc_clim()' function 
#' @param clim_timeunits climatology time units - calendar week(s), calendar month(s) to plot, depending on climatology
#' @param bc logical. Should BC coastline layer be plotted? Can only be plotted with one plot layer.
#' @param eez logical. Should BC EEZ layer be plotted? Can only be plotted with one plot layer.
#' @param ... optional arguments passed on to `plot.sf()`
#' 
#' @return plot of the spatial data to the current device (returns nothing)
#' @export
#'
#' @examples
#' \dontrun{
#' clim_data <- oisst_7day
#' plot(clim_data)
#' }
plot.pacea_oiclim <- function(obj,
                              clim_timeunits,
                              bc = TRUE, 
                              eez = TRUE,
                              ...) {
  
  stopifnot("'obj' must have 'week' or 'month' as column name" = any(c("week", "month") %in% colnames(obj)))
  
  # month table
  month_table <- data.frame(month.name = month.name,
                            month.abb = month.abb,
                            month.num = 1:12)
  
  # object units attribute
  obj_unit <- attributes(obj)$units
  
  if(missing(clim_timeunits)) {
    if("week" %in% colnames(obj)){
      clim_timeunits <- lubridate::week(Sys.Date())
    }
    if("month" %in% colnames(obj)){
      clim_timeunits <- lubridate::month(Sys.Date())
    }
  } 
  
  # extract clim_timeunits
  if("month" %in% colnames(obj)) {
    tobj <- obj %>%
      rename(tunit = month)
    
    m_ind <- vector()
    for(imonth in clim_timeunits) {
      if(is.na(suppressWarnings(as.numeric(imonth)))){
        tind <- as.vector(unlist(apply(month_table, 2, function(x) {
          grep(pattern = imonth, x = x, ignore.case = TRUE)
        })))
        
        if(length(unique(tind)) == 0) stop("'clim_timeunits' month names are invalid - must be full names, abbreviations, or numeric")
        if(length(unique(tind)) > 1) stop(paste0("'", imonth, "'", " month name incorrect or abbreviation too short - more than one name matched"))
        
        m_ind <- c(m_ind, unique(tind))
      } else {
        tind <- which(month_table$month.num == as.numeric(imonth))
        
        if(length(unique(tind)) == 0) stop(paste0("'", imonth, "'", " is not a valid month."))
        
        m_ind <- c(m_ind, unique(tind))
      }
    }
    ind <- m_ind
    
  } 
  if("week" %in% colnames(obj)) {
    tobj <- obj %>%
      rename(tunit = week)
    
    stopifnot("'clim_timeunits' week numbers are invalid - must be numeric from week 1 to 53" = all(as.numeric(clim_timeunits) %in% 1:53))
    
    ind <- as.numeric(clim_timeunits)
  }
  
  ind <- ind[order(ind)]
  
  tobjplot <- tobj %>%
    filter(tunit %in% ind) %>%
    tidyr::pivot_wider(id_cols = "geometry", names_from = "tunit", values_from = "sst") %>% 
    dplyr::relocate(geometry, .after = last_col())
  
  plot(tobjplot, pch = 19, border = NA, key.pos = 4, reset = FALSE, ...)
  
  
  # add legend units and extra layers
  if(ncol(tobjplot) == 2){
    mtext(text = obj_unit, side = 4, line = 0)
    if(eez == TRUE){
      plot(bc_eez, border = "black", col = NA, lty = 2, add = TRUE)
    }
    if(bc == TRUE){
      plot(bc_coast, border = "grey50", col = "grey80", add = TRUE)
    }
  } else {
    mtext(text = obj_unit, side = 4, line = -4)
    
    if(any(bc,eez)) {
      message("Plotting EEZ and BC layers only available for single month plots.")
    }
  }
  
}



plot.pacea_buoyclim <- function(x,
                                stn_id = "C46146",
                                years = NULL,
                                year_highlight = lubridate::year(lubridate::today())){
  # 
  # # object units attribute
  # obj_unit <- attributes(obj)$units
  # 
  # station <- stn_id       # Can't use stn_id == stn_id in upcoming filter
  # if(length(station) == 1){
  #   obj_one_stn <- dplyr::filter(obj,
  #                                stn_id == station)
  #   if(!is.null(years)){
  #     obj_one_stn <- dplyr::filter(obj_one_stn,
  #                                  lubridate::year(date) %in% years)
  #   }
  #   plot_buoy_sst_single(obj_one_stn,
  #                        year_highlight = year_highlight)
  #   
  # } else {
  #   stop("`stn_id` has to be a single station; if doing multiple in a panel plot would be useful then contact us and we can implement it")
  #   #plot_buoy_sst_multiple(obj,
  #   #                       stn_id = station,
  #   #                       years = years,
  #   #                       year_highlight = year_highlight
  #   #                       )
  # }
  # # month table
  # month_table <- data.frame(month.name = month.name,
  #                           month.abb = month.abb,
  #                           month.num = 1:12)
  # 
  # if(missing(clim_timeunits)) {
  #   
  # } 
  # 
  # if("day" %in% colnames(obj)){
  #   
  # }
  # if("week" %in% colnames(obj)){
  #   
  # }
  # if("month" %in% colnames(obj)){
  #   
  # }
  # 
  
  
}


