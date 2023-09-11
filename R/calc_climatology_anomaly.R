
#' Calculate climatology and anomaly 
#' 
#' Function for calculating climatology or anomaly of a pacea data object.
#' 
#' @details 
#' The following data objects in 'pacea' are in the absolute values of the variable: BCCM, OISST, and buoy SST data. The functions `calc_clim` and `calc_anom` output the climatology of a specified time period and anomaly values relative to a climtological period, respectively. 
#' 
#'
#' @param data pacea data object
#' @param clim_years climatology period years 
#' @param temporal_FUN temporal unit for climatology as lubridate 'date' function (for objects of class 'pacea_buoy')
#'
#' @import dplyr sf lubridate
#' @return anomaly data values of same class as input pacea data
#' @export
#'
#' @examples
#' \dontrun{
#' # bccm data
#' pdata <- bccm_surface_temperature()
#' clim_bccm <- calc_clim(pdata)
#' head(clim_bccm)
#' plot(clim_bccm)
#' 
#' # oisst data
#' clim_oi <- calc_clim(oisst_7day)
#' head(clim_oi)
#' plot(clim_oi)
#' 
#' 
#' # pacea buoy sst data
#' clim_buoy <- calc_clim(buoy_sst)
#' head(clim_buoy)
#' plot(clim_buoy)
#' }
calc_clim <- function(data, clim_years = c(1991:2020), temporal_FUN = "month") {
  
  # if class is pacea_st
  if("pacea_st" %in% class(data)) {
    
    ind.dat <- data %>%
      mutate(ind = 1:nrow(data))
    
    index <- ind.dat %>% dplyr::select(ind)
    
    # create month names
    month_table <- data.frame(month.abb = month.abb,
                              month.num = 1:12)
    
    out <- ind.dat %>% 
      st_drop_geometry() %>%
      tidyr::pivot_longer(cols = -ind, cols_vary = "slowest", names_to = "date", values_to = "value")  %>%
      mutate(year = as.numeric(substr(date, 1, 4)),
             month = as.numeric(substr(date, 6, 7))) 
    
    # warning for climatology not equal to full 30 years specified
    dat.years <- unique(out$year)
    if(!all(clim_years %in% dat.years)) {
      warning(paste0("Number of years for climatology only span ", length(dat.years)," years:", min(dat.years), " to ", max(dat.years)))
    }
    
    out <- out %>%
      dplyr::filter(year %in% clim_years) %>%
      group_by(ind, month) %>%
      summarise(clim_value = mean(value, na.rm = TRUE)) %>%
      ungroup() %>%
      left_join(month_table, by = join_by(month == month.num)) %>%
      tidyr::pivot_wider(id_cols = "ind", names_from = "month.abb", values_from = "clim_value") %>%
      left_join(index, by = join_by(ind == ind)) %>%
      dplyr::select(-ind) %>%
      st_as_sf()
    
    class(out) <- c("pacea_stclim", "sf", "tbl_df", "tbl", "data.frame")
    attr(out, "units") <- attributes$(data)$units
    
    gc()
    return(out)
  }
  
  # if class is pacea_oi
  if("pacea_oi" %in% class(data)) {
    
    if("week" %in% colnames(data)){
      out <- data %>% 
        filter(year %in% clim_years) %>% 
        group_by(week, geometry) %>% 
        summarise(tsst = mean(sst, na.rm = TRUE),
                  sst_sd = sd(sst, na.rm = TRUE),
                  sst_n = sum(!is.na(sst_n))) %>% 
        ungroup() %>%
        rename(sst = tsst) %>%
        relocate(geometry, .after = last_col()) 
    }
    
    if("month" %in% colnames(data)){
      out <- data %>% 
        filter(year %in% clim_years) %>% 
        group_by(month, geometry) %>% 
        summarise(tsst = mean(sst, na.rm = TRUE),
                  sst_sd = sd(sst, na.rm = TRUE),
                  sst_n = sum(!is.na(sst_n))) %>% 
        ungroup() %>%
        rename(sst = tsst) %>%
        relocate(geometry, .after = last_col()) 
    }
    
    class(out) <- c("pacea_oiclim", "sf", "tbl_df", "tbl", "data.frame")
    attr(out, "units") <- "Temperature (\u00B0C)"
    return(out)
  }
  
  # if class is buoy_sst
  if("pacea_buoy" %in% class(data)) {
    
    FUN <- match.fun(temporal_FUN)
    out <- data %>%
      mutate(year = lubridate::year(date),
             time_unit = FUN(date)) %>%
      filter(year %in% clim_years) %>% 
      group_by(stn_id, time_unit) %>%
      summarise(tsst = mean(sst, na.rm = TRUE),
                sst_sd = sd(sst, na.rm = TRUE),
                sst_n = sum(!is.na(sst))) %>% 
      ungroup() %>%
      rename(sst = tsst)
    colnames(out)[which(colnames(out) == "time_unit")] <- temporal_FUN
    
    class(out) <- c("pacea_buoyclim", "sf", "tbl_df", "tbl", "data.frame")
    attr(out, "units") <- "Temperature (\u00B0C)"
    return(out)
  }
  
  # if class is pacea_index?
  # if("pacea_index" %in% class(data)){
  # }
  
}


#' Calculate anomaly
#'
#' @param data 
#' @param clim_years 
#' @param temporal_FUN 
#'
#' @return anomaly data values of same class as input pacea data
#' @import dplyr sf lubridate
#' 
#' @rdname calc_clim
#' @export
#'
#' @examples
#' \dontrun{
#' }
calc_anom <- function(data, clim_years = c(1991:2020), temporal_FUN = "month"){
  
  # if class is pacea_st
  if("pacea_st" %in% class(data)) {
    
    ind.dat <- data %>% 
      mutate(ind = 1:nrow(data)) 
    
    index <- ind.dat %>% dplyr::select(ind)
    
    # convert to long format
    long.dat <- ind.dat %>% 
      st_drop_geometry() %>%
      tidyr::pivot_longer(cols = -ind, cols_vary = "slowest", names_to = "date", values_to = "value")  %>%
      mutate(year = as.numeric(substr(date, 1, 4)),
             month = as.numeric(substr(date, 6, 7)))
    
    # warning for climatology not equal to full 30 years specified
    dat.years <- unique(long.dat$year)
    if(!all(clim_years %in% dat.years)) {
      warning(paste0("Number of years for climatology only span ", length(dat.years)," years:", min(dat.years), " to ", max(dat.years)))
    }
    
    # climatology of data
    clim.dat <- long.dat %>%
      dplyr::filter(year %in% clim_years) %>%
      group_by(ind, month) %>%
      summarise(clim_value = mean(value, na.rm = TRUE)) %>%
      ungroup()
    
    out <- long.dat %>%
      left_join(clim.dat, by = join_by(month == month, ind == ind)) %>%
      mutate(anom_value = value - clim_value) %>%
      tidyr::pivot_wider(id_cols = "ind", names_from = "date", values_from = "anom_value") %>%
      left_join(index, by = join_by(ind == ind)) %>%
      dplyr::select(-ind) %>%
      st_as_sf()
    
    class(out) <- c("pacea_stanom", "sf", "tbl_df", "tbl", "data.frame")
    attr(out, "units") <- paste(attributes(data)$units, " anomaly"
    
    gc()
    return(out)
    
  }
  
  # if class is pacea_oi
  if("pacea_oi" %in% class(data)) {
    
    if("week" %in% colnames(data)){
      
      coords <- st_coordinates(data)
      dat.nogeo <- data %>%
        dplyr::select(year, week, sst, start_date, end_date, geometry) %>%
        mutate(x = coords[, 1], 
               y = coords[, 2]) %>%
        st_drop_geometry() 
      
      clim.dat <- dat.nogeo %>% 
        filter(year %in% clim_years) %>% 
        group_by(week, x, y) %>% 
        summarise(clim_value = mean(sst, na.rm = TRUE)) %>% 
        ungroup()
      
      out <- dat.nogeo %>% 
        left_join(clim.dat, by = join_by(week == week, x == x, y == y)) %>%
        mutate(anom = sst - clim_value) %>%
        st_as_sf(coords = c("x", "y"))
    }
    
    if("month" %in% colnames(data)){
      
      coords <- st_coordinates(data)
      dat.nogeo <- data %>%
        dplyr::select(year, month, sst, start_date, end_date, geometry) %>%
        mutate(x = coords[, 1], 
               y = coords[, 2]) %>%
        st_drop_geometry() 
      
      clim.dat <- dat.nogeo %>% 
        filter(year %in% clim_years) %>% 
        group_by(month, x, y) %>% 
        summarise(clim_value = mean(sst, na.rm = TRUE)) %>% 
        ungroup()
      
      out <- dat.nogeo %>% 
        left_join(clim.dat, by = join_by(month == month, x == x, y == y)) %>%
        mutate(anom = sst - clim_value) %>%
        st_as_sf(coords = c("x", "y"))
    }
    
    class(out) <- c("pacea_oianom", "sf", "tbl_df", "tbl", "data.frame")
    attr(out, "units") <- "Temperature (\u00B0C) anomaly"
    return(out)
    
  }
  
  # if class is buoy_sst
  if("pacea_buoy" %in% class(data)) {
    
    FUN <- match.fun(temporal_FUN)
    clim.dat <- data %>%
      mutate(year = lubridate::year(date),
             time_unit = FUN(date)) %>%
      filter(year %in% clim_years) %>% 
      group_by(stn_id, time_unit) %>%
      summarise(clim_value = mean(sst, na.rm = TRUE)) %>% 
      ungroup()
    
    out <- data %>%
      mutate(time_unit = FUN(date)) %>% 
      left_join(clim.dat, by = join_by(stn_id == stn_id, time_unit == time_unit)) %>%
      mutate(anom = sst - clim_value) %>%
      dplyr::select(-time_unit)
    
    class(out) <- c("pacea_buoyanom", "sf", "tbl_df", "tbl", "data.frame")
    attr(out, "units") <- "Temperature (\u00B0C) anomaly"
    return(out)
  }
  
  # if class is pacea_index?
  # if("pacea_index" %in% class(data)){
  # }
  
}