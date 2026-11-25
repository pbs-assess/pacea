#' Calculate climatology
#'
#' Function for calculating climatology of a pacea data object.
#'
#' @details
#' The functions `calc_clim` and `calc_anom` output the climatology of a specified time period and anomaly values relative to a climatological period, respectively. These functions can be used for data from BCCM (e.g. `bccm_surface_temperature()`), OISST (e.g. `oisst_7day`), and buoy SST (e.g. `buoy_sst`).
#'
#' @param data pacea data object: BCCM, OISST, buoy_sst data only
#' @param clim_years climatology period years
#' @param clim_time time units (e.g. month) to summarize climatologies
#' @param time_period_return vector of value(s) for the specific time units to estimate climatologies (e.g. '4' for week 4 or April). Set to equal 'all' for all time units.
#'
#' @importFrom dplyr mutate select filter group_by summarise ungroup left_join join_by rename relocate
#' @importFrom sf st_drop_geometry st_as_sf
#' @importFrom tidyr pivot_longer
#' @importFrom lubridate year
#' @importFrom stats sd
#'
#' @return climatology of data
#' @export
#'
#' @examples
#' \dontrun{
#' # bccm data
#' pdata <- bccm_surface_temperature()
#' clim_bccm <- calc_clim(pdata)
#' head(clim_bccm)
#'
#' clim_bccm2 <- calc_clim(pdata, time_period_return = c(1, 6))
#' head(clim_bccm2)
#'
#' # oisst data
#' clim_oi <- calc_clim(oisst_7day)
#' head(clim_oi)
#'
#' # pacea buoy sst data
#' clim_buoy <- calc_clim(buoy_sst)
#' head(clim_buoy)
#'
#' clim_buoy2 <- calc_clim(buot_sst, clim_time = "week")
#' }
calc_clim <- function(data, clim_years = c(1991:2020), clim_time = "month", time_period_return = "all") {


  # month reference table
  month_table <- data.frame(month.name = month.name,
                            month.abb = month.abb,
                            month.num = 1:12)

  stopifnot("'clim_time' must have a value of 'month' or 'week'" = clim_time %in% c("month", "week"))

  # clim_time is irrelevant if class != pacea_buoy
  if("pacea_st" %in% class(data)) clim_time <- "month"
  if("month" %in% colnames(data)) clim_time <- "month"
  if("week" %in% colnames(data)) clim_time <- "week"

  # index values for time_period_return to subset from data
  if(time_period_return[1] == "all"){
    if(clim_time == "month") time_period_return <- 1:12
    if(clim_time == "week") time_period_return <- 1:53
  }

  if(clim_time == "month") {
    m_ind <- month_match(time_period_return)
    time_period_return <- m_ind
  }

  # if class is pacea_st
  if("pacea_st" %in% class(data)) {

    ind.dat <- data %>%
      mutate(ind = 1:nrow(data))

    index <- ind.dat %>% dplyr::select(ind)

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
      dplyr::filter(year %in% clim_years,
                    month %in% time_period_return) %>%
      group_by(ind, month) %>%
      summarise(clim_value = mean(value, na.rm = TRUE),
                clim_sd = sd(value, na.rm = TRUE),
                clim_n = sum(!is.na(value))) %>%
      ungroup() %>%
      left_join(index, by = join_by(ind == ind)) %>%
      dplyr::select(-ind) %>%
      st_as_sf()

    class(out) <- c("pacea_stclim", "sf", "tbl_df", "tbl", "data.frame")
    attr(out, "units") <- attributes(data)$units

    gc()
    return(out)
  }

  # if class is pacea_oi
  if("pacea_oi" %in% class(data)) {

    if("week" %in% colnames(data)){
      out <- data %>%
        filter(year %in% clim_years,
               week %in% time_period_return) %>%
        group_by(week, geometry) %>%
        summarise(clim_value = mean(sst, na.rm = TRUE),
                  clim_sd = sd(sst, na.rm = TRUE),
                  clim_n = sum(!is.na(sst_n))) %>%
        ungroup() %>%
        relocate(geometry, .after = last_col())
    }

    if("month" %in% colnames(data)){
      out <- data %>%
        filter(year %in% clim_years,
               month %in% time_period_return) %>%
        group_by(month, geometry) %>%
        summarise(clim_value = mean(sst, na.rm = TRUE),
                  clim_sd = sd(sst, na.rm = TRUE),
                  clim_n = sum(!is.na(sst_n))) %>%
        ungroup() %>%
        relocate(geometry, .after = last_col())
    }

    # warning for climatology not equal to full 30 years specified
    dat.years <- unique(data$year)
    if(!all(clim_years %in% dat.years)) {
      warning(paste0("Number of years for climatology only span ", length(dat.years)," years:", min(dat.years), " to ", max(dat.years)))
    }

    class(out) <- c("pacea_oiclim", "sf", "tbl_df", "tbl", "data.frame")
    attr(out, "units") <- "Temperature (\u00B0C)"
    return(out)
  }

  # if class is buoy_sst
  if("pacea_buoy" %in% class(data)) {

    FUN <- match.fun(clim_time)
    out <- data %>%
      mutate(year = lubridate::year(date),
             time_unit = FUN(date)) %>%
      filter(year %in% clim_years,
             time_unit %in% time_period_return) %>%
      group_by(stn_id, time_unit) %>%
      summarise(clim_value = mean(sst, na.rm = TRUE),
                clim_sd = sd(sst, na.rm = TRUE),
                clim_n = sum(!is.na(sst))) %>%
      ungroup()
    colnames(out)[which(colnames(out) == "time_unit")] <- clim_time

    class(out) <- c("pacea_buoyclim", "tbl_df", "tbl", "data.frame")
    attr(out, "units") <- "Temperature (\u00B0C)"
    return(out)
  }
}
