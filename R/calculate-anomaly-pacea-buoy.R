#' Calculate climatology and anomaly ANDY DOING JUST buoy_sst object adapting from Travis's,
#' want to tailor the options for each pacea object, so use generics. TODO
#' putting anomaly calcs in here also, then return a list object that has
#' both. So change name to anomaly at some point.
#'
#' Function for calculating climatology of a buoy_sstpacea data object.
#' @details
#' TODO The functions `calc_clim` and `calc_anom` output the climatology of a specified time period and anomaly values relative to a climtological period, respectively. These functions can be used for data from BCCM (e.g. `bccm_surface_temperature()`), OISST (e.g. `oisst_7day`), and buoy SST (e.g. `buoy_sst`).
#'
#' @param data `buoy_sst` pacea data object
#' @param climatology_years climatology period years
#' @param climatology_time time units (e.g. month) to summarize climatologies
#' @param time_period_return vector of value(s) for the specific time units to estimate climatologies (e.g. '4' for week 4 or April). Set to equal 'all' for all time units.
#' @param years_return vector of value(s) to return the years of
#' interest. Defaults to all years in input data
#'
#' @importFrom dplyr mutate select filter group_by summarise ungroup left_join join_by rename relocate
#' @importFrom sf st_drop_geometry st_as_sf
#' @importFrom tidyr pivot_longer
#' @importFrom lubridate year
#' @importFrom stats sd
#'
#' @return TODO list object of climatology of data and anomaly, of class `pacea_buoy_anomaly_list`
#' @export
#'
#' @examples
#' \dontrun{

#' # Will integrate options into function better TODO
#' one_stn_id_example <- "C46146"
#' buoy_example <- buoy_sst %>%
#'   filter(stn_id == one_stn_id_example)
#' res <- calculate_anomaly(buoy_example,
#'                          climatology_time = "month")
#' }
calculate_anomaly.pacea_buoy <- function(data,
                                         climatology_years = c(1991:2020),
                                         climatology_time = "month",
                                         time_period_return = "all",
                                         years_return = NULL) {


  # month reference table
  month_table <- data.frame(month.name = month.name,
                            month.abb = month.abb,
                            month.num = 1:12)

  stopifnot("'climatology_time' must have a value of 'month' or 'week'" = climatology_time %in% c("month", "week"))

  # climatology_time is irrelevant if class != pacea_buoy
  if("pacea_st" %in% class(data)) climatology_time <- "month"
  if("month" %in% colnames(data)) climatology_time <- "month"
  if("week" %in% colnames(data)) climatology_time <- "week"

  # index values for time_period_return to subset from data
  if(time_period_return[1] == "all"){
    if(climatology_time == "month") time_period_return <- 1:12
    if(climatology_time == "week") time_period_return <- 1:53
  }

  if(climatology_time == "month") {
    m_ind <- month_match(time_period_return)
    time_period_return <- m_ind
  }

  if(is.null(years_return)){
    years_return <- 1800:2100
  }


  FUN <- match.fun(climatology_time)
  climatology <- data %>%
    mutate(year = lubridate::year(date),
           time_unit = FUN(date)) %>%   # think using lubridate here based on
    # choice of month or week
    filter(year %in% climatology_years,
           time_unit %in% time_period_return) %>%
    group_by(stn_id,
             time_unit) %>%
    summarise(clim_value = mean(sst,
                                na.rm = TRUE),
              clim_sd = sd(sst,
                           na.rm = TRUE),
              clim_n = sum(!is.na(sst))) %>%
    ungroup()

  # Adapting from Travis's calc_climatology_anomaly.R
  # BUT now averaging over the time_unit first and then do
  # the anomaly from the climatology.
  anomaly <- data %>%
    mutate(year = lubridate::year(date),
           time_unit = FUN(date)) %>%
    filter(year %in% years_return,
           time_unit %in% time_period_return) %>%
    group_by(stn_id,
             year,
             time_unit) %>%
    summarise(sst_mean = mean(sst,
                              na.rm = TRUE),
              sst_n = sum(!is.na(sst))) %>%
    ungroup() %>%
    left_join(climatology,
              by = join_by(stn_id == stn_id,
                           time_unit == time_unit)) %>%
    mutate(sst_anomaly = sst_mean - clim_value) %>%
    select(-c("clim_value",
              "clim_sd",
              "clim_n"))    # no point in keep repeating them

  # Now rename time_unit column to the actual unit
  colnames(climatology)[which(colnames(climatology) == "time_unit")] <- climatology_time

  class(climatology) <- c("pacea_buoy_climatology", "tbl_df", "tbl", "data.frame")

  attr(climatology, "units") <- "Temperature (\u00B0C)"


  colnames(anomaly)[which(colnames(anomaly) == "time_unit")] <- climatology_time

  class(anomaly) <- c("pacea_buoy_anomaly", "tbl_df", "tbl", "data.frame")
  attr(anomaly, "units") <- "Temperature (\u00B0C) anomaly"

  res <- list(climatology = climatology,
              anomaly = anomaly,
              climatology_years = climatology_years,
              climatology_time = climatology_time)

  class(res) <- c("pacea_buoy_anomaly_list",
                  "list")
  return(res)
}

