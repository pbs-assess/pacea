#' Calculate climatology and anomalies for buoy SST data.
#'
#' Function for calculating climatology and subsequent anomalies of a `buoy_sst` pacea data object.
#'
#' @param data `buoy_sst` pacea data object
#' @param climatology_years climatology period years
#' @param climatology_time time units ("month" or "week") to summarize
#' climatologies and anomalies, but only "month" is currently available (there
#' is some functionality to use "week" also -- contact Andy if this would be useful).
#' @param time_period_return vector of value(s) for the specific time units to estimate climatologies (e.g. '4' for week 4 or April). Set to equal 'all' for all time units.
#' @param years_return vector of value(s) to return the years of
#' interest. Defaults to all years in input data
#' @param min_days_per_month minimum number of daily SST values required in a
#' time period (month or week) for that period to be included in climatology
#' calculation and anomalies calculation. Defaults to 15 days per month.
#' @param max_consecutive_missing_days maximum number of consecutive days allowed
#' to be missing (NA) within a time period (month or week). If a time period has
#' more than this many consecutive NAs, it is excluded from climatology and anomalies
#' calculation. Defaults to 6 days.
#'
#' @importFrom dplyr mutate select filter group_by summarise ungroup left_join join_by rename relocate
#' @importFrom sf st_drop_geometry st_as_sf
#' @importFrom tidyr pivot_longer
#' @importFrom lubridate year
#' @importFrom stats sd
#'
#' @return pacea_buoy_anomalies_list object (also a list) of climatology of data and anomalies, of class
#' `pacea_buoy_anomalies_list`. Note that `climatology_years` will be the
#' prescribed years, but these may not be available for all buoys.
#' @export
#'
#' @examples
#' \dontrun{
#' # See [climatology and anomalies vignette](https://pbs-assess.github.io/pacea/vignettes/buoy_anomalies.html)
#' }
calculate_anomalies.pacea_buoy <- function(data,
                                         climatology_years = 1991:2020,
                                         climatology_time = "month",
                                         time_period_return = "all",
                                         years_return = NULL,
                                         min_days_per_month = 15,
                                         max_consecutive_missing_days = 6) {

  stopifnot("'climatology_time' must currently have a value of 'month'; if you want 'week' then email Andy or make an Issue, as some of the code will need updating, and we did not think this was the most important thing to work on"
            = climatology_time %in% c("month"))
  # Some of the code and plotting code will need thinking about if we want
  # "week" to work.

  # climatology_time is irrelevant if class != pacea_buoy
  # if("pacea_st" %in% class(data)) climatology_time <- "month"
  # if("month" %in% colnames(data)) climatology_time <- "month"
  # if("week" %in% colnames(data)) climatology_time <- "week"

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

  # First pass: count days per stn_id/year/time_unit in climatology period
  insufficient_data <- data %>%
    mutate(year = lubridate::year(date),
           time_unit = FUN(date)) %>%  # lubridate based on choice of month or week
    filter(year %in% climatology_years,
           time_unit %in% time_period_return) %>%
    group_by(stn_id,
             year,
             time_unit) %>%
    summarise(days_not_NA = sum(!is.na(sst))) %>%
    ungroup() %>%
    filter(days_not_NA < min_days_per_month)

  # Detect excessive consecutive missing days in climatology period
  excessive_gaps <- data %>%
    mutate(year = lubridate::year(date),
           time_unit = FUN(date),
           true_days_in_month = lubridate::days_in_month(date)) %>%
    filter(year %in% climatology_years,
           time_unit %in% time_period_return) %>%
    group_by(stn_id,
             year,
             time_unit) %>%
    mutate(sst_na = is.na(sst),
           na_run_id = dplyr::consecutive_id(sst_na)) %>%
    group_by(na_run_id,
             .add = TRUE) %>%
    mutate(na_streak_length = dplyr::if_else(is.na(sst),
                                             n(),
                                             0)) %>%  # still has every day
    ungroup() %>%
    group_by(stn_id,
             year,
             time_unit) %>%
    summarise(max_na_streak_length = max(na_streak_length),
              data_days_in_month = n(),
              true_days_in_month = unique(true_days_in_month)) %>%
    ungroup() %>%
    # But then if some days in a month have no data, need to think of those as NA's. And
    # this is only at the start of each month, as done by running the above part
    # to here
    # and checking:
    # filter(excessive_gaps, data_days_in_month != true_days_in_month) %>% a()
    # each buoy only had short months at only the start and end of its time series.
    # Except C46303 which started on 2019-10-01 so that is not short.
    # C46181 started on 1988-12-07, but had a big run of NA's from 1988-12-14,
    # so max streak is 18. Only that and C46304 have a longer NA streak than the
    # missing days.
    mutate(missing_days_in_month = true_days_in_month - data_days_in_month) %>%
    mutate(max_na_streak_length = pmax(max_na_streak_length,
                                       missing_days_in_month))  %>%
    filter(max_na_streak_length > max_consecutive_missing_days) %>%
    select(stn_id, year, time_unit)

  # Combine insufficient_data and excessive_gaps
  data_to_exclude_clim <- bind_rows(insufficient_data %>% select(stn_id, year, time_unit),
                                    excessive_gaps)

  # Second pass: set SST to NA for insufficient data, then calculate climatology
  climatology <- data %>%
    mutate(year = lubridate::year(date),
           time_unit = FUN(date)) %>%
    filter(year %in% climatology_years,
           time_unit %in% time_period_return) %>%
    mutate(exclude = interaction(stn_id,
                                 year,
                                 time_unit) %in%
                     interaction(data_to_exclude_clim$stn_id,
                                 data_to_exclude_clim$year,
                                 data_to_exclude_clim$time_unit)) %>%
    mutate(sst = ifelse(exclude, NA, sst)) %>%
    select(-exclude) %>%
    group_by(stn_id,
             time_unit) %>%
    summarise(clim_sst_mean = mean(sst,
                                na.rm = TRUE),
              clim_sst_sd = sd(sst,
                           na.rm = TRUE),
              clim_sst_n = sum(!is.na(sst))) %>%
    ungroup() %>%
    # Replace all NaNs with NAs
    mutate(across(where(is.numeric),
                  ~ ifelse(is.nan(.x), NA, .x)))


  # Adapting from Travis's calc_climatology_anomaly.R
  # BUT now averaging over the time_unit first and then do
  # the anomalies from the climatology.

  # First pass: count days per stn_id/year/time_unit for all years
  insufficient_data_anomalies <- data %>%
    mutate(year = lubridate::year(date),
           time_unit = FUN(date)) %>%
    filter(year %in% years_return,
           time_unit %in% time_period_return) %>%
    group_by(stn_id,
             year,
             time_unit) %>%
    summarise(days_not_NA = sum(!is.na(sst))) %>%
    ungroup() %>%
    filter(days_not_NA < min_days_per_month)

  # Detect excessive consecutive missing days in anomaly period
  excessive_gaps_anomalies <- data %>%
    mutate(year = lubridate::year(date),
           time_unit = FUN(date),
           true_days_in_month = lubridate::days_in_month(date)) %>%
    filter(year %in% years_return,
           time_unit %in% time_period_return) %>%
    group_by(stn_id,
             year,
             time_unit) %>%
    mutate(sst_na = is.na(sst),
           na_run_id = dplyr::consecutive_id(sst_na)) %>%
    group_by(na_run_id,
             .add = TRUE) %>%
    mutate(na_streak_length = dplyr::if_else(is.na(sst),
                                             n(),
                                             0)) %>%  # still has every day
    ungroup() %>%
    group_by(stn_id,
             year,
             time_unit) %>%
    summarise(max_na_streak_length = max(na_streak_length),
              data_days_in_month = n(),
              true_days_in_month = unique(true_days_in_month)) %>%
    ungroup() %>%
    mutate(missing_days_in_month = true_days_in_month - data_days_in_month) %>%
    mutate(max_na_streak_length = pmax(max_na_streak_length,
                                       missing_days_in_month))  %>%
    filter(max_na_streak_length > max_consecutive_missing_days) %>%
    select(stn_id, year, time_unit)

  # Combine insufficient_data_anomalies and excessive_gaps_anomalies
  data_to_exclude_anom <- bind_rows(insufficient_data_anomalies %>% select(stn_id, year, time_unit),
                                     excessive_gaps_anomalies)

  # Second pass: set SST to NA for insufficient data, then calculate anomalies
  anomalies <- data %>%
    mutate(year = lubridate::year(date),
           time_unit = FUN(date)) %>%
    filter(year %in% years_return,
           time_unit %in% time_period_return) %>%
    mutate(exclude = interaction(stn_id, year, time_unit) %in%
                     interaction(data_to_exclude_anom$stn_id,
                                 data_to_exclude_anom$year,
                                 data_to_exclude_anom$time_unit)) %>%
    mutate(sst = ifelse(exclude,
                        NA,
                        sst)) %>%
    select(-exclude) %>%
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
    mutate(sst_anomaly = sst_mean - clim_sst_mean) %>%
    select(-c("clim_sst_mean",
              "clim_sst_sd",
              "clim_sst_n")) %>% # no point in keep repeating them
   # Replace all NaNs with NAs
    mutate(across(where(is.numeric),
                  ~ ifelse(is.nan(.x), NA, .x)))

  # Now rename time_unit column to the actual unit
  colnames(climatology)[which(colnames(climatology) == "time_unit")] <- climatology_time

  class(climatology) <- c("pacea_buoy_climatology", "tbl_df", "tbl", "data.frame")

  attr(climatology, "units") <- "Temperature (\u00B0C)"


  colnames(anomalies)[which(colnames(anomalies) == "time_unit")] <- climatology_time

  class(anomalies) <- c("pacea_buoy_anomalies", "tbl_df", "tbl", "data.frame")
  attr(anomalies, "units") <- "Temperature (\u00B0C) anomalies"

  # Had thought likely need climatology_years per stn_id, though it's kind of obvious
  # once plotted (climatology cannot start in 1990 if data starts in 1995), and
  # so just keep it simple to use for figure title.
  res <- list(climatology = climatology,
              anomalies = anomalies,
              climatology_years = climatology_years,
              climatology_time = climatology_time)

  class(res) <- c("pacea_buoy_anomalies_list",
                  "list")
  return(res)
}

