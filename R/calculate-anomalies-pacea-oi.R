#' TODO all the help Calculate climatology and anomalies for oisst data.
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
#' @param area TODO characters string of a named area (only `pfma_area_126` at the
#' moment, which is harwired into the function and will be removed), or
#' data.frame, tibble, or matrix with columns as latitude and longitude of
#' points around the region, with the final point equalling the first one (so
#' defining a closed  polygon). If NULL then the whole region of data. TODO NOT
#' ALL IMPLEMENTED YET. Also change create-index.R help.
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
#' @rdname calculate_anomalies
#' @author Andrew Edwards and Travis Tai
#' @examples
#' \dontrun{
#' TODO area_126 <- tibble(
#'   lat = c(-127.1506, -128.2331, -129.3492, -127.9167, -127.1847, -126.8200,
#' -127.1506),
#'   lon = c(49.85766, 49.00000, 48.99991, 50.11915, 50.40183, 50.24466, 49.85766))
#' res <- calculate_anomalies(oisst_month, area = area_126)
#' }
calculate_anomalies.pacea_oi <- function(data,                # TODO only for
                                         # monthly oisst_month
                                         climatology_years = 1991:2020,
                                         climatology_time = "month",
                                         time_period_return = "all",
                                         years_return = NULL,
                                         area = NULL){

  # Reduce data to the area specified
  if(is.null(area)){
    data_for_area <- data
  } else {
    if(!("sf" %in% class(area))){
      stop("`area` needs to be an sf object; use `create_area_sf_polygon()` to properly create your area")
    }

    data_for_area <- data[area, ]
  }

  stopifnot("'climatology_time' must currently have a value of 'month'; if you want 'week' then email Andy or make an Issue, as some of the code will need updating, and we did not think this was the most important thing to work on"
            = climatology_time %in% c("month"))
  # Some of the code and plotting code will need thinking about if we want
  # "week" to work.

  # climatology_time is irrelevant if class != pacea_buoy    TODO tidyup
  # Stick with how it comes:
  # if("pacea_st" %in% class(data)) climatology_time <- "month"   # BCCM
  if("month" %in% colnames(data)) climatology_time <- "month"
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

  # From calc_clim:
  climatology <- data_for_area %>%
    filter(year %in% climatology_years,
           month %in% time_period_return) %>%
    group_by(month,
             geometry) %>%
    summarise(clim_value = mean(sst,
                                na.rm = TRUE),
              clim_sd = sd(sst,
                           na.rm = TRUE),
              clim_n = sum(!is.na(sst_n))) %>%
    ungroup() %>%
    relocate(geometry,
             .after = last_col())

  class(climatology) <- c("pacea_oiclim",
                          "sf",
                          "tbl_df",
                          "tbl",
                          "data.frame")
  attr(climatology, "units") <- "Temperature (\u00B0C)"


  # Anomalies
  coords <- st_coordinates(data_for_area)   # take out to save time, then put back in later

  dat_nogeometry <- data_for_area %>%
    select(year,
           month,
           sst,
           start_date,
           end_date,
           geometry) %>%
    mutate(x = coords[, 1],
           y = coords[, 2]) %>%
    st_drop_geometry()

  clim_dat <- dat_nogeometry %>%         # Recalculate as same format
    filter(year %in% years_return) %>%
    group_by(month,
             x,
             y) %>%
    summarise(clim_value = mean(sst,
                                na.rm = TRUE)) %>%
    ungroup()

  anomalies <- dat_nogeometry %>%
    filter(year %in% years_return,
           month %in% time_period_return) %>%
    left_join(clim_dat,
              by = join_by(month == month,
                           x == x,
                           y == y)) %>%
    mutate(anom = sst - clim_value) %>%
    st_as_sf(coords = c("x", "y"),
             crs = st_crs(data_for_area))

  class(anomalies) <- c("pacea_oianom",
                        "sf",
                        "tbl_df",
                        "tbl",
                        "data.frame")
  attr(anomalies, "units") <- "Temperature (\u00B0C) anomaly"

  res <- list(climatology = climatology,
              anomalies = anomalies,
              climatology_years = climatology_years,
              climatology_time = climatology_time)

  class(res) <- c("pacea_oisst_anomalies_list",
                  "list")
  return(res)
}

