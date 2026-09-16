##' @export
##' @rdname create_index
create_index.pacea_buoy <- function(data,
                                    years = NULL,
                                    months = 4,
                                    stn_id = "C46205",  # can be name also
                                    index_label = NULL,
                                    index_name = NULL,
                                    buoy_statistic = "anomalies",  # or mean
                                    # TODO add to help
                                    # index_statistic = "sst_anomaly_mean", # TODO, maybe
                                    # not needed
                                    require_requested_months = NULL,
                                    ...){

  # data is `buoy_sst` pacea data object, basically just the full daily buoy_sst as
  # will filter for the stn_id straight away.

  # Need to filter for buoy
  # Do calculate_anomalies which returns monthly anomalies relative to a monthly
  # climatology, and has the options for min_days_per_month and
  # max_consecutive_missing_days that we can pass on from here.

  # Then plot.pacea_buoy_anomalies_list() does some of the aggregating for
  # multiple months. Think we just adapt some of that here to use, as calling
  # that seems a bit overkill.

  stopifnot(length(stn_id) == 1)

  # If stn_id does not start with "C4" then it is the name, so replace
  #  it with it's stn_id code for filtering.
  if(!startsWith(stn_id,
                 "C4")){
    station <- buoy_metadata$stn_id[match(stn_id,
                                          buoy_metadata$name)]
    if(is.na(station)){
      stop("You have mis-spelled the buoy name in `stn_id`")
    }
  }

  station <- stn_id      # Can't use stn_id in filter(stn_id == stn_id)

  # Check months are consecutive except Dec to Jan
  if(!is.null(months)){
    diffs <- diff(months)
    if(length(diffs) > 0){
      # All diffs should be 1, except possibly one -11 (wrapping over end of year)
      invalid_diffs <- diffs[!(diffs == 1 | diffs == -11)]
      n_wrap_around <- sum(diffs == -11)
      if(length(invalid_diffs) > 0 || n_wrap_around > 1){
        stop("'months' must be consecutive, e.g. 1:5, or like c(11, 12, 1, 2) for wrapping over the end of the year.")
      }
    }
  }

  # Set default require_requested_months
  if(is.null(require_requested_months)){
    require_requested_months <- length(months)
  } else {
    if(require_requested_months > length(months)){
      stop("Need `require_requested_months <= length(months)`")
    }
  }

  if(is.null(index_label)){
    index_label <- filter(buoy_metadata,
                          stn_id == station) %>%
      dplyr::pull(name) %>%
      paste0(" buoy SST for ",
             summarise_months(months))
  }

  if(is.null(index_name)){
    index_name <-
      filter(buoy_metadata,
             stn_id == station) %>%
      dplyr::pull(name) %>%
      tolower() %>%
      stringr::str_replace_all(" ",
                               "_") %>%
      paste0("_sst_",
             tolower(summarise_months(months)))
  }

  # Just the one station and then the desired years
  data_stn <- filter(data,
                     stn_id == station) %>%
    mutate(year = lubridate::year(date))

  if(is.null(years)){
    years <- min(data_stn$year):max(data_stn$year)
  }

  # So have just the required years for the single station.
  data_stn <- dplyr::filter(data_stn,
                           year %in% years)

  res_calculate_anomalies <- calculate_anomalies(data_stn,   # class pacea_buoy
                                                 ...)
  # that has class pacea_buoy_anomalies_list.

  # Below will take what we need from plot.pacea_buoy_anomalies_list() and make an index


  # Copied from plot.pacea_buoy_anomalies_list:

  # HERE
  # Determine which column to filter NA values from. TODO internally   not
  # correct yet as maybe need after the calcs, not actually used TODO fix
  # na_check_col <- switch(buoy_statistic,
  #                       "anomalies" = "sst_anomaly",
  #                       "mean" = "sst_mean")

  if(which.max(months) == length(months)){
    # months are increasing and so are in the same year

    res <- res_calculate_anomalies$anomalies %>%
      dplyr::filter(month %in% months) %>%
      dplyr::group_by(year) %>%
      # value becomes the average over the specified months, no need to
      # keep month column
      dplyr::summarise(
        n_available_anomalies = sum(!is.na("anomalies")),
        n_available_mean = sum(!is.na("mean")),
        sst_mean_of_monthly_anomalies = ifelse(n_available_anomalies >= require_requested_months,
                                               mean(sst_anomaly,
                                                    na.rm = TRUE),
                                               NA),
        sst_mean_of_monthly_means = ifelse(n_available_mean >= require_requested_months,
                                           mean(sst_mean,
                                                na.rm = TRUE),
                                           NA)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(sst_mean_of_monthly_anomalies) | !is.na(sst_mean_of_monthly_means)) %>%
      dplyr::select(-c("n_available_anomalies",
                       "n_available_mean"))
  } else {
    # Months are not increasing, for which it is implied a winter average is
    # being calculated that includes Dec and Jan.
    res <- res_calculate_anomalies$anomalies %>%
      dplyr::filter(month %in% months) %>%
      dplyr::mutate(year_of_january = (year + 1) * (month >= months[1]) +
                      year * (month < months[1])) %>%    # the year of the january for the winter
      dplyr::group_by(year_of_january) %>%
      # value becomes the average over the specified months, no need to keep month column
      dplyr::summarise(
        # TODO copy same as above
        # n_available = sum(!is.na(.data[[na_check_col]])),
        sst_mean_of_monthly_anomalies = ifelse(n_available >= require_requested_months,
                                  mean(sst_anomaly,
                                       na.rm = TRUE),
                                  NA),
        sst_mean_of_monthly_means = ifelse(n_available >= require_requested_months,
                               mean(sst_mean,
                                    na.rm = TRUE),
                               NA)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(sst_mean_of_monthly_anomalies) | !is.na(sst_mean_of_monthly_means)) %>%
      #  TODO check all as above
      dplyr::select(-n_available) %>%
      dplyr::rename(year = year_of_january)
  }

  browser()
  index_statistic <- switch(buoy_statistic,
                           "anomalies" = "sst_mean_of_monthly_anomalies",
                           "mean" = "sst_mean_of_monthly_means")

  # Want to remove any NA's? Probably.
  ret <- create_index.pacea_recruitment(data = res,
                                        years = years,
                                        index_label = index_label,
                                        index_name = index_name,
                                        index_statistic = index_statistic)
  ret
}
