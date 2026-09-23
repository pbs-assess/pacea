##' @export
##' @rdname create_index
create_index.pacea_oisst_anomalies_list <- function(data,
                                                    years = NULL,
                                                    index_label = NULL,
                                                    index_name = NULL,
                                                    index_statistic = "anomalies",
                                                    area = NULL,
                                                    require_requested_months = NULL,
                                                    ...){
  stopifnot(index_statistic %in% c("anomalies"))    # TODO help, note it's the mean
  # of the anomalies

  anomalies_sf <- data[["anomalies"]]  # The anomalies sf object

  if(is.null(years)){
    years <- min(anomalies_sf$year):max(anomalies_sf$year)
  }

  # Extract months from ..., default to 4 if not provided
  months <- list(...)$months

  if(is.null(months)){
    months <- 4
  }

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
    index_label <-
      paste0("Mean OISST anomaly for ",
             summarise_months(months))
    # TODO think if that makes sense for one month
  }

  if(is.null(index_name)){
    index_name <-
      paste0("oisst_",
             tolower(summarise_months(months))) %>%
      tolower() %>%
      stringr::str_replace_all(" ",
                               "_")
  }

  data_to_use <- filter(anomalies_sf,
                        year %in% years,
                        month %in% months)
  if(!is.null(area)){
    data_to_use <- sf::st_filter(data_to_use,
                                 area,
                                 .predicate = sf::st_within)   # TODO check with
    # Travis
  }

  data_to_use <- sf::st_drop_geometry(data_to_use)    # done with the spatial

  # Adapting from create-index.pacea_buoy()
  if(which.max(months) == length(months)){
    # months are increasing and so are in the same year
    res <- data_to_use %>%
      dplyr::group_by(year) %>%
      # value becomes the average over the specified months, no need to
      # keep month column
      dplyr::summarise(
        # Although OISST should have values every month, still need to check we
        #  have enough data in the requested months. Not doing the mean absolute
        #  sst. Might need it if buoy ones end up not being the same for anomaly
        #  and mean absolute SST.
        n_available_anomalies = sum(!is.na(anom)),
        oisst_mean_of_monthly_anomalies = ifelse(n_available_anomalies >= require_requested_months,
                                               mean(anom,
                                                    na.rm = TRUE),
                                               NA)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c("n_available_anomalies"))
  } else {
    # Months are not increasing, for which it is implied a winter average is
    # being calculated that includes Dec and Jan.
    res <- data_to_use %>%
      dplyr::mutate(year_of_january = (year + 1) * (month >= months[1]) +
                      year * (month < months[1])) %>%    # the year of the january for the winter
      dplyr::group_by(year_of_january) %>%
      # value becomes the average over the specified months, no need to
      # keep month column
      dplyr::summarise(
        # Although OISST should have values every month, still need to check we
        #  have enough data in the requested months. Not doing the mean absolute
        #  sst. Might need it if buoy ones end up not being the same for anomaly
        #  and mean absolute SST.
        n_available_anomalies = sum(!is.na(anom)),
        oisst_mean_of_monthly_anomalies = ifelse(n_available_anomalies >= require_requested_months,
                                               mean(anom,
                                                    na.rm = TRUE),
                                               NA)) %>%
      dplyr::ungroup() %>%
        dplyr::select(-c("n_available_anomalies")) %>%
      dplyr::ungroup() %>%
      dplyr::rename(year = year_of_january)
  }

  index_statistic <- switch(index_statistic,
                            "anomalies" = "oisst_mean_of_monthly_anomalies")

  ret <- create_index.pacea_recruitment(data = res,
                                        years = years,
                                        index_label = index_label,
                                        index_name = index_name,
                                        index_statistic = index_statistic)
  ret
}
