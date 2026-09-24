##' @export
##' @rdname create_index
create_index.pacea_oi <- function(data,
                                  years = NULL,
                                  index_label = NULL,
                                  index_name = NULL,
                                  index_statistic = "anomalies",
                                  area = NULL,
                                  months = NULL,
                                  require_requested_months = NULL,
                                  ...){
  # data is oisst_monthly or similar, need some checks as to what can be used
  # TODO

  # Originally did this on the anomalies list, but now creating that directly
  # like for create_index.pacea_buoy(). TODO can delete this when finalised it.

  # Don't think need this here, and want months to be in ?create_index, so
  # making explicit. Think ... was for something more complex.
  # Extract months from ..., default to 4 if not provided
  # months <- list(...)$months

  if(is.null(months)){
    months <- 4
  }

  stopifnot(index_statistic %in% c("anomalies"))    # TODO help, note it's the mean
  # of the anomalies

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

  # Could reduce the years and months here but there is `climatology_years` also
  # which can be an option. So might be calculating un-needed things (e.g. Dec
  # when you only want Apr). TODO reduce the data here if running speed becomes
  # an issue.

  if(is.null(years)){
    years <- min(data$year):max(data$year)
  }

  res_calculate_anomalies <- dots_parser(calculate_anomalies.pacea_oi,
                                         data = data,
                                         ...)    # TODO check if can include years_return

  anomalies_to_use <- filter(res_calculate_anomalies$anomalies,
                             year %in% years,
                             month %in% months)
  if(!is.null(area)){
    anomalies_to_use <- sf::st_filter(anomalies_to_use,
                                      area,
                                      .predicate = sf::st_intersects)   # TODO
    # Also, area is an argument in calculate_anomalies.pacea_oi so prob
    # dont' need to filter on it once I have that working. TODO come back to.
  }

  anomalies_to_use <- sf::st_drop_geometry(anomalies_to_use)    # done with the spatial

  # Adapting from create-index.pacea_buoy()
  if(which.max(months) == length(months)){
    # months are increasing and so are in the same year
    res <- anomalies_to_use %>%
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
    res <- anomalies_to_use %>%
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
