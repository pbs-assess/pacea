##' @export
##' @rdname create_index
create_index_oni <- function(data,  # TODO change to .pacea_index when
                                     # got going, then need to check other
                                     # indices; annual ones will be different
                                  years = NULL,
                                  index_label = NULL,
                                  index_name = NULL,
                                  index_statistic = "mean_of_monthly_anomalies",
                                  months = NULL,
                                  require_requested_months = NULL,
                                  standardise_index = TRUE,
                                  ...){

  # Need a different function for the annual indices that do not have months.

  if(is.null(months)){
    months <- 4
  }

  # TODO remove stopifnot(index_statistic %in% c("anomalies"))    # TODO help, note it's the mean
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
      paste0("Mean ONI anomaly for ",
             summarise_months(months))
    # TODO think if that makes sense for one month
  }

  if(is.null(index_name)){
    index_name <-
      paste0("oni_",
             tolower(summarise_months(months))) %>%
      tolower() %>%
      stringr::str_replace_all(" ",
                               "_")
  }

  if(is.null(years)){
    years <- min(data$year):max(data$year)
  }

  anomalies_to_use <- filter(data,
                             year %in% years,
                             month %in% months)
  # Adapting from create-index.pacea_buoy()
  if(which.max(months) == length(months)){
    # months are increasing and so are in the same year
    res <- anomalies_to_use %>%
      dplyr::group_by(year) %>%
      # value becomes the average over the specified months, no need to
      # keep month column, but need to check we have enough months (e.g. might
      # not in the current year)
      dplyr::summarise(
        n_available_anomalies = sum(!is.na(anomaly)),
        mean_of_monthly_anomalies =
          ifelse(n_available_anomalies >= require_requested_months,
                 mean(anomaly,
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
        n_available_anomalies = sum(!is.na(anomaly)),
        mean_of_monthly_anomalies =
          ifelse(n_available_anomalies >= require_requested_months,
                 mean(anomaly,
                      na.rm = TRUE),
                 NA)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c("n_available_anomalies")) %>%
      dplyr::ungroup() %>%
      dplyr::rename(year = year_of_january)
  }

  ret <- create_index.pacea_recruitment(data = res,
                                        years = years,
                                        index_label = index_label,
                                        index_name = index_name,
                                        index_statistic = index_statistic,
                                        standardise_index = standardise_index)
  ret
}
