##' @export
##' @rdname create_index
create_index.pacea_harbour_seals <- function(data,
                                             years = NULL,
                                             seal_region = "WCVI",
                                             index_label = NULL,
                                             index_name = NULL,
                                             index_statistic = "mean"){
  # Need to add the region in the label
  if(is.null(index_label)){
    index_label <- paste0("Harbour seals ",
                          seal_region,
                          " population")
  }

  if(is.null(index_name)){
    index_name <- paste0(deparse(substitute(data)),
                         "_",
                         tolower(seal_region))
  }

  data_region <- data %>%
    filter(region == seal_region) %>%
    select(-c("region"))

  # Each required year to span the data
  years_in_data <- year(min(data_region$date)):year(max(data_region$date))
  # Create target dates (Jan 1 for each year)
  target_dates <- lubridate::as_date(paste0(years_in_data,
                                            "-01-01"))

  result <- tibble()

  for(i in 1:length(target_dates)){
    target <- target_dates[i]

    # Check if exact match exists, and use it
    exact_match <- filter(data_region,
                          date == target)

    if(nrow(exact_match) > 0){   # will only be 1
      this_match <- exact_match %>%
        select(date,
               low,
               mean,
               high) %>%
        mutate(year = lubridate::year(date)) %>%
        relocate(year) %>%
        select(-c("date"))

      result <- bind_rows(result,
                          this_match)
    } else {
      # Find closest date before and after
      before <- data_region %>%
        filter(date < target) %>%
        dplyr::slice_max(date, n = 1)
      after <- data_region %>%
        filter(date > target) %>%
        dplyr::slice_min(date, n = 1)

      if(nrow(before) > 0 && nrow(after) > 0) {
        # Linear interpolation
        frac <- as.numeric(target - before$date) / as.numeric(after$date - before$date)
        interp <- tibble(
          year = lubridate::year(target),   # just want year of 1st Jan date
          low = before$low + frac * (after$low - before$low),
          mean = before$mean + frac * (after$mean - before$mean),
          high = before$high + frac * (after$high - before$high)
        )
        result <- bind_rows(result,
                            interp)
      }
    }
  }

  create_index.pacea_recruitment(data = result,
                                 years = years,
                                 index_label = index_label,
                                 index_name = index_name,
                                 index_statistic = index_statistic)
}
