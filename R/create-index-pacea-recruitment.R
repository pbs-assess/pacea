##' @export
##' @rdname create_index
create_index.pacea_recruitment <- function(data,
                                           years = NULL,
                                           index_label = "Hake age-0 recruitment",
                                           index_name = NULL,
                                           index_statistic = "median",
                                           standardise_index = TRUE){
  if(is.null(years)){
    years <- min(data$year):max(data$year)
  }

  res <- dplyr::filter(data,
                       year %in% years)

  if(is.null(index_name)){
    index_name <- deparse(substitute(data))
  }

  if(standardise_index){
    res <- res %>%
      dplyr::mutate(value = standardise(.data[[index_statistic]]))
  } else {
    res <- res %>%
      dplyr::mutate(value = .data[[index_statistic]])
  }

  res <- res %>%
    dplyr::mutate(index = index_name,
                  index_label = index_label) %>%
    dplyr::select(index,
                  year,
                  value,
                  index_label)

  class(res)[1] <- "pacea_standardised_index"

  res
}
