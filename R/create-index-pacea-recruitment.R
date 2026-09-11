##' @export
##' @rdname create_index
create_index.pacea_recruitment <- function(data,
                                           years = NULL,
                                           index_label = "Hake recruitment"){
  if(is.null(years)){
    years <- min(data$year):max(data$year)
  }

  res <- dplyr::filter(data,
                       year %in% years)

  index_name <- deparse(substitute(data))
  res <- res %>%
    dplyr::mutate(value = standardise(median),
                  index = index_name,
                  index_label = index_label) %>%
    dplyr::select(index,
                  year,
                  value,
                  index_label)

  class(res)[1] <- "pacea_standardised_index"

  res
}
