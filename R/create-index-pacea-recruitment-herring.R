##' @export
##' @rdname create_index
create_index.pacea_recruitment_herring <- function(data,
                                                   years = NULL,
                                                   herring_region = "WCVI",
                                                   index_label = NULL){
  if(is.null(years)){
    years <- min(data$year):max(data$year)
  }

  if(is.null(index_label)){
    index_label <- paste0("Herring ",
                          herring_region,
                          " recruitment")
  }

  res <- dplyr::filter(data,
                       year %in% years,
                       region == herring_region)

  index_name <- paste0(deparse(substitute(data)),
                       "_",
                       herring_region)
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
