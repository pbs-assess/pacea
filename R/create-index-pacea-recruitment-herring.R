##' @export
##' @rdname create_index
create_index.pacea_recruitment_herring <- function(data,
                                                   years = NULL,
                                                   herring_region = "WCVI",
                                                   index_label = NULL,
                                                   index_name = NULL,
                                                   index_statistic = "median"){
  result <- filter(data,
                   region == herring_region) %>%
    select(-c("region"))

  if(is.null(index_label)){
    index_label <- paste0("Herring ",
                          herring_region,
                          " age-2 recruitment")
  }

  if(is.null(index_name)){
    index_name <- paste0(deparse(substitute(data)),
                         "_",
                         tolower(herring_region))
  }

  create_index.pacea_recruitment(data = result,
                                 years = years,
                                 index_label = index_label,
                                 index_name = index_name,
                                 index_statistic = index_statistic)
}
