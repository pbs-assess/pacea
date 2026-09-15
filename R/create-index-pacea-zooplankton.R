##' @export
##' @rdname create_index
create_index.pacea_zooplankton <- function(data,
                                           years = NULL,
                                           zooplankton_species_group = "total_biomass",
                                           index_label = NULL,
                                           index_name = NULL,
                                           index_statistic = "anomaly"){
  stopifnot("zooplankton_species_group must be a column of the zooplankton_sog object" =
              zooplankton_species_group %in% names(data))

  res <- dplyr::select(data,
                       year,
                       anomaly = eval(zooplankton_species_group))

  # Need to make a label for plotting in the ecosystem summary heatmap
  if(is.null(index_label)){
    index_label <- filter(zooplankton_sog_axis_names,
                          species_group_name == zooplankton_species_group) %>%
      dplyr::pull(heatmap_axis_name)
  }

  if(is.null(index_name)){
    index_name <- paste0(zooplankton_species_group,
                         "_",
                         "sog")
  }

  # Can use this master one, having wrangled the data here
  create_index.pacea_recruitment(data = res,
                                 years = years,
                                 index_label = index_label,
                                 index_name = index_name,
                                 index_statistic = index_statistic)
}
