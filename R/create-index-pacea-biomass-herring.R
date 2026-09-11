##' @export
##' @rdname create_index
create_index.pacea_biomass_herring <- function(data,
                                               years = NULL,
                                               herring_region = "WCVI",
                                               index_label = NULL,
                                               index_name = NULL){
  # Need to add the region in
  if(is.null(index_label)){
    index_label <- paste0("Herring ",
                          herring_region,
                          " biomass")
  }

  if(is.null(index_name)){
    index_name <- paste0(deparse(substitute(data)),
                         "_",
                         tolower(herring_region))
  }

  create_index.pacea_recruitment_herring(data = data,
                                         years = years,
                                         herring_region = herring_region,
                                         index_label = index_label,
                                         index_name = index_name)
}
