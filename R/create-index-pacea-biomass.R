##' @export
##' @rdname create_index
create_index.pacea_biomass <- function(data,
                                       years = NULL,
                                       index_label = "Hake spawning biomass"){
  index_name <- deparse(substitute(data))
  create_index.pacea_recruitment(data = data,
                                 years = years,
                                 index_label = index_label,
                                 index_name = index_name)
}
