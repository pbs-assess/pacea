##' Which data objects are spatial areas (sf polygon objects)?
##'
##' This function identifies which data objects are sf (simple features)
##' objects with POLYGON or MULTIPOLYGON geometries. Useful for discovering
##' available spatial area data in the package, and automatically listing them
##' in the `spatial_areas.Rmd` vignette. Make sure to use `load_all()` when
##' developing.
##'
##' @param ignore Character vector of object names to ignore in the results.
##'   Default ignores `c("bc_coast", "grid26", "grid26_depth")` as these are
##'   supporting spatial data rather than management areas.
##'
##' @return A character vector of sf polygon/multipolygon object names in the
##'   pacea package. If no polygon sf objects are found, returns a character
##'   vector of length 0.
##'
##' @examples
##' \dontrun{
##'   which_data_are_areas()
##'   which_data_are_areas(ignore = NULL)
##' }
##'
##' @export
which_data_are_areas <- function(ignore = c("bc_coast",
                                            "grid26",
                                            "grid26_depth",
                                            "roms_cave" # not documented and not
                                            # full BCCM grid now anyway
                                            )){
  # Get all objects in the pacea package environment
  all_objects <- ls('package:pacea')

  # Check which are sf polygon/multipolygon objects
  polygon_objects <- c()
  for(obj in all_objects) {
    tryCatch({
      x <- get(obj)
      if(inherits(x, 'sf')) {
        geom_types <- unique(sf::st_geometry_type(x))
        if(any(geom_types %in% c('POLYGON', 'MULTIPOLYGON'))) {
          polygon_objects <- c(polygon_objects, obj)
        }
      }
    }, error = function(e) {})
  }

  # Filter out ignored objects
  if(!is.null(ignore)) {
    polygon_objects <- polygon_objects[!(polygon_objects %in% ignore)]
  }

  invisible(polygon_objects)
}
