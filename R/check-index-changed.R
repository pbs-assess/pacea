##' Check if a newly downloaded pacea object has changed from one currently in pacea
##'
##' If index has changed from the saved one then save the new version. Only used
##' for updating data (see files in `data-raw`).
##'
##' @param old original object (of class `pacea_index`, `pacea_recruitment`,
##'   `pacea_biomass`, or `pacea_recruitment_herring`) saved in pacea
##' @param new newly downloaded and wrangled object (must also be one of the above classes)
##' @param alpi logical if checking the `alpi` object, as comparing axis name
##'   does not work because it is convoluted. See
##'   `data-raw/coastwide-indices/alpi-update.R` for use.
##' @return logical if the object has changed (and we would then want to resave it)
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' check_index_changed(oni, oni[-10, ]))
##' # see data-raw/groundfish/data.R
##' # see data-raw/coastwide-indices/coastwide-indices.R
##' }
check_index_changed <- function(old,
                                new,
                                alpi = FALSE){
  stopifnot(class(old)[1] %in% c("pacea_index",
                                 "pacea_recruitment",
                                 "pacea_biomass",
                                 "pacea_recruitment_herring",
                                 "pacea_biomass_herring"))
  stopifnot(class(new)[1] %in% c("pacea_index",
                                 "pacea_recruitment",
                                 "pacea_biomass",
                                 "pacea_recruitment_herring",
                                 "pacea_biomass_herring"))

  # Tried testthat::expect_equal but it returned tibble of FALSE's, so need
  #  extra checks here

  index_changed = FALSE

  if(class(old)[1] != class(new)[1]){
    index_changed = TRUE
  }

  if(nrow(old) != nrow(new) |
     ncol(old) != ncol(new) |
     # Attribute comparison does not work for ALPI:
     ifelse(alpi,
            FALSE,
            attr(old, "axis_name") != attr(new, "axis_name"))) {
       index_changed = TRUE} else {
    # Dimensions are the same so check values:
  if(!(all(old == new, na.rm = TRUE)) |
     !all(names(old) == names(new))){
    index_changed = TRUE
      # Only overwrite if values have changed (else
      #  timestamp of data object will be confusingly later than
      #  the Git commit that made it, because Git won't
      #  re-commit it if no values had changed.
    }
  }
  return(index_changed)
}
