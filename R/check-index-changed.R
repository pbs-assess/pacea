##' Check if a newly downloaded index has changed from one currently in pacea
##'
##' If index has changed from the saved one then save the new version.
##'
##' @param old original index (`pacea_t` object) saved in pacea
##' @param new newly downloaded and wrangled index (`pacea_t` object)
##' @return logical if the index has changed (and we would then want to resave it)
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' check_index_changed(oni, oni[-10, ]))
##' }
check_index_changed <- function(old, new){
  stopifnot(class(old)[1] %in% c("pacea_index",
                                 "pacea_recruitment",
                                 "pacea_biomass"))
  stopifnot(class(new)[1] %in% c("pacea_index",
                                 "pacea_recruitment",
                                 "pacea_biomass"))

  # Tried testthat::expect_equal but it returned tibble of FALSE's, so need
  #  extra checks here

  index_changed = FALSE

  if(class(old)[1] != class(new)[1]){
    index_changed = TRUE
  }

  if(nrow(old) != nrow(new) |
     ncol(old) != ncol(new) |
     attr(old, "axis_name") != attr(new, "axis_name")) {
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
