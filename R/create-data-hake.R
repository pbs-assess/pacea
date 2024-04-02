##' Create package data with one call. Taken from hake package.
##'
##' Usually you have to create a variable first, and then call
##'  [usethis::use_data()] with the variable name as an argument and the
##'  package data will be created. This function just unifies that into one step,
##'  and allows automated naming with `paste0()`, unlike `usethis::use_data()`.
##'  Keeping hake in the name for consistency with the original function, but this
##'  will work for any data object. See `data-raw/groundfish/hake.R` for example use.
##' @param var The variable name
##' @param val The value for variable `var`
##' @author Chris Grandin (help file added to by Andrew Edwards)
##' @return Nothing, creates package data. See `usethis::use_data()`.
##' @examples
##' \dontrun{
##' In data-raw/groundfish/hake.R see these lines (which allow `assess_yr` to
#'   part of the data object, which `use_data()` does not):
##'  create_data_hake(paste0("hake_recruitment_", assess_yr),
##'                   get(paste0("hake_recruitment_", assess_yr)))
##' }
##' @export
create_data_hake <- function(var, val){

  assign(var, val)
  do.call(usethis::use_data,
          list(as.name(var),
               overwrite = TRUE))
}
