##' Restrict two time series to only the overlapping dates (only years for now)
##'
##' TODO Extend to all the classes of pacea objects, and make it work on non
##' pacea objects also.
##'##'
##' @param obj_1 data.frame first time series
##' @param obj_2 data.frame second time series
##' @param region character corresponding the region to be used for
##'   herring-related objects (of class `pacea_recruitment_herring` or
##' `pacea_biomass_herring`)
##' @return tibble with first column `year` and the rest being the columns of
##' `obj_1` and `obj_2` restricted to the overlapping years
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' overlap(hake_recruitment, bi)
##' overlap(herring_spawning_biomass,
##' }
overlap <- function(obj_1,
                    obj_2,
                    region = "SOG"){

  obj_1_use <- obj_1
  obj_2_use <- obj_2

  # If herring then restrict to specified region
  if(class(obj_1)[1] %in% c("pacea_recruitment_herring",
                            "pacea_biomass_herring")){
    obj_1_use <- dplyr::filter(obj_1,
                               region == region)
  }

  if(class(obj_2)[1] %in% c("pacea_recruitment_herring",
                            "pacea_biomass_herring")){
    obj_2_use <- dplyr::filter(obj_2,
                               region == region)
  }

  if("month" %in% names(obj_1_use) | "month" %in% names(obj_2_use)){
    stop("Not set up yet to deal with objects that have months")
  }

  if( !("year" %in% names(obj_1_use) & "year" %in% names(obj_2_use))){
    stop("Need year as a column in both objects")
  }

  res <- dplyr::inner_join(obj_1_use,
                           obj_2_use,
                           by = "year")
  # TODO maybe tailor the suffix argument to give more meaningful column names?
                           # Or can just integrate this function with a plotting one

  res
}
