##' Create herring object to then save as data (five regions so best to functionalise).
##'
##' Used in `data-raw/herring/herring.R` to create data objects; not really
##' needed by `pacea` userse.
##'
##' TODO
##' Usually you have to create a variable first, and then call
##'  [usethis::use_data()] with the variable name as an argument and the
##'  package data will be created. This function just unifies that into one step,
##'  and allows automated naming with `paste0()`, unlike `usethis::use_data()`.
##'  Change name from original hake::create_data_hake(), as
##'  will work for any data object. See `data-raw/groundfish/hake.R` for example use.
##' @param assess_yr The year of the assessment (yielding stock status in that year).
##' @author Andrew Edwards
##' @return List of recruitment and biomass objects, to then be saved in package.
##' @examples
##' \dontrun{
##' In data-raw/groundfish/hake.R see these lines (which allow `assess_yr` to
#'   part of the data object, which `use_data()` does not):
##'  create_data(paste0("hake_recruitment_", assess_yr),
##'              get(paste0("hake_recruitment_", assess_yr)))
##' }
##' @export
create_herring_object <- function(assess_yr,
                                  region,
                                  herring_dir = NULL){

  if(is.null(herring_dir)){
    herring_dir <- paste0(here::here(),
                          "/data-raw/herring/herring-assessment-",
                          assess_yr,
                          "/")
  }


  load(file = paste0(herring_dir,
                     region,
                     "_aaa_gfiscam.RData"))

  raw_recruit <- model$mcmccalcs$recr.quants

  expect_equal(row.names(raw_recruit),
               c("5%", "50%", "95%", "MPD"))

  recruit <- model$mcmccalcs$recr.quants %>%
    t() %>%
    cbind(year = row.names(t(raw_recruit))) %>%
    as_tibble() %>%
    select(-"MPD") %>%
    relocate(year,
             low = "5%",
             median = "50%",
             high = "95%") %>%
    type.convert(as.is = TRUE) %>%
    mutate(year = as.numeric(year),  # Make a double like oni, hake_recruitment etc.
           region = region,
           low = low / 1000,
           median = median / 1000,
           high = high / 1000) %>%       # Convert from millions to billions (to match
    relocate(year,                       #  SR Figures).
             region)                     # Similar structure to harbour_seals

    # Spawning biomass (Table 19)
# model$mcmccalcs$sbt.quants
# row.names(model$mcmccalcs$sbt.quants)
  return(recruit)  # Then change to list with biomass also
}
