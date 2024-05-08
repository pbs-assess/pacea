##' Create herring object to then save as data (five regions so best to functionalise).
##'
##' Used in `data-raw/herring/herring.R` to create data objects; not really
##' needed by `pacea` users.

##' @param assess_yr The year of the assessment (yielding stock status in that year).
##' @author Andrew Edwards
##' @return List of recruitment and biomass objects, to then be saved in package.
##' @examples
##' \dontrun{
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

  # Recruitment
  raw_recruit <- model$mcmccalcs$recr.quants

  expect_equal(row.names(raw_recruit),
               c("5%", "50%", "95%", "MPD"))

  recruit <- raw_recruit %>%
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

  # Spawning biomass
  raw_spawning_biomass <- model$mcmccalcs$sbt.quants

  expect_equal(row.names(raw_spawning_biomass),
               c("5%", "50%", "95%", "MPD"))

  spawning_biomass <- raw_spawning_biomass %>%
    t() %>%
    cbind(year = row.names(t(raw_spawning_biomass))) %>%
    as_tibble() %>%
    select(-"MPD") %>%
    relocate(year,
             low = "5%",
             median = "50%",
             high = "95%") %>%
    type.convert(as.is = TRUE) %>%
    mutate(year = as.numeric(year),  # Make a double like oni, hake_recruitment etc.
           region = region) %>%      # Units are in 1000s of tons already,
                                     # matching the SR figures and tables, so no
                                     # conversions needed
    relocate(year,
             region)                     # Similar structure to harbour_seals

  return(list(recruit = recruit,
              spawning_biomass = spawning_biomass))
}
