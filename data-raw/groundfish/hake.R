# Outputs from the hake stock assessment. Run code line-by-line and check plots.
#  See ?hake for details.

load_all()

# Get the .rda files from Andy running
#  hake-assessment/sandbox/andy/pacea-save/pacea-save.R
#  after 2023 hake assessment and copying it here.
#  For future assessments will maybe convert that to
#  a function. Check with Andy or Chris Grandin.

# Recruitment

load("hake_recruitment_new.rda")                #

class(hake_recruitment_new) <- c("pacea_t",
                                 class(hake_recruitment_new))

attr(hake_recruitment_new, "axis_name") <-
  "Pacific Hake recruitment (billions of age-0 fish)"

if(check_index_changed(hake_recruitment,
                       hake_recruitment_new)){
  hake_recruitment <- hake_recruitment_new
  usethis::use_data(hake_recruitment,
                    overwrite = TRUE)
  plot(hake_recruitment, value = "val", style = "plain")
}

# Spawning biomass

load("hake_biomass_new.rda")                #

class(hake_biomass_new) <- c("pacea_t",
                             class(hake_biomass_new))

attr(hake_biomass_new, "axis_name") <-
  "Pacific Hake spawning biomass (million t)"

if(check_index_changed(hake_biomass,
                       hake_biomass_new)){
  hake_biomass <- hake_biomass_new
  usethis::use_data(hake_biomass,
                    overwrite = TRUE)
  plot(hake_biomass, value = "val", style = "plain")
}
