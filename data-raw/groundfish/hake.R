# Outputs from the hake stock assessment. Run code line-by-line and check plots.
#  See ?hake for details.

load_all()

# TODO - update when finalised.
# Get the .rda files from Andy running
#  hake-assessment/sandbox/andy/pacea-save/pacea-save.R
#  after 2023 hake assessment, gets file directly from that directory.
#  For future assessments will maybe convert that to
#  a function. Check with Andy or Chris Grandin.

# One-time code to save original data objects from 2023 assessments as
# _2023. Before then adding _2024 and hake_recruitment etc. being the 2024
# values (by default).
hake_recruitment_2023 <- hake_recruitment
hake_recruitment_over_2010_2023 <- hake_recruitment_over_2010
hake_recruitment_over_R0_2023 <- hake_recruitment_over_R0
hake_biomass_2023 <- hake_biomass

usethis::use_data(hake_recruitment_2023,
                  overwrite = TRUE)

usethis::use_data(hake_recruitment_over_2010_2023,
                  overwrite = TRUE)

usethis::use_data(hake_recruitment_over_R0_2023,
                  overwrite = TRUE)

usethis::use_data(hake_biomass_2023,
                  overwrite = TRUE)


# Recruitment

# Comment this out if you have copied the .rda files to the local directory
#  (you need to first run the code in the hake directory shown below), and
#  uncomment the second line
hake_dir <- paste0(here::here(), "/../hake-assessment/sandbox/andy/pacea-save/")
# hake_dir <- getwd()

load(paste0(hake_dir, "hake_recruitment_new.rda"))

class(hake_recruitment_new) <- c("pacea_recruitment",
                                 class(hake_recruitment_new))

attr(hake_recruitment_new, "axis_name") <-
  "Pacific Hake recruitment (billions of age-0 fish)"

if(check_index_changed(hake_recruitment,
                       hake_recruitment_new)){
  hake_recruitment <- hake_recruitment_new
  usethis::use_data(hake_recruitment,
                    overwrite = TRUE)
  plot(hake_recruitment)
}

# Spawning biomass

load(paste0(hake_dir, "hake_biomass_new.rda"))

class(hake_biomass_new) <- c("pacea_biomass",
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

# Recruitment scaled by 2010 value

load(paste0(hake_dir, "hake_recruitment_over_2010_new.rda"))

class(hake_recruitment_over_2010_new) <- c("pacea_recruitment",
                                           class(hake_recruitment_over_2010_new))

attr(hake_recruitment_over_2010_new, "axis_name") <-
  "Age-0 Pacific Hake recruitment relative to that in 2010"

if(check_index_changed(hake_recruitment_over_2010,
                       hake_recruitment_over_2010_new)){
  hake_recruitment_over_2010 <- hake_recruitment_over_2010_new
  usethis::use_data(hake_recruitment_over_2010,
                    overwrite = TRUE)
  plot(hake_recruitment_over_2010)
  }
}

# Recruitment scaled by R0:

load(paste0(hake_dir, "hake_recruitment_over_R0_new.rda"))

class(hake_recruitment_over_R0_new) <- c("pacea_recruitment",
                                           class(hake_recruitment_over_R0_new))

attr(hake_recruitment_over_R0_new, "axis_name") <-
  "Age-0 Pacific Hake recruitment relative to unfished equilibrium recruitment"

if(check_index_changed(hake_recruitment_over_R0,
                       hake_recruitment_over_R0_new)){
  hake_recruitment_over_R0 <- hake_recruitment_over_R0_new
  usethis::use_data(hake_recruitment_over_R0,
                    overwrite = TRUE)
  plot(hake_recruitment_over_R0)
  }
}
