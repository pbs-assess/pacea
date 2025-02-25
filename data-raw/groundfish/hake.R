# Outputs from the hake stock assessment. Run code line-by-line and check plots.
# Change assess_yr each year, the rest is automated.
# hake_recruitment will become the latest assessment results, and
#  hake_recruitment_2023 retains the 2023 assessment results, this is then ongoing for
#  each year. So hake_recruitment_<assess_yr> = hake_recruitment.
#  See ?hake for details.

# Not re-run in 2024 since changing create_data_hake() -> create_data(), but
# should work.
load_all()
assess_yr <- 2025       # Year of the hake assessment; update each year

# The hake-2024/*.rds files are built automatically from (Andy or Chris Grandin)
#  running `pacea_save()` in the hake-assessment repo after having just built the document.
# pacea-save() automatically creates the hake-<assess_yr> directory here if it doesn't exist,
#  and puts the files into it, all named with assess_yr.

# One-time code used to save original data objects from 2023 assessments as
#  _2023. Before then adding _2024 and hake_recruitment etc. being the 2024
#  values (by default):
# hake_recruitment_2023 <- hake_recruitment
# hake_recruitment_over_2010_2023 <- hake_recruitment_over_2010
# hake_recruitment_over_R0_2023 <- hake_recruitment_over_R0
# hake_biomass_2023 <- hake_biomass

## usethis::use_data(hake_recruitment_2023,
##                   overwrite = TRUE)

## usethis::use_data(hake_recruitment_over_2010_2023,
##                   overwrite = TRUE)

## usethis::use_data(hake_recruitment_over_R0_2023,
##                   overwrite = TRUE)

## usethis::use_data(hake_biomass_2023,
##                   overwrite = TRUE)


# Recruitment
hake_dir <- paste0(here::here(),
                   "/data-raw/groundfish/hake-",
                   assess_yr,
                   "/")

hake_recruitment_new <- readRDS(paste0(hake_dir,
                                       "hake_recruitment_",
                                       assess_yr,
                                       ".rds"))

class(hake_recruitment_new) <- c("pacea_recruitment",
                                 class(hake_recruitment_new))

attr(hake_recruitment_new, "axis_name") <-
  "Pacific Hake recruitment (billions of age-0 fish)"

# Keep if statement so okay to run multiple times
check_index_changed(hake_recruitment,
                    hake_recruitment_new)

if(check_index_changed(hake_recruitment,
                       hake_recruitment_new)){

  hake_recruitment <- hake_recruitment_new

  assign(paste0("hake_recruitment_", assess_yr),
         hake_recruitment_new)

  usethis::use_data(hake_recruitment,
                    overwrite = TRUE)

  create_data(paste0("hake_recruitment_", assess_yr),
              get(paste0("hake_recruitment_", assess_yr)))

  plot(hake_recruitment)
}


# Recruitment scaled by 2010 value

hake_recruitment_over_2010_new <- readRDS(paste0(hake_dir,
                                                 "hake_recruitment_over_2010_",
                                                 assess_yr,
                                                 ".rds"))

class(hake_recruitment_over_2010_new) <- c("pacea_recruitment",
                                           class(hake_recruitment_over_2010_new))

attr(hake_recruitment_over_2010_new, "axis_name") <-
  "Age-0 Pacific Hake recruitment relative to that in 2010"

# Keep if statement so okay to run multiple times
check_index_changed(hake_recruitment_over_2010,
                    hake_recruitment_over_2010_new)

if(check_index_changed(hake_recruitment_over_2010,
                       hake_recruitment_over_2010_new)){
  hake_recruitment_over_2010 <- hake_recruitment_over_2010_new

  assign(paste0("hake_recruitment_over_2010_", assess_yr),
         hake_recruitment_over_2010_new)

  usethis::use_data(hake_recruitment_over_2010,
                    overwrite = TRUE)

  create_data(paste0("hake_recruitment_over_2010_", assess_yr),
              get(paste0("hake_recruitment_over_2010_", assess_yr)))

  plot(hake_recruitment_over_2010)
}

# Recruitment scaled by R0

hake_recruitment_over_R0_new <- readRDS(paste0(hake_dir,
                                               "hake_recruitment_over_R0_",
                                               assess_yr,
                                               ".rds"))

class(hake_recruitment_over_R0_new) <- c("pacea_recruitment",
                                         class(hake_recruitment_over_R0_new))

attr(hake_recruitment_over_R0_new, "axis_name") <-
  "Age-0 Pacific Hake recruitment relative to unfished equilibrium recruitment"

# Keep if statement so okay to run multiple times
check_index_changed(hake_recruitment_over_R0,
                    hake_recruitment_over_R0_new)

if(check_index_changed(hake_recruitment_over_R0,
                       hake_recruitment_over_R0_new)){
  hake_recruitment_over_R0 <- hake_recruitment_over_R0_new

  assign(paste0("hake_recruitment_over_R0_", assess_yr),
         hake_recruitment_over_R0_new)

  usethis::use_data(hake_recruitment_over_R0,
                    overwrite = TRUE)

  create_data(paste0("hake_recruitment_over_R0_", assess_yr),
              get(paste0("hake_recruitment_over_R0_", assess_yr)))

  plot(hake_recruitment_over_R0)
}

# Recruitment deviations

hake_recruitment_deviations_new <- readRDS(paste0(hake_dir,
                                                  "hake_recruitment_deviations_",
                                                  assess_yr,
                                                  ".rds"))

hake_recruitment_deviations_new <- filter(hake_recruitment_deviations_new,
                                          year > 1965)   # 1966 is first year of model

class(hake_recruitment_deviations_new) <- c("pacea_recruitment",
                                            class(hake_recruitment_deviations_new))

attr(hake_recruitment_deviations_new, "axis_name") <-
  "Pacific Hake recruitment deviations"

# Keep if statement so okay to run multiple times
check_index_changed(hake_recruitment_deviations,
                    hake_recruitment_deviations_new)

if(check_index_changed(hake_recruitment_deviations,
                       hake_recruitment_deviations_new)){
  hake_recruitment_deviations <- hake_recruitment_deviations_new

  assign(paste0("hake_recruitment_deviations_", assess_yr),
         hake_recruitment_deviations_new)

  usethis::use_data(hake_recruitment_deviations,
                    overwrite = TRUE)

  create_data(paste0("hake_recruitment_deviations_", assess_yr),
              get(paste0("hake_recruitment_deviations_", assess_yr)))

  plot(hake_recruitment_deviations)
}

# Spawning biomass

hake_biomass_new <- readRDS(paste0(hake_dir,
                                   "hake_biomass_",
                                   assess_yr,
                                   ".rds"))

class(hake_biomass_new) <- c("pacea_biomass",
                             class(hake_biomass_new))

attr(hake_biomass_new, "axis_name") <-
  "Pacific Hake spawning biomass (million t)"

# Keep if statement so okay to run multiple times
check_index_changed(hake_biomass,
                    hake_biomass_new)

if(check_index_changed(hake_biomass,
                       hake_biomass_new)){
  hake_biomass <- hake_biomass_new

  assign(paste0("hake_biomass_", assess_yr),
         hake_biomass_new)

  usethis::use_data(hake_biomass,
                    overwrite = TRUE)

  create_data(paste0("hake_biomass_", assess_yr),
              get(paste0("hake_biomass_", assess_yr)))

  plot(hake_biomass)
}

# Total biomass of age-1 hake

hake_total_biomass_age_1_new <- readRDS(paste0(hake_dir,
                                               "hake_total_biomass_age_1_",
                                               assess_yr,
                                               ".rds"))

if(!is.na(hake_total_biomass_age_1_new[1, "low"])){
  stop("Can remove the next lines it seems as now have uncertainties")
}

# Removing these makes plot(hake_total_biomass_age_1) automatically work
hake_total_biomass_age_1_new <- dplyr::select(hake_total_biomass_age_1_new,
                                              -c("low",
                                                 "high"))

class(hake_total_biomass_age_1_new) <- c("pacea_biomass",
                                         class(hake_total_biomass_age_1_new))

attr(hake_total_biomass_age_1_new, "axis_name") <-
  "Pacific Hake total biomass of age-1 fish (thousand t)"

# Keep if statement so okay to run multiple times
check_index_changed(hake_total_biomass_age_1,
                    hake_total_biomass_age_1_new)

if(check_index_changed(hake_total_biomass_age_1,
                       hake_total_biomass_age_1_new)){
  hake_total_biomass_age_1 <- hake_total_biomass_age_1_new

  assign(paste0("hake_total_biomass_age_1_", assess_yr),
         hake_total_biomass_age_1_new)

  usethis::use_data(hake_total_biomass_age_1,
                    overwrite = TRUE)

  create_data(paste0("hake_total_biomass_age_1_", assess_yr),
              get(paste0("hake_total_biomass_age_1_", assess_yr)))

  plot(hake_total_biomass_age_1)
}

# Biomass at age of hake (all ages, so a tibble not an index)

hake_total_biomass_at_age_new <- readRDS(paste0(hake_dir,
                                                  "hake_total_biomass_at_age_",
                                                  assess_yr,
                                                  ".rds"))

# No real appropriate class yet, so don't assign one, but worth doing to make a
# nice plot; no time right now.
# class(hake_total_biomass_at_age_new) <- c("pacea_biomass",
#                                           class(hake_total_biomass_at_age_new))

attr(hake_total_biomass_at_age_new, "axis_name") <-
  "Pacific Hake total biomass at age (thousand t)"

# Keep if statement so okay to run multiple times
check_index_changed(hake_total_biomass_at_age,
                    hake_total_biomass_at_age_new)

if(check_index_changed(hake_total_biomass_at_age,
                       hake_total_biomass_at_age_new)){
  hake_total_biomass_at_age <- hake_total_biomass_at_age_new

  assign(paste0("hake_total_biomass_at_age_", assess_yr),
         hake_total_biomass_at_age_new)

  usethis::use_data(hake_total_biomass_at_age,
                    overwrite = TRUE)

  create_data(paste0("hake_total_biomass_at_age_", assess_yr),
              get(paste0("hake_total_biomass_at_age_", assess_yr)))

  plot(hake_total_biomass_at_age)
}
