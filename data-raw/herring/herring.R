# Outputs from the Pacific Herring stock assessments. Run code line-by-line and check plots.
#  See ?herring for details. Combining parts of Matt's sample code, hake.R,  harbour-seals.R,
#  and pacea-save() from hake-assessment package.

# From DFO (2024) STOCK STATUS UPDATE WITH APPLICATION OF MANAGEMENT PROCEDURES
# FOR PACIFIC HERRING (CLUPEA PALLASII) IN BRITISH COLUMBIA: STATUS IN 2023 AND
# FORECAST FOR 2024. Science Response.

# Earlier longer email from Matt, shortened:

## These are the five *.RData files (one for each SAR) with iSCAM model output for
## the assessment that we used this year and for the last few years -- it’s not the
## new one that was reviewed in June 2023  which we will use next year.

## There are five major herring stock assessment regions (SARs): Haida Gwaii (HG),
## Prince Rupert District (PRD), Central Coast (CC), Strait of Georgia (SoG), and
## West Coast of Vancouver Island (WCVI). (There are also two minor SARs but we
## don’t do a full stock assessment for these ones.) I just added these to the
## names of the *.RData files (usually they’re just called “aaa_gfiscam.RData” and
## stored in their respective folders). For herring we use median and
## 90% CI. Note that spawning biomass goes up to 2024, but 2024 is a projection so you might want to omit it.
## There’s a LOT of other stuff in there.. just ask if you want to know more.

# Order (e.g. Figure 8) is HG, PRD, CC, SOG, WCVI.

load_all()
assess_yr <- 2023       # Year of the assessment (status in that year)
regions_all <- c("HG", "PRD", "CC", "SOG", "WCVI")

# call create_herring_object() once for each region, and make recruitment and
#  spawning biomass each into one big tibble, that then get modified after

herring_recruitment_new <- tibble()
herring_spawning_biomass_new <- tibble()

for(i in 1:length(regions_all)){
  results_region <- create_herring_object(assess_yr,
                                          regions_all[i])

  herring_recruitment_new <- rbind(herring_recruitment_new,
                                   results_region[["recruit"]])

  herring_spawning_biomass_new <- rbind(herring_spawning_biomass_new,
                                   results_region[["spawning_biomass"]])
}

# Recruitment
herring_recruitment_new <- dplyr::mutate(herring_recruitment_new,
                                         region = as.factor(region))

class(herring_recruitment_new) <- c("pacea_recruitment_herring",
                                    class(herring_recruitment_new))

attr(herring_recruitment_new, "axis_name") <-
  "Pacific Herring recruitment (billions of age-2 fish)"

herring_recruitment_new
herring_recruitment_new %>% tail()

# plots all five regions:
plot(herring_recruitment_new)         # Calls plot.pacea_recruitment_herring(herring_recruitment_new)

# one region:
plot(herring_recruitment_new,
     region = "HG")


# Will need to include something like this when update with 2024 assessment
# results. This is taken from hake

# For 2023, first year of doing this, forcing the if statement to work, should
# then work for future years.

check_index_changed(herring_recruitment,
                    herring_recruitment_new)

if(check_index_changed(herring_recruitment,
                       herring_recruitment_new)){

  plot(herring_recruitment)  # Hard to do in one big figure because
                             # par(mfrow=c(5, 1)) is in plotting function
  windows()
  plot(herring_recruitment_new)

  herring_recruitment <- herring_recruitment_new

  assign(paste0("herring_recruitment_", assess_yr),
         herring_recruitment_new)

  usethis::use_data(herring_recruitment,
                    overwrite = TRUE)

  create_data(paste0("herring_recruitment_", assess_yr),
              get(paste0("herring_recruitment_", assess_yr)))
}

# Check values with Tables 14-18:
dplyr::filter(herring_recruitment,
              year >= 2014) %>%
  a()


# Spawning biomass (copying recruitment code from above, query-replace, then
# going through TODO delete me when finished going through).
# TODO make sure to remove the final year
herring_spawning_biomass_new <- dplyr::mutate(herring_spawning_biomass_new,
                                              region = as.factor(region))

# The final year, which should be assess_yr + 1, is a projection in the absence
# of fishing, though TODO am clarifying with Matt exactly when in the year
# biomass is estimated (Fig 13d for SOG gives known 2023 catch, obviously no
# 2024 catch).

# Check there's one value for assess_yr for each region
dplyr::filter(herring_spawning_biomass_new,
              year == assess_yr + 1)
# So only keep years before the projection year
herring_spawning_biomass_new <- dplyr::filter(herring_spawning_biomass_new,
                                              year < assess_yr + 1)

class(herring_spawning_biomass_new) <- c("pacea_biomass_herring",
                                    class(herring_spawning_biomass_new))

attr(herring_spawning_biomass_new, "axis_name") <-
  "Pacific Herring spawning biomass (thousand tons)"

herring_spawning_biomass_new
herring_spawning_biomass_new %>% tail()    # Nothing for assess_yr

# plot all five regions:
plot(herring_spawning_biomass_new)         # Calls TODO plot.pacea_spawning_biomass_herring(herring_spawning_biomass_new)

# one region:
plot(herring_spawning_biomass_new,
     region = "HG")

# Will need to include something like this when update with 2024 assessment
# results. This is taken from hake

# For 2023, first year of doing this, forcing the if statement to work, should
# then work for future years.

check_index_changed(herring_spawning_biomass,
                    herring_spawning_biomass_new)

if(check_index_changed(herring_spawning_biomass,
                       herring_spawning_biomass_new)){

  plot(herring_spawning_biomass)  # Hard to do in one big figure because
                                  # par(mfrow=c(5, 1)) is in plotting function
  windows()
  plot(herring_spawning_biomass_new)

  herring_spawning_biomass <- herring_spawning_biomass_new

  assign(paste0("herring_spawning_biomass_", assess_yr),
         herring_spawning_biomass_new)

  usethis::use_data(herring_spawning_biomass,
                    overwrite = TRUE)

  create_data(paste0("herring_spawning_biomass_", assess_yr),
              get(paste0("herring_spawning_biomass_", assess_yr)))
}

# Check values with Tables 19-23:
dplyr::filter(herring_spawning_biomass,
              year >= 2014) %>%
  a()
# Looks good
