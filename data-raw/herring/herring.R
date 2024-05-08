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


# call create_herring_object() once for each region, and make into one big object

herring_recruitment_new <- tibble()
for(i in 1:length(regions_all)){
  recruit_region <- create_herring_object(assess_yr,
                                          regions_all[i])
  herring_recruitment_new <- rbind(herring_recruitment_new,
                                   recruit_region)
}

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

  plot(herring_recruitment)
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
