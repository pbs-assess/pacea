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

recruit <- tibble()
for(i in 1:length(regions_all)){
  recruit_region <- create_herring_object(assess_yr,
                                          regions_all[i])
  recruit <- rbind(recruit,
                   recruit_region)
}

recruit <- dplyr::mutate(recruit,
                         region = as.factor(region))

class(recruit) <- c("pacea_recruitment_herring",
                    class(recruit))

attr(recruit, "axis_name") <-
  "Pacific Herring recruitment (billions of age-2 fish)"

# works, plots all five
plot.pacea_recruitment_herring(recruit)

# working:
plot.pacea_recruitment_herring(recruit,
                               region = "HG")

# works now it's exported:
plot(recruit,
     region = "HG")










if(check_index_changed(hake_recruitment,
                       hake_recruitment_new)){
  hake_recruitment <- hake_recruitment_new
  usethis::use_data(hake_recruitment,
                    overwrite = TRUE)
  plot(hake_recruitment)
}




NEED somewhere maybe:
years <- 1953:2023
expect_equal(as.numeric(row.names(t(raw_recruit))),    # names() does not work since a matrix
             years)
