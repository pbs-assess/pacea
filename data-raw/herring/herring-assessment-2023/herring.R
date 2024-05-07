# Outputs from the Pacific Herring stock assessments. Run code line-by-line and check plots.
#  See ?herring for details. Likely using some of hake and harbour-seals.

# From DFO (2024) STOCK STATUS UPDATE WITH APPLICATION OF MANAGEMENT PROCEDURES
# FOR PACIFIC HERRING (CLUPEA PALLASII) IN BRITISH COLUMBIA: STATUS IN 2023 AND
# FORECAST FOR 2024. Science Response.

load_all()

# Order (Figure 8) is HG, PRD, CC, SOG, WCVI. Presumably consistent.

# Sample code from Matt:

# This should get you going, for example HG:

load(file = "HG_aaa_gfiscam.RData")
# Age-2 recruitment (should match Table 14 in the PDF)
model$mcmccalcs$recr.quants
# Spawning biomass (Table 19)
model$mcmccalcs$sbt.quants
row.names(model$mcmccalcs$sbt.quants)


Andy doing:

years <- 1953:2023

raw_recruit <- model$mcmccalcs$recr.quants

expect_equal(row.names(raw_recruit),
             c("5%", "50%", "95%", "MPD"))
expect_equal(as.numeric(row.names(t(raw_recruit))),    # names() does not work since a matrix
             years)


recruit <- model$mcmccalcs$recr.quants %>%
  t() %>%
  as_tibble() %>%
  cbind(year = row.names(t(raw_recruit)))
recruit


better:




Earlier longer email

These are the five *.RData files (one for each SAR) with iSCAM model output for the assessment that we used this year and for the last few years -- it’s not the new one that was reviewed in June which we will use next year. Is this what you want? If not let me know. I’ll explain them a bit below, not sure if it’s going to be too much info, or not enough. I also attached our current SR (in progress, don’t cite or circulate) if you want more info.

There are five major herring stock assessment regions (SARs): Haida Gwaii (HG), Prince Rupert District (PRD), Central Coast (CC), Strait of Georgia (SoG), and West Coast of Vancouver Island (WCVI). (There are also two minor SARs but we don’t do a full stock assessment for these ones.) I just added these to the names of the *.RData files (usually they’re just called “aaa_gfiscam.RData” and stored in their respective folders). For herring we use median and 90% CI. Note that spawning biomass goes up to 2024, but 2024 is a projection so you might want to omit it.

..

There’s a LOT of other stuff in there.. just ask if you want to know more.
matt









stop()
# Get the .rda files from Andy running
#  hake-assessment/sandbox/andy/pacea-save/pacea-save.R
#  after 2023 hake assessment, gets file directly from that directory.
#  For future assessments will maybe convert that to
#  a function. Check with Andy or Chris Grandin.

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
