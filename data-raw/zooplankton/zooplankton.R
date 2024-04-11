# Incorporating Kelly Young's zooplankton anomaly calculations into pacea.
#  Quite a few different species groups, so just save the whole thing as a
#  tibble, give it a pacea_zooplankton class, and then have a plotting function
#  where you specify one of the species groups, and make that into a pacea_index
#  class so we can just use plot.pacea_index().
# Also creates zooplankton_axis_names to automatically give an axis name.

# 2024_Feb-zoop df_anomalies_deepSoG_forRpacea_1996-2021baseline.xlsx - from
# Kelly on 4/3/24.
# Andy exporting first sheet as ....csv, and second sheet as
# 2024_biomass_definitions.csv (after deleting column D that just had
# year. n.samples, and vol.filt repeated).

load_all()
library(dplyr)

raw_orig <-
  readr::read_csv("2024/2024_Feb-zoop df_anomalies_deepSoG_forRpacea_1996-2021baseline.csv") %>%
  dplyr::mutate_if(is.character, factor)

# rename the headings, should give error if any don't exist. Sticking with
# Kelly's classifications, just making lower case and with underscores for
# consistency. Can give full definitions in the help file.

# If re-order these at all (there should not be a reason, these are all alphabetical), then have to re-order zooplankton_axis_names below
zooplankton_new <- select(raw_orig,
                          year,
                          number_samples = n.samples,
                          volume_filtered = vol.filt,
                          total_biomass = TotalBiomass,
                          amphipods_gammarid = AmphiGam,
                          amphipods_hyperiid = AmphiHyp,
                          benthic_larvae = BenthicLarv,
                          calanoid_copepods_large = CalCops.larg,
                          calanoid_copepods_medium = CalCops.med,
                          calanoid_copepods_small = CalCops.smal,
                          cephalopoda = Cephalopoda,
                          chaetognatha = Chaetognatha,
                          cladocera = Cladocera,
                          ctenophora = Ctenophora,
                          euphausiids = Euphs,
                          fish = Fish,
                          larvacea = Larvacea,
                          medusae = Medusae,
                          mysids = Mysids,
                          natantia = Natantia,
                          non_calanoid_copeopods = NonCalCops,
                          ostracoda = Ostracoda,
                          other = Other,
                          pelagic_polychaeta = PolychaetPelagic,
                          pteropods = Pteropods,
                          repantia = Repantia,
                          scyphozoa = Scyphozoa,
                          siphonophorae = Siphonophorae)

class(zooplankton_new) <- c("pacea_zooplankton",
                            class(zooplankton_new))

plot(zooplankton_new)
tail(zooplankton_new)
tail(zooplankton)     # what's currently in pacea. Will know if we're updating
                      # data, so no need to check automatically like for oni etc.

# for 2025 update, do a check to see whether historic values have changed, and
#  do a plot to compare them if so. See oni example in
#  data-raw/coastwide-indices/coastwide-indices.R

# Check that axis names are still consistent; if fail then manually change
#  zooplankton_axis_names definition below
expect_equal(names(zooplankton_new),
             c("year",
               "number_samples",
               "volume_filtered",
               pull(zooplankton_axis_names,
                    name)))

# Axis names, based on 2024/2024_biomass_definitions.csv.
zooplankton_axis_names_new <- tibble(
  species_group_name = names(zooplankton_new)[-c(1, 2, 3)],
  axis_name = c("Total biomass",
                "Gammarid amphipods",
                "Hyperiid amphipods",
                "Benthic larvae",
                "Calanoid copepods (large)",
                "Calanoid copepods (medium)",
                "Calanoid copepods (small)",
                "Cephalopoda",
                "Chaetognatha",
                "Cladocera",
                "Ctenophora",
                "Euphausiids",
                "Larval fish",
                "Larvacea",
                "Medusae",
                "Mysids",
                "Natantia",
                "Non-calanoid copeopods",
                "Ostracoda",
                "Other taxa",
                "Pelagic polychaeta",
                "Pteropods",
                "Repantia",
                "Scyphozoa",
                "Siphonophorae"))

zooplankton <- zooplankton_new
usethis::use_data(zooplankton,
                  overwrite = TRUE)

zooplankton_axis_names <- zooplankton_axis_names_new
usethis::use_data(zooplankton_axis_names,
                  overwrite = TRUE)
