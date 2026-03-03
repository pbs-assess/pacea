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

# 2026 Kelly sent me a .csv file, I never got the 2025 one into pacea

load_all()
library(dplyr)

# Change this when get updated data
assess_yr <- 2026    # for consistency with hake and herring, this is the year
# of the SOPO report, final year of data will be assess_yr - 1
raw_orig <-
  readr::read_csv("2026/2026_Feb df_anomalies_deepSoG_forRpacea_1996-2021baseline euphx3.csv") %>%
  dplyr::mutate_if(is.character, factor)

# rename the headings, should give error if any don't exist. Sticking with
# Kelly's classifications, just making lower case and with underscores for
# consistency. Can give full definitions in the help file.

# If re-order these at all (there should not be a reason, these are all alphabetical), then have to re-order zooplankton_axis_names below
zooplankton_sog_new <- select(raw_orig,
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

class(zooplankton_sog_new) <- c("pacea_zooplankton",
                                class(zooplankton_sog_new))

plot(zooplankton_sog_new)
tail(zooplankton_sog_new)
tail(zooplankton_sog)     # what's currently in pacea. Will know if we're updating
                      # data, so no need to check automatically like for oni etc.

# for 2026 update, clear that historical values to change, can see recent sample
#  numbers have increased (except for one that decreased).
# So, like hake, want to keep older versions, so save each year with it's
#  final_year appended.

# One time (before updating), then comment out: save original zooplankton_sog as zooplankton_sog_2024
# zooplankton_sog_2024 <- zooplankton_sog
# usethis::use_data(zooplankton_sog_2024,
#                   overwrite = TRUE)

# Now do the update automatically each year:

zooplankton_sog <- zooplankton_sog_new

assign(paste0("zooplankton_sog_", assess_yr),
       zooplankton_sog_new)

usethis::use_data(zooplankton_sog,
                  overwrite = TRUE)

create_data(paste0("zooplankton_sog_", assess_yr),
            get(paste0("zooplankton_sog_", assess_yr)))

plot(zooplankton_sog)

# Check that axis names are still consistent; if fail then manually change
#  zooplankton_axis_names definition below
expect_equal(names(zooplankton_sog_new),
             c("year",
               "number_samples",
               "volume_filtered",
               pull(zooplankton_sog_axis_names,
                    species_group_name)))

# If that fails then this will run, will have to manually look into and update:
if(any(names(zooplankton_sog_new) !=
         c("year",
           "number_samples",
           "volume_filtered",
           pull(zooplankton_sog_axis_names,
                species_group_name)))){

  # Axis names, based on 2024/2024_biomass_definitions.csv.
  #  Adapting methods for names used in ALPI.
  zooplankton_sog_axis_names_new <- tibble(
    species_group_name = names(zooplankton_sog_new)[-c(1, 2, 3)],
    axis_name = c(expression(paste(plain(Total) * " " * plain(biomass) * " " * plain(anomaly) * ", " * log[10] * " " * g * " " * m^-2)),
                expression(paste(plain(Gammarid) * " " * plain(amphipods) * " " * plain(anomaly) * ", " * log[10] * " " * g * " " * m^-2)),
                expression(paste(plain(Hyperiid) * " " * plain(amphipods) * " " * plain(anomaly) * ", " * log[10] * " " * g * " " * m^-2)),
                expression(paste(plain(Benthic) * " " * plain(larvae) * " " * plain(anomaly) * ", " * log[10] * " " * g * " " * m^-2)),
                expression(paste(plain(Calanoid) * " " * plain(copepods) * " " * plain(large) * " " * plain(anomaly) * ", " * log[10] * " " * g * " " * m^-2)),
                expression(paste(plain(Calanoid) * " " * plain(copepods) * " " * "(" * plain(medium) * ")" * " " * plain(anomaly) * ", " * log[10] * " " * g * " " * m^-2)),
                expression(paste(plain(Calanoid) * " " * plain(copepods) * " " * "(" * plain(small) * ")" * " " * plain(anomaly) * ", " * log[10] * " " * g * " " * m^-2)),
                expression(paste(plain(Cephalopoda) * " " * plain(anomaly) * ", " * log[10] * " " * g * " " * m^-2)),
                expression(paste(plain(Chaetognatha) * " " * plain(anomaly) * ", " * log[10] * " " * g * " " * m^-2)),
                expression(paste(plain(Cladocera) * " " * plain(anomaly) * ", " * log[10] * " " * g * " " * m^-2)),
                expression(paste(plain(Ctenophora) * " " * plain(anomaly) * ", " * log[10] * " " * g * " " * m^-2)),
                expression(paste(plain(Euphausiids) * " " * plain(anomaly) * ", " * log[10] * " " * g * " " * m^-2)),
                expression(paste(plain(Larval) * " " *  plain(fish) * " " * plain(anomaly) * ", " * log[10] * " " * g * " " * m^-2)),
                expression(paste(plain(Larvacea) * " " * plain(anomaly) * ", " * log[10] * " " * g * " " * m^-2)),
                expression(paste(plain(Medusae) * " " * plain(anomaly) * ", " * log[10] * " " * g * " " * m^-2)),
                expression(paste(plain(Mysids) * " " * plain(anomaly) * ", " * log[10] * " " * g * " " * m^-2)),
                expression(paste(plain(Natantia) * " " * plain(anomaly) * ", " * log[10] * " " * g * " " * m^-2)),
                expression(paste(plain(Non-calanoid) * " " *  plain(copeopods) * " " * plain(anomaly) * ", " * log[10] * " " * g * " " * m^-2)),
                expression(paste(plain(Ostracoda) * " " * plain(anomaly) * ", " * log[10] * " " * g * " " * m^-2)),
                expression(paste(plain(Other) * " " *  plain(taxa) * " " * plain(anomaly) * ", " * log[10] * " " * g * " " * m^-2)),
                expression(paste(plain(Pelagic) * " " *  plain(polychaeta) * " " * plain(anomaly) * ", " * log[10] * " " * g * " " * m^-2)),
                expression(paste(plain(Pteropods) * " " * plain(anomaly) * ", " * log[10] * " " * g * " " * m^-2)),
                expression(paste(plain(Repantia) * " " * plain(anomaly) * ", " * log[10] * " " * g * " " * m^-2)),
                expression(paste(plain(Scyphozoa) * " " * plain(anomaly) * ", " * log[10] * " " * g * " " * m^-2)),
                expression(paste(plain(Siphonophorae) * " " * plain(anomaly) * ", " * log[10] * " " * g * " " * m^-2))),
  in_perry_fig_s1 = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE,
                      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE,
                      TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE))

  zooplankton_sog_axis_names <- zooplankton_sog_axis_names_new
  usethis::use_data(zooplankton_sog_axis_names,
                    overwrite = TRUE)
}

