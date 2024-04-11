# Incorporating Kelly Young's zooplankton anomaly calculations into pacea.
#  Quite a few different species groups, so just save the whole thing as a
#  tibble, give it a pacea_zooplankton class, and then have a plotting function
#  where you specify one of the species groups, and make that into a pacea_index
#  class so we can just use plot.pacea_index().

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

raw_rename <- select(raw_orig,
                     year,
                     num_samples = n.samples,
                     vol_filtered = vol.filt,
                     total_biomass = TotalBiomass,
                     amphipod_gammarid = AmphiGam,
                     amphipod_hyperiid = AmphiHyp,
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



stop()
# might need some of this:
colnames(oni_new)<-c("month",
                     "year",
                     "value",
                     "anomaly")

class(oni_new) <- c("pacea_index",
                    class(oni_new))

attr(oni_new, "axis_name") <- "Oceanic Niño Index"

check_index_changed(oni, oni_new)

tail(oni)
tail(oni_new)

if(check_index_changed(oni, oni_new)){
  # Check previous values haven't changed, but for oni we expect the last two
  #  months of anomaly to get updated (and maybe one month of value)
  par(mfrow=c(2,1))
  plot(oni, main = "Currently in pacea")
  plot(oni_new, main = "Updated")

  expect_equal(oni[1:(nrow(oni) - 2), ],
               oni_new[1:(nrow(oni) - 2), ],
               tolerance = 0.02) # See note at top if this fails

  oni <- oni_new
  usethis::use_data(oni,
                    overwrite = TRUE)
}
