# Adapting from Andrea Hilborn's
#  https://github.com/IOS-OSD-DPG/Pacific_SST_Monitoring/blob/main/scripts/POI_latlon.R
# Metadata of buoys, location, names etc., that shouldn't really need changing,
#  but can be updated here if needed.

# 13/8/26 - Andy updating the order (at the end). Andrea's original is in
# numerical order of wmo_id, but when plotting I want to follow her
# geographically arranged ones in
# https://ios-osd-dpg.github.io/Pacific_SST_Monitoring_Site/#buoy-sst-and-sst-anomalies
# They are not all plotted, so I will manually insert the missing ones.

library(dplyr)
library(forcats)

buoy_metadata <- tibble::tibble(wmo_id = c("46004", "46036", "46131", "46132",
                                           "46134", "46145", "46146", "46147",
                                           "46181", "46183", "46184", "46185",
                                           "46204", "46205", "46206", "46207",
                                           "46208", "46303", "46304"),
                                name = c("Middle NOMAD",
                                         "South NOMAD",
                                         "Sentry Shoal",
                                         "South Brooks",
                                         "ECOBUOY_1",
                                         "Central Dixon Entrance",
                                         "Halibut Bank",
                                         "South Moresby",
                                         "Nanakwa Shoal",
                                         "North Hecate Strait",
                                         "North NOMAD",
                                         "South Hecate Strait",
                                         "West Sea Otter",
                                         "West Dixon Entrance",
                                         "La Perouse Bank",
                                         "East Dellwood Knolls",
                                         "West Moresby",
                                         "S. Georgia Strait",
                                         "Entrance English Bay"),
                                type = c("NOMAD",
                                         "NOMAD",
                                         "3 metre discus",
                                         "3 metre discus",
                                         "3 metre discus",
                                         "3 metre discus",
                                         "3 metre discus",
                                         "3 metre discus",
                                         "3 metre discus",
                                         "3 metre discus",
                                         "NOMAD",
                                         "3 metre discus",
                                         "3 metre discus",
                                         "3 metre discus",
                                         "3 metre discus",
                                         "3 metre discus",
                                         "3 metre discus",
                                          NA,
                                         NA),
                                latitude = c(50.983, 48.35, 49.9067, 49.73,
                                             48.656, 54.383, 49.34, 51.83,
                                             53.83, 53.617, 53.9, 52.407,
                                             51.3683, 54.167, 48.835, 50.875,
                                              52.5, 49.025, 49.30167),
                                longitude = c(-135.783, -133.93, -124.985, -127.9167,
                                              -123.483, -132.427, -123.727, -131.217,
                                              -128.8317, -131.105, -138.867, -129.783,
                                              -128.75, -134.33, -125.9983, -129.917,
                                              -132.7, -123.43, -123.357),
                                water_depth_m = c(3600, 3500, 18, 2040,
                                                  65, 257, 43, 2000,
                                                  22, 60, 3200, 228,
                                                  222, 2675, 73, 2215, 2950,
                                                  NA,  NA),
#                                Can work out from data if needed (see vignette), plus didn't all agree:
#                                start_date = c("1989-05-01", "1988-04-01", "1992-10-13",
#                                                "1993-10-05", "1999-01-01", "1991-04-11",
#                                                "1992-03-16", "1992-05-11", "1990-05-22",
#                                                "1991-04-09", "1987-09-20", "1990-06-08",
#                                                "1989-09-07", "1988-11-22", "1988-11-22",
#                                                "1989-10-18", "1991-07-01",
#                                                NA, NA),
                                col_key = c("#0000FF", "#FF0000", "#00FF00", "#000033",
                                            "#000033", "#FF00B6", "#005300", "#FFD300",
                                            "#009FFF", "#9A4D42", "#00FFBE", "#783FC1",
                                            "#1F9698", "#FFACFD", "#FE8F42", "#DD00FF", "#02AD24",
                                            "#C8FF00", "#886C00")) %>%
  mutate(stn_id = paste0("C", wmo_id),
         name_key = paste(stn_id,
                          name)) %>%
  mutate_if(is.character, as.factor)


# New order is based somewhat on Andrea's plots, with some changes. Basically
# going in horizontal bands from west to east, then moving south for each
# 'band', ending with Strait of Georgia. Plus adding in ones Andrea did not show.
new_order <- c("C46184",
               "C46205",
               "C46145",
               "C46183",
               "C46181",
               "C46208",
               "C46147",
               "C46185",
               "C46004",
               "C46207",
               "C46204",
               "C46036",
               "C46132",
               "C46206",
               "C46131",
               "C46146",
               "C46304",
               "C46303",
               "C46134"
               )

# Ones in buoy_metadata that are not listed there. Then manually whittle the
# above vector down so this becomes empty:
testthat::expect_equal(length(setdiff(buoy_metadata$stn_id,
                                      new_order)),
                       0)

buoy_metadata_new_order <-
  mutate(buoy_metadata,
         stn_id = fct_relevel(stn_id,
                              new_order)) %>%
  arrange(stn_id)

# Now put levels for all factors (all columns) into the order they appear
# buoy_metadata, I think needed because then plotting will always be in the same
# order regardless of what is used to label the buoy s(e.g. stn_id or name or
# name_key).

# Identify which columns are factors
factor_cols <- names(buoy_metadata_new_order)[sapply(buoy_metadata_new_order,
                                                     is.factor)]

# For each factor column, set levels to the order in the data
for (col in factor_cols){
  buoy_metadata_new_order[[col]] <- factor(buoy_metadata_new_order[[col]],
                                 levels = unique(buoy_metadata_new_order[[col]]))
}

buoy_metadata_new_order

buoy_metadata <- buoy_metadata_new_order

# Buoy comments:
# ECOBUOY_1: This buoy has been specially modified to serve as a platform for
# additional sensors, including solar radiation, salinity, temperature and
# chlorophyll fluorescence. This buoy is located in Saanich Inlet, near the
# Institute of Ocean Sciences and serves as a temporary "test bed" for new
# sensors and telemetry tests. This is NOT a permanent monitoring station.

# Halibut Bank: This buoy has been specially modified to serve as a platform for
# additional sensors, including solar radiation, salinity, temperature and
# chlorophyll fluorescence.

# Nanakwa Shoal: This buoy is located in Douglas Channel, near Kitimat.

usethis::use_data(buoy_metadata,
                  overwrite = TRUE)
