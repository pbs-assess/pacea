# Updating the ALPI values with the values from Chris Rooper, that he calculated
# up to 2022 (original pacea values only go to 2015). So use those, check the
# historic values agree with original, and update pacea now. Then need to extend
# by automating the code.

load_all()
library(dplyr)

# Original from chris is alpi-values-from-chris-orig.txt, I then added commas and
# changed headings to give alpi-values-from-chris-orig.txt, so in same format as
# alpi.txt below.

# Can't use read_table as , separated without spaces
alpi_new_raw <- read.csv("alpi-surry/alpi-values-from-chris.txt") %>%
  as_tibble() %>%
  dplyr::rename("year" = "YEAR",
                "anomaly" = "ALEUTIAN.LOW.PRESSURE.INDEX..ALPI.")

# stopifnot(alpi_new_raw[1, 1:2] == c(1900, 1.3)) # Check still starts in 1950

alpi_new_raw   # starts in 1966
alpi       # starts in 1900

# Check the overlapping years agree
overlap_years <- intersect(alpi_new_raw$year,
                           alpi$year)

expect_equal(filter(alpi_new_raw,
                    year %in% overlap_years),
             filter(alpi,
                    year %in% overlap_years))

# Fails, as alpi_new_raw gives more decimal places, but alpi only had two. Given we
# want to keep the historical alpi values (alpi_new_raw starts in 1966)), we will
# use two decimal places.
filter(alpi_new_raw, year == 1967)   # this only prints 3 sig figs though
filter(alpi_new_raw, year == 1967)$anomaly    # -0.165704
filter(alpi, year == 1967)$anomaly            # -0.17

expect_equal(filter(alpi_new_raw,
                    year %in% overlap_years)$year,
             filter(alpi,
                    year %in% overlap_years)$year)

# That passes, years are the same. Try rounding the anomalies:

expect_equal(round(filter(alpi_new_raw,
                          year %in% overlap_years)$anomaly,
                   2),
             filter(alpi,
                    year %in% overlap_years)$anomaly)
# Fails, but it seems only for the 9th value, corresponding to 1974:
overlap_years[9]
alpi_new_raw$anomaly[9]                # 1.045096
filter(alpi, year == 1974)$anomaly     # 1.04

# So it depends when the rounding was done (maybe one calculation rounded once
# then again later), becuase:
round(1.045, 2)    # 1.04, round down
round(1.045096, 2) # 1.05

# So it's just a minor rounding thing and maybe even due to very minor updates in
#  the older data. So keep the existing value and be happy that others all agree,
#  just checking others are good:
expect_equal(round(filter(alpi_new_raw,
                          year %in% overlap_years[-9])$anomaly,
                   2),
             filter(alpi,
                    year %in% overlap_years[-9])$anomaly)
# That passes. So all good except for the 0.01 difference for 1974.

# So keep with originaly alpi, and then add on the new year (rounded) from
# Chris:
tail(alpi)

alpi_new_raw_late_years <- filter(alpi_new_raw,
                                  year > 2015) %>%
  mutate(anomaly = round(anomaly,
                         2))
alpi_new_raw_late_years
alpi_new_raw_late_years$anomaly     # All to 2 dp's

alpi_new <- rbind(alpi,
                  alpi_new_raw_late_years)

# Class and attribute gets retained, so not needed
# class(alpi_new) <- c("pacea_index",
#                      class(alpi_new))
# attr(alpi_new, "axis_name") <- expression(paste(plain(Aleutian) * " " * plain(Low) * " " * plain(Pressure) * " " * plain(Index) * ", " * 10^6 * km^2))
# Must be an easier way of doing that, but it works

# So check the pre-2016 values are the same, they should be (so this is FALSE):
check_index_changed(alpi,
                    filter(alpi_new, year < 2016),
                    alpi = TRUE)

# Attributes are the same since copied over; if they change would have to override the next if().

if(check_index_changed(alpi,
                       filter(alpi_new),
                       alpi = TRUE)){   # Will have changed since have new
                                        # years the first time this is run

  par(mfrow=c(2,1))
  x_lim = c(lubridate::dmy(01011899),
            lubridate::dmy(01012023))
  plot(alpi, main = "Currently in pacea", xlim = x_lim,
       lwd = 5, xaxs= "i")
  plot(alpi_new, main = "Updated", xlim = x_lim,
       lwd = 5, xaxs = "i")
  # Saved as alpi-update.png, to send with email

  alpi <- alpi_new
  usethis::use_data(alpi,
                    overwrite = TRUE)
}

# Then also rerun the pacea_indices code at the bottom of
# coastwide-indices.R. Running it here (as it was directly copied over) then
# deleting so it's not in two places.
