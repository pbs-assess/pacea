# Obtaining the oceanographic basin indices and compile them into pacea.
# Creating a consistent format, of class pacea_index.

# To update - run through line-by-line. Some of the checks to do with checking
#  older values of an index with the newly-downloaded older values may fail,
#  because recent values may sometimes be tweaked. But test failing will force a
#  manual check - if historical values have changed greatly, this should be
#  mentioned in the NEWS.

# To add a new index - check the saved .txt files below to see which one
#  most closely matches the new one and use the code here as a
#  template (copy to near the end of this file).
#  If none match then congratulations, you have discovered yet another
#  format!
#  Add the new index to the  pacea_index object at the end of this file.

# Can prob delete: TODO
#
#  Adapted from original code supplied by Chris Rooper.
#
# Checking with Tetjana's from SOPO 2022. She has (they all say 'full year'):
# Index    Used below by Chris?    Source file agrees with Chris's?
#D ONI      ENSO ONI                Yes (Chris found the link to the .txt file).
#D NPI      NPI                     Tetjana has
# https://climatedataguide.ucar.edu/sites/default/files/cas_data_files/asphilli/npindex_monthly.txt
# Chris has similar
# https://climatedataguide.ucar.edu/sites/default/files/npindex_monthly.txt
# Both are currently down (29/3/31, 10am; check again at some point, also 6/4/23). This seems
# to have annual anomalies and information:
#  https://climatedataguide.ucar.edu/climate-data/north-pacific-np-index-trenberth-and-hurrell-monthly-and-winter

#  https://climatedataguide.ucar.edu/sites/default/files/2023-01/npindex_anom_ndjfm.txt  - annoying if their websites now have dates in them, bit harder to automate (but doable)
#   or monthly absolute values:
#  https://climatedataguide.ucar.edu/sites/default/files/2023-01/npindex_monthly.txt .
#D PDO      PDO                    Tetjana: http://research.jisao.washington.edu/pdo/ (but data links are borken, 29/3/31)
#                                 Chris: https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.pdo.dat  (works and is updated)
# SOI      SOI                    Same website, still updated.
# NPGO     NPGO                   Same website, still updated.

# Chris also has:
# ENSO MEI
# AO
# ALPI

load_all()
library(dplyr)

# ENSO ONI
download.file("https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt",
              destfile="oni.txt",
              mode="wb",
              quiet = FALSE)

oni_new <- readr::read_table("oni.txt")

stopifnot(colnames(oni_new) == c("SEAS", "YR", "TOTAL", "ANOM"),
          oni_new[1, 1] == "DJF")    # If this fails then months as factors
                                     #  below will become incorrect because it
                                     #  assumes first one is January

colnames(oni_new)<-c("month",
                     "year",
                     "value",
                     "anomaly")

oni_new <- dplyr::relocate(oni_new,
                           year)

oni_new$month <- as.numeric(factor(oni_new$month,
                                   levels=unique(oni_new$month),
                                   ordered=TRUE))
class(oni_new) <- c("pacea_index",
                    class(oni_new))

attr(oni_new, "axis_name") <- "Oceanic Niño Index"

if(check_index_changed(oni, oni_new)){
  # Check previous values haven't changed, but for oni we expect the last two
  #  months of anomaly to get updated (and maybe one month of value)
  expect_equal(oni[1:(nrow(oni) - 2), ],
               oni_new[1:(nrow(oni) - 2), ]) # See note at top if this fails

  oni <- oni_new
  usethis::use_data(oni,
                    overwrite = TRUE)
  plot(oni)    # To check it looks okay; if no figure then hasn't changed
}


# NPI monthly
# The website has 2023-01 in it for date-year, so check later ones for updated
# values. There is no 2023-02 to 2023-04 (as of 2023-04) , presumably just need
# to check that last one then later ones. Tried writing code to automate this
# but proved fiddly, and once we update every month shouldn't have to check too many.

stop("You need to manually update i in the next bit.")

last_npi_monthly <- "2023-04"   # WILL GET UPDATED MANUALLY SOMETIMES - error
                                #  message later will tell you when.

# Create potential new months (note that the filenames do not seem to get
#  updated every month)
last <- lubridate::ym(last_npi_monthly)
today <- lubridate::today()
to_check_dates <- seq(last, today, "1 month")
to_check_stamps <- rev(substring(as.character(to_check_dates), 1, 7))   # Latest first

# Going to see if each file exists, and then take the latest one. Could not
#  figure out a simple way to check if a website exists, so need to do this
#  manually, though will be fairly simple once we are checking this every month.
#  So first check if latest month exists. Gave up trying to automate as
#   RCurl::url.exists(sitename)  returnsed FALSE even with a sitename that exists.

i <- 1      # MAY NEED TO CHANGE THIS MANUALLY - error message below will tell
            # you when.

sitename <- paste0("https://climatedataguide.ucar.edu/sites/default/files/",
                   to_check_stamps[i],
                   "/npindex_monthly.txt")

download.file(sitename,
              destfile = "npi_monthly.txt",
              mode = "wb",
              quiet = FALSE)   # If errors see instructions:

# See next error message for instructions (no need to keep running it once you
#  understand it)

stop(paste("If download.file just gave an error then increase i <- 1 to 2, then 3 etc. if happens again, up to",
           length(to_check_stamps),
           "which should be same as the latest one saved; repeat above code from i.",
           "For the first i for which download.file does NOT error then, to set up for the next future update, replace last_npi_monthly above with",
            to_check_stamps[i],
           "and set i <- 1 again in code. Then keep calm and carry on through this file.",
           sep = "\n"))

npi_monthly_new <- readr::read_table("npi_monthly.txt",
                                     col_names = c("yearmonth", "value"),
                                     skip = 1,
                                     na = "-999.00")    # December 1944, need to
                                                        #  keep in

stopifnot(npi_monthly_new[1,1] == 189901)    # Check still starts in 1899.

npi_monthly_new$month <- as.numeric(substr(npi_monthly_new$yearmonth, 5, 6))
npi_monthly_new$year  <- as.numeric(substr(npi_monthly_new$yearmonth, 1, 4))

npi_monthly_new <- dplyr::select(npi_monthly_new,
                                 year,
                                 month,
                                 value)

class(npi_monthly_new) <- c("pacea_index",
                    class(npi_monthly_new))

attr(npi_monthly_new, "axis_name") <- "North Pacific Index"

if(check_index_changed(npi_monthly, npi_monthly_new)){
  expect_equal(npi_monthly,
               npi_monthly_new[1:nrow(npi_monthly), ]) # See note at top if fails.

  npi_monthly <- npi_monthly_new
  usethis::use_data(npi_monthly,
                    overwrite = TRUE)
  plot(npi_monthly,
       value = "value",
       style = "plain")  # plain not a thing yet, just
                         # not red-blue. Could add in average
                         # value so can show colours
}

# NPI annual
# This website has 2022-10 in it but includes value for 2022 (which by definition
#  includes data from 2023), so not sure about their naming convention. Doing a
# similar manual checking to that above for npi_monthly.

stop("You need to manually update j in the next bit.")

last_npi_annual <- "2023-04"   # WILL GET UPDATED MANUALLY SOMETIMES - error
                                #  message later will tell you when.

# Create potential new months (note that the filenames do not seem to get
#  updated every month); note these replace the ones used above
last <- lubridate::ym(last_npi_annual)
today <- lubridate::today()
to_check_dates <- seq(last, today, "1 month")
to_check_stamps <- rev(substring(as.character(to_check_dates), 1, 7))   # Latest first

# Going to check if the file with latest month exists, and work backwards to ensure
#  getting the latest. Could not
#  figure out a simple way to check if a website exists, so need to do this
#  manually, though will be fairly simple once we are checking this every month.
#  So first check if latest month exists. Gave up trying to automate as
#   RCurl::url.exists(sitename)  returnsed FALSE even with a sitename that exists.

j <- 1      # MAY NEED TO CHANGE THIS MANUALLY - error message below will tell
            # you when.

sitename <- paste0("https://climatedataguide.ucar.edu/sites/default/files/",
                   to_check_stamps[j],
                   "/npindex_ndjfm.txt")

sitename_anomaly <- paste0("https://climatedataguide.ucar.edu/sites/default/files/",
                   to_check_stamps[j],
                   "/npindex_anom_ndjfm.txt")

download.file(sitename,
              destfile = "npi_annual_val.txt",
              mode = "wb",
              quiet = FALSE)

stop(paste("If download.file just gave an error then increase j <- 1 to 2, then 3 etc. if happens again, up to",
           length(to_check_stamps),
           "which should be same as the latest one saved; repeat above code from j.",
           "For the first j for which download.file does NOT error then, to set up for the next future update, replace last_npi_annual above with",
            to_check_stamps[j],
           "and set j <- 1 again in code. Then keep calm and carry on through this file.",
           sep = "\n"))

npi_annual_val_new <- readr::read_table("npi_annual_val.txt",
                                        col_names = c("year", "value"),
                                        skip = 1,
                                        na = "-999.00")    # 1899 (since no 1898 data)

stopifnot(npi_annual_val_new[1,1] == 1899)    # Check still starts in 1899.

# Presumably the anomaly file will also exist, if this fails then do some
# detective work.

download.file(sitename_anomaly,
              destfile = "npi_annual_anom.txt",
              mode = "wb",
              quiet = FALSE)

npi_annual_anom_new <- readr::read_table("npi_annual_anom.txt",
                                         col_names = c("year", "anomaly"),
                                         skip = 1,
                                         na = "-999.00")    # 1899 (since no 1898 data)

stopifnot(npi_annual_anom_new[1,1] == 1899)    # Check still starts in 1899.

npi_annual_new <- dplyr::left_join(npi_annual_val_new,
                                   npi_annual_anom_new,
                                   by = "year")

class(npi_annual_new) <- c("pacea_index",
                           class(npi_annual_new))

attr(npi_annual_new, "axis_name") <- "North Pacific Index"

if(check_index_changed(npi_annual, npi_annual_new)){
  expect_equal(npi_annual,
               npi_annual_new[1:nrow(npi_annual), ]) # See note at top if fails.

  npi_annual <- npi_annual_new
  usethis::use_data(npi_annual,
                    overwrite = TRUE)
  plot(npi_annual)
}

# PDO
# UCAR's second link going to Chris's one that we use below:
# https://climatedataguide.ucar.edu/climate-data/pacific-decadal-oscillation-pdo-definition-and-indices
download.file("https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.pdo.dat",
              destfile = "pdo.txt",
              mode = "wb",
              quiet = FALSE)

pdo_new <- readr::read_table("pdo.txt",
                             skip = 1,
                             na = "99.99")  # Final months

stopifnot(pdo_new[1,1] == 1854) # Check still starts in 1854

pdo_new <- tidyr::pivot_longer(pdo_new,
                               cols = "Jan":"Dec",
                               names_to = "month",
                               values_to = "anomaly") %>%
  mutate(month = as.numeric(match(month, month.abb))) %>%
  rename(year = Year)

pdo_new <- filter(pdo_new,
                  !is.na(anomaly))

class(pdo_new) <- c("pacea_index",
                    class(pdo_new))

attr(pdo_new, "axis_name") <- "Pacific Decadal Oscillation"

if(check_index_changed(pdo, pdo_new)){
  expect_equal(pdo[1:(nrow(pdo) - 1), ],
               pdo_new[1:nrow(pdo) - 1, ]) # See note at top if fails. Final
                                        # value seems to get updated (the final March
                                        # 2023 value did when updating in August 2023).
  pdo <- pdo_new
  usethis::use_data(pdo,
                    overwrite = TRUE)
  plot(pdo)
}

#SOI
download.file("https://www.cpc.ncep.noaa.gov/data/indices/soi",
              destfile = "soi.txt",
              mode = "wb",
              quiet = FALSE)

#soi_new <- readr::read_table("soi.txt",
#                             skip = 3,
#                             na = "-999.9")  # Final months of 2023 plus more,
                                        # but is next to the true final month

# Need Chris's approach due to "-999.9" adjacent to final month of data, and more.
soi_new <- read.table("soi.txt",
                    skip = 3,
                    as.is = TRUE,
                    header = TRUE,
                    fill = TRUE)

stopifnot(soi_new[1,1] == 1951) # Check still starts in 1951

# Year after current year (currently 2023) gets saved as
#  "2024-999.9-999.9-999.9-999.9-999.9-999.9-999.9-999.9-999.9-999.9-999.9-999.9"
# Tetjana only uses the anomaly data in the first half of the file, not the
#  second half (STANDARDIZED DATA), so can just find the above year and delete
#  everything after that.

names(soi_new)[1] <- "year"

soi_new$year <- as.numeric(soi_new$year)   # Converts that 2024 (next) year to NA

soi_new <- soi_new[1:(min(which(is.na(soi_new$year))) - 1), ]  # Remove next year
                                                               # onwards
# Now just have to fix the final month of data (currently March 2023) which is
#  0.3-999.9-.... (without a space between the actual 0.3 value).
soi_new[nrow(soi_new), ] <-  stringr::str_replace_all(soi_new[nrow(soi_new), ],
                                                      "-999.9",
                                                      "")

soi_new <- dplyr::mutate_all(soi_new,
                             function(x) as.numeric(x)) # also adds NA
                                                                 # in final row as needed
soi_new <- tidyr::pivot_longer(soi_new,
                               cols = "JAN":"DEC",
                               names_to = "month",
                               values_to = "anomaly") %>%
  mutate(month = as.numeric(match(month, toupper(month.abb))))

soi_new <- filter(soi_new,
                  !is.na(anomaly))

class(soi_new) <- c("pacea_index",
                    class(soi_new))

attr(soi_new, "axis_name") <- "Southern Oscillation Index"

if(check_index_changed(soi, soi_new)){
  expect_equal(soi,
               soi_new[1:nrow(soi), ])  # See note at top if this fails
  soi <- soi_new
  usethis::use_data(soi,
                    overwrite = TRUE)
  plot(soi)
}


#NGPO
# Useful background:
#  http://www.o3d.org/npgo/
download.file("http://www.o3d.org/npgo/npgo.php",
              destfile="npgo.txt",
              mode="wb",
              quiet = FALSE)

# Adapting Chris's original, as html code in file is otherwise fiddly to deal with
npgo_new <-read.table("npgo.txt",
                 skip = 5,
                 as.is = TRUE,
                 header = FALSE,
                 fill = TRUE,
                 comment = "#") %>%
  as_tibble() %>%
  dplyr::rename("year" = "V1",
                "month" = "V2",
                "anomaly" = "V3") %>%
  filter(!is.na(month)) %>%
  mutate(year = as.numeric(year))

stopifnot(npgo_new[1, 1:2] == c(1950, 1)) # Check still starts in January 1950

class(npgo_new) <- c("pacea_index",
                    class(npgo_new))

attr(npgo_new, "axis_name") <- "North Pacific Gyre Oscillation"

if(check_index_changed(npgo, npgo_new)){
  npgo <- npgo_new
  usethis::use_data(npgo,
                    overwrite = TRUE)
  plot(npgo)  # TODO maybe update when plotting functions finalised
}

# ENSO MEI https://www.psl.noaa.gov/enso/mei
# Add to help:
# Multivariate ENSO Index Version 2 (MEI.v2)
# https://www.psl.noaa.gov/enso/mei
# Row values are 2 month seasons (YEAR DJ JF FM MA AM MJ JJ JA AS SO ON ND)
download.file("https://psl.noaa.gov/enso/mei/data/meiv2.data",
              destfile = "mei.txt",
              mode = "wb",
              quiet = FALSE)

mei_new <- read.table("mei.txt",
                           skip = 1,
                           as.is = TRUE,
                           fill = TRUE) %>%
  as_tibble()

names(mei_new) <- c("year", 1:12)  # Note in help that months are two-month combinations

mei_new$year <- as.numeric(mei_new$year)  # gives warning; puts NA's
                                        # at end, the strange -999 after the
                                        # last year of values stays, so remove it and
                                        # later rows next:
mei_new <-  mei_new[-(seq(which(mei_new$year == -999),
                                     nrow(mei_new))), ]

mei_new <- tidyr::pivot_longer(mei_new,
                               cols = "1":"12",
                               names_to = "month",
                               values_to = "anomaly") %>%
  mutate(month = as.numeric(month),
         anomaly = as.numeric(anomaly)) %>%
  filter(anomaly != -999)

summary(mei_new)     # check no NA's

class(mei_new) <- c("pacea_index",
                    class(mei_new))

attr(mei_new, "axis_name") <- "Multivariate ENSO Index"

if(check_index_changed(mei,
                       mei_new)){
  expect_equal(mei,
               mei_new[1:nrow(mei), ])  # See note at top if this fails
  mei <- mei_new
  usethis::use_data(mei,
                    overwrite = TRUE)
  plot(mei)
}

# AO - Arctic Oscillation
download.file("https://www.cpc.ncep.noaa.gov/products/precip/CWlink/daily_ao_index/monthly.ao.index.b50.current.ascii.table",
              destfile="ao.txt",
              mode="wb",
              quiet = FALSE)

# Adapt above code for pdo_new as it's similar format, but also need to add column
#  name for first column. But need read.table to deal with empty final line (no
#  fill option in read_table).
# Though having done all this just noticed that they also have it in the format
#  similear to what we using:
#  https://www.cpc.ncep.noaa.gov/products/precip/CWlink/daily_ao_index/monthly.ao.index.b50.current.ascii

ao_new <-read.table("ao.txt",
                 header = TRUE,
                 fill = TRUE)

# Years are row names, so make them the first column:
ao_new_years <- row.names(ao_new) %>%
  as.numeric()

ao_new <- cbind("year" = ao_new_years,
                ao_new) %>%
  as_tibble()

row.names(ao_new) <- NULL

stopifnot(ao_new[1,1] == 1950) # Check still starts in 1950

ao_new <- tidyr::pivot_longer(ao_new,
                              cols = "Jan":"Dec",
                              names_to = "month",
                              values_to = "anomaly") %>%
  mutate(month = as.numeric(match(month, month.abb)))

ao_new <- filter(ao_new,
                  !is.na(anomaly))

class(ao_new) <- c("pacea_index",
                    class(ao_new))

attr(ao_new, "axis_name") <- "Arctic Oscillation"

if(check_index_changed(ao, ao_new)){
  expect_equal(ao,
               ao_new[1:nrow(ao_new), ]) # See note at top if fails. Not tested
                                        # this one yet so it may fail if final
                                        # value gets revised; if so tweak here.
  ao <- ao_new
  usethis::use_data(ao,
                    overwrite = TRUE)
  plot(ao)
}

# ALPI - not updated since 2015, see ?alpi. So no need to keep running this each
#  month.
if(FALSE){      # Change to TRUE if we want to update, though it'd probably be
                #  done some other way anyway
  download.file("https://api-proxy.edh.azure.cloud.dfo-mpo.gc.ca/catalogue/records/4bb821ce-bef7-46d3-95d2-064065f1bda4/attachments/alpi_1900_2015_en.csv",
                destfile = "alpi.txt",
                mode="wb",
                quiet = FALSE)

  # Can't use read_table as , separated without spaces
  alpi_new <- read.csv("alpi.txt") %>%
    as_tibble() %>%
    dplyr::rename("year" = "YEAR",
                  "anomaly" = "ALEUTIAN.LOW.PRESSURE.INDEX..ALPI.")

  stopifnot(alpi_new[1, 1:2] == c(1900, 1.3)) # Check still starts in 1950

  class(alpi_new) <- c("pacea_index",
                       class(alpi_new))

  attr(alpi_new, "axis_name") <- expression(paste(plain(Aleutian) * " " * plain(Low) * " " * plain(Pressure) * " " * plain(Index) * ", " * 10^6 * km^2))
  # Must be an easier way of doing that, but it works

  # if(check_index_changed(alpi, alpi_new)){  # this needs updating as it errors with
  #  the complex axis name, but that's only needed if ALPI is going to be updated
  #  (and so this section should be uncommented).
  # alpi <- alpi_new
  # usethis::use_data(alpi,
  #                  overwrite = TRUE)
  # plot(alpi)
  # }
}

# pacea_indices - saving data frame of all indices and ranges to easily see, and
#  automatically include in vignette. Ordering by start year (did with arrange
#  then redoing myself for ease of adding things in).

pacea_indices <-
  dplyr::tribble(
           ~Object, ~Description, ~Resolution, ~`Start year`, ~`End year`,
           "pdo", "Pacific Decadal Oscillation", "monthly", min(pdo$year), max(pdo$year),
           "npi_monthly", "North Pacific Index (monthly)", "monthly", min(npi_monthly$year), max(npi_monthly$year),
           "npi_annual", "North Pacific Index (annual)", "annual", min(npi_annual$year), max(npi_annual$year),
           "alpi", "Aleutian Low Pressure Index", "annual", min(alpi$year), max(alpi$year),
           "oni", "Oceanic Niño Index", "monthly", min(oni$year), max(oni$year),
           "npgo", "North Pacific Gyre Oscillation", "monthly", min(npgo$year), max(npgo$year),
           "ao", "Arctic Oscillation", "monthly", min(ao$year), max(ao$year),
           "soi", "Southern Oscillation Index", "monthly", min(soi$year), max(soi$year),
           "mei", "Multivariate El Niño Southern Oscillation Index", "monthly", min(mei$year), max(mei$year)) %>%
  arrange(`Start year`)

# if(pacea_indices_new != pacea_indices){   # couldn't figure out, or using expect_equal
#  pacea_indices <- pacea_indices_new
usethis::use_data(pacea_indices,
                   overwrite = TRUE)
pacea_indices
