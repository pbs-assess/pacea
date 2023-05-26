# Obtaining the oceanographic basin indices and compile them into pacea. Code is
#  best run line-by-line to check plots etc.
#
#  Converted from original code supplied by Chris Rooper. Moving updated calls
#  to the top, to save each data set as our new format.
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


# Column names
#year_months <- c("Year", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
#                  "11", "12")

load_all()

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

oni_new$month <- as.numeric(factor(oni_new$month,
                                   levels=unique(oni_new$month),
                                   ordered=TRUE))
class(oni_new) <- c("pacea_index",
                    class(oni_new))

attr(oni_new, "axis_name") <- "Oceanic Niño Index"

if(check_index_changed(oni, oni_new)){
  oni <- oni_new
  usethis::use_data(oni,
                    overwrite = TRUE)
  plot(oni)    # to check it looks okay; if no figure then hasn't changed
}


# NPI monthly
# The website has 2023-01 in it for date-year, so check later ones for updated
# values. There is no 2023-02 to 2023-04, presumably just need to check that
# last one then later ones. Could write code to automate this.

download.file("https://climatedataguide.ucar.edu/sites/default/files/2023-01/npindex_monthly.txt",
              destfile="npi_monthly.txt",
              mode="wb",
              quiet = FALSE)

npi_monthly_new <- readr::read_table("npi_monthly.txt",
                                     col_names = c("yearmonth", "value"),
                                     skip = 1,
                                     na = "-999.00")    # December 1944

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
  npi_monthly <- npi_monthly_new
  usethis::use_data(npi_monthly,
                    overwrite = TRUE)
  plot(npi_monthly, value = "value", style = "plain")  # plain not a thing yet, just
                                             # not red-blue TODO add in average
                                             # value so can show colours
}

# NPI annual
# This website has 2022-10 in it but includes value for 2022 (which by definition
#  includes data from 2023), so not sure about their naming convention.

download.file("https://climatedataguide.ucar.edu/sites/default/files/2022-10/npindex_ndjfm.txt",
              destfile="npi_annual_val.txt",
              mode="wb",
              quiet = FALSE)

npi_annual_val_new <- readr::read_table("npi_annual_val.txt",
                                        col_names = c("year", "value"),
                                        skip = 1,
                                        na = "-999.00")    # 1899 (since no 1898 data)

stopifnot(npi_annual_val_new[1,1] == 1899)    # Check still starts in 1899.


download.file("https://climatedataguide.ucar.edu/sites/default/files/2023-01/npindex_anom_ndjfm.txt",
              destfile="npi_annual_anom.txt",
              mode="wb",
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
  npi_annual <- npi_annual_new
  usethis::use_data(npi_annual,
                    overwrite = TRUE)
  plot(npi_annual, value = "value", style = "plain")  # plain not a thing yet, just
                                             # not red-blue TODO add in average
                                             # value so can show colours
}

# PDO
# UCAR's second link going to Chris's one that we use below:
# https://climatedataguide.ucar.edu/climate-data/pacific-decadal-oscillation-pdo-definition-and-indices
download.file("https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.pdo.dat",
              destfile="pdo.txt",
              mode="wb",
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

class(pdo_new) <- c("pacea_index",
                    class(pdo_new))

attr(pdo_new, "axis_name") <- "Pacific Decadal Oscillation"

if(check_index_changed(pdo, pdo_new)){
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
                    fill=T)

stopifnot(soi_new[1,1] == 1951) # Check still starts in 1951

# Year after current year (currently 2023) gets saved as
#  "2024-999.9-999.9-999.9-999.9-999.9-999.9-999.9-999.9-999.9-999.9-999.9-999.9"
# Tetjana only uses the anomaly data in the first half of the file, not the
#  second half (STANDARDIZED DATA), so can just fine the above year and delete
#  everything after that.

names(soi_new)[1] <- "year"

soi_new$year <- as.numeric(soi_new$year)   # Converts that 2024 year to NA

soi_new <- soi_new[1:(min(which(is.na(soi_new$year))) - 1), ]  # Remove 2024
                                                               # onwards
# Now just have to fix the final month of data (currently March 2023) which is
#  0.3-999.9-.... (without a space between the actual 0.3 value).
soi_new[nrow(soi_new), ] <-  stringr::str_replace_all(soi_new[nrow(soi_new), ],
                                                      "-999.9",
                                                      "")

soi_new <- dplyr::mutate_all(soi_new, function(x) as.numeric(x)) # also adds NA
                                                                 # in final row as needed
soi_new <- tidyr::pivot_longer(soi_new,
                               cols = "JAN":"DEC",
                               names_to = "month",
                               values_to = "anomaly") %>%
  mutate(month = as.numeric(match(month, toupper(month.abb))))

class(soi_new) <- c("pacea_index",
                    class(soi_new))

attr(soi_new, "axis_name") <- "Southern Oscillation Index"

if(check_index_changed(soi, soi_new)){
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

stop("Got to here")

# ENSO MEI
n_lines <- as.numeric(format(Sys.time(),
                             "%Y")) - 1978     # Then double check later that
                                              #  first year is 1979 and last is
                                              #  current year (ish)
# TODO think about that above line more, as to when things get updated

download.file("https://psl.noaa.gov/enso/mei/data/meiv2.data",
              destfile="enso_mei.txt",
              mode="wb",
              quiet = FALSE)

enso_mei <- read.table("enso_mei.txt",
                       skip = 1,
                       as.is = TRUE,
                       nrows = n_lines)
HERE

colnames(ENSO_MEI) <- year_months

ENSO_MEI[ENSO_MEI == -999] <- NA

ENSO_MEI <- reshape::melt(ENSO_MEI,
                          id="Year")

colnames(ENSO_MEI)<-c("Year","Month","ENSO_MEI")

ENSO_MEI$Month <- as.numeric(ENSO_MEI$Month)

# If get an error (or earlier) then likely that it's early in a year and there
#  are no values for that year yet. Depends when the website updates their table length.
stopifnot(min(ENSO_MEI$Year) == 1979,
          max(ENSO_MEI$Year) ==  format(Sys.time(), "%Y"))

# Add to help:
# Multivariate ENSO Index Version 2 (MEI.v2)
# https://www.psl.noaa.gov/enso/mei
# Row values are 2 month seasons (YEAR DJ JF FM MA AM MJ JJ JA AS SO ON ND)



# AO
download.file("https://www.cpc.ncep.noaa.gov/products/precip/CWlink/daily_ao_index/monthly.ao.index.b50.current.ascii.table",
              destfile="AO.dat",
              mode="wb",
              quiet = FALSE)

AO <- read.table("AO.dat",
                 skip=1,
                 as.is=TRUE,
                 header=F,
                 fill=T)

colnames(AO) <- year_months

AO <- reshape::melt(AO,
                  id="Year")

colnames(AO) <- c('Year',
                  'Month',
                  'AO_Index')

AO$Month <- as.numeric(AO$Month)

**GOT TO HERE**

#ALPI - CURRENT DATA NOT AVAILABLE - LAST UPDATE 2015
download.file("http://www.pac.dfo-mpo.gc.ca/od-ds/science/alpi-idda/ALPI_1900_2015_EN.csv",
              destfile="ALPI.csv",
              mode="wb",
              quiet = FALSE)

ALPI<-read.csv("ALPI.csv",
               skip=0,
               as.is=TRUE,
               header=TRUE)

colnames(ALPI)<-c("Year",
                  "ALPI")

plot(ALPI)
