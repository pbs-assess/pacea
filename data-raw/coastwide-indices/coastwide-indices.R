# Obtaining the oceanographic basin indices and compile them into pacea. Code is
#  best run line-by-line to check plots etc.
#
#  Converted from original code supplied by Chris Rooper. Moving updated calls
#  to the top, to save each data set as our new format.

# Probably not using, see Issue #9: Note that these time series of indices are NOT stored across the
#  BC grid. This saves a lot of space. Instead, pacea recognises that a covariate
#  is `coastwide' if the `Poly_ID` value is set to `-1`.

# Checking with Tetjana's from SOPO 2022. She has (they all say 'full year'):
# Index    Used below by Chris?    Source file agrees with Chris's?
# ONI      ENSO ONI                Yes (Chris found the link to the .txt file).
# NPI      NPI                     Tetjana has
# https://climatedataguide.ucar.edu/sites/default/files/cas_data_files/asphilli/npindex_monthly.txt
# Chris has similar
# https://climatedataguide.ucar.edu/sites/default/files/npindex_monthly.txt
# Both are currently down (29/3/31, 10am; check again at some point, also 6/4/23). This seems
# to have annual anomalies and information:
#  https://climatedataguide.ucar.edu/climate-data/north-pacific-np-index-trenberth-and-hurrell-monthly-and-winter

#  https://climatedataguide.ucar.edu/sites/default/files/2023-01/npindex_anom_ndjfm.txt  - annoying if their websites now have dates in them, bit harder to automate (but doable)
#   or monthly absolute values:
#  https://climatedataguide.ucar.edu/sites/default/files/2023-01/npindex_monthly.txt .
# PDO      PDO                    Tetjana: http://research.jisao.washington.edu/pdo/ (but data links are borken, 29/3/31)
#                                 Chris: https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.pdo.dat  (works and is updated)
# SOI      SOI                    Same website, still updated.
# NPGO     NPGO                   Same website, still updated.

# Chris also has:
# ENSO MEI
# AO
# ALPI


# Column names
# year_months <- c("Year", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
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
                     "val",
                     "anom")

oni_new$month <- as.numeric(factor(oni_new$month,
                                   levels=unique(oni_new$month),
                                   ordered=TRUE))
class(oni_new) <- c("pacea_t",
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
                             col_names = c("yearmonth", "val"),
                             skip = 1,
                             na = "-999.00")    # December 1944

stopifnot(npi_monthly_new[1,1] == 189901)    # Check still starts in 1899.

npi_monthly_new$month <- as.numeric(substr(npi_monthly_new$yearmonth, 5, 6))
npi_monthly_new$year  <- as.numeric(substr(npi_monthly_new$yearmonth, 1, 4))

npi_monthly_new <- dplyr::select(npi_monthly_new,
                         year,
                         month,
                         val)

class(npi_monthly_new) <- c("pacea_t",
                    class(npi_monthly_new))

attr(npi_monthly_new, "axis_name") <- "North Pacific Index"

if(check_index_changed(npi_monthly, npi_monthly_new)){
  npi_monthly <- npi_monthly_new
  usethis::use_data(npi_monthly,
                    overwrite = TRUE)
  plot(npi_monthly, value = "val", style = "plain")  # plain not a thing yet, just
                                             # not red-blue TODO add in average
                                             # value so can show colours
}

# NPI annual
# This webiste has 2022-10 in it but includes value for 2022 (which by definition
#  includes data from 2023), so not sure about their naming convention.

download.file("https://climatedataguide.ucar.edu/sites/default/files/2022-10/npindex_ndjfm.txt",
              destfile="npi_annual_val.txt",
              mode="wb",
              quiet = FALSE)

npi_annual_val_new <- readr::read_table("npi_annual_val.txt",
                                        col_names = c("year", "val"),
                                        skip = 1,
                                        na = "-999.00")    # 1899 (since no 1898 data)

stopifnot(npi_annual_val_new[1,1] == 1899)    # Check still starts in 1899.


download.file("https://climatedataguide.ucar.edu/sites/default/files/2023-01/npindex_anom_ndjfm.txt",
              destfile="npi_annual_anom.txt",
              mode="wb",
              quiet = FALSE)

npi_annual_anom_new <- readr::read_table("npi_annual_anom.txt",
                                         col_names = c("year", "anom"),
                                         skip = 1,
                                         na = "-999.00")    # 1899 (since no 1898 data)

stopifnot(npi_annual_anom_new[1,1] == 1899)    # Check still starts in 1899.

npi_annual_new <- dplyr::left_join(npi_annual_val_new,
                                   npi_annual_anom_new,
                                   by = "year")

class(npi_annual_new) <- c("pacea_t",
                           class(npi_annual_new))

attr(npi_annual_new, "axis_name") <- "North Pacific Index"

if(check_index_changed(npi_annual, npi_annual_new)){
  npi_annual <- npi_annual_new
  usethis::use_data(npi_annual,
                    overwrite = TRUE)
  plot(npi_annual, value = "val", style = "plain")  # plain not a thing yet, just
                                             # not red-blue TODO add in average
                                             # value so can show colours
}

TODO make plot work for objects without months

stop("Got to here")


# ENSO MEI
nlines <- as.numeric(format(Sys.time(),
                            "%Y")) - 1978     # Then double check later that
                                              #  first year is 1979

download.file("https://psl.noaa.gov/enso/mei/data/meiv2.data",
              destfile="ENSO_MEI.txt",
              mode="wb",
              quiet = FALSE)

ENSO_MEI <- read.table("ENSO_MEI.txt",
                       skip = 1,
                       as.is = TRUE,
                       nrows = nlines)

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


# PDO
# From http://research.jisao.washington.edu/pdo/
# The "Pacific Decadal Oscillation" (PDO) is a long-lived El Niño-like pattern of Pacific climate variability. While the two climate oscillations have similar spatial climate fingerprints, they have very different behavior in time. Fisheries scientist Steven Hare coined the term "Pacific Decadal Oscillation" (PDO) in 1996 while researching connections between Alaska salmon production cycles and Pacific climate (his dissertation topic with advisor Robert Francis). Two main characteristics distinguish PDO from El Niño/Southern Oscillation (ENSO): first, 20th century PDO "events" persisted for 20-to-30 years, while typical ENSO events persisted for 6 to 18 months; second, the climatic fingerprints of the PDO are most visible in the North Pacific/North American sector, while secondary signatures exist in the tropics - the opposite is true for ENSO. Several independent studies find evidence for just two full PDO cycles in the past century: "cool" PDO regimes prevailed from 1890-1924 and again from 1947-1976, while "warm" PDO regimes dominated from 1925-1946 and from 1977 through (at least) the mid-1990's. Shoshiro Minobe  has shown that 20th century PDO fluctuations were most energetic in two general periodicities, one from 15-to-25 years, and the other from 50-to-70 years.
## http://ingrid.ldeo.columbia.edu/%28/home/alexeyk/mydata/TSsvd.in%29readfile/.SST/.PDO/

## Major changes in northeast Pacific marine ecosystems have been correlated with phase changes in the PDO; warm eras have seen enhanced coastal ocean biological productivity in Alaska and inhibited productivity off the west coast of the contiguous United States, while cold PDO eras have seen the opposite north-south pattern of marine ecosystem productivity.

## Causes for the PDO are not currently known. Likewise, the potential
## predictability for this climate oscillation are not known. Some climate
## simulation models produce PDO-like oscillations, although often for different
## reasons. The mechanisms giving rise to PDO will determine whether skillful
## decades-long PDO climate predictions are possible. For example, if PDO arises
## from air-sea interactions that require 10 year ocean adjustment times, then
## aspects of the phenomenon will (in theory) be predictable at lead times of up to
## 10 years. Even in the absence of a theoretical understanding, PDO climate
## information improves season-to-season and year-to-year climate forecasts for
## North America because of its strong tendency for multi-season and multi-year
## persistence. From a societal impacts perspective, recognition of PDO is
## important because it shows that "normal" climate conditions can vary over time
## periods comparable to the length of a human's lifetime .

#download.file("https://www.ncdc.noaa.gov/teleconnections/pdo/data.csv", destfile="PDO.csv",mode="wb", quiet = FALSE)
download.file("https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.pdo.dat",
              destfile="PDO.dat",
              mode="wb",
              quiet = FALSE)

PDO <- read.table("PDO.dat",
                  skip=1,
                  as.is=TRUE,
                  header=TRUE,
                  sep='',
                  fill=T)

colnames(PDO)<- year_months

PDO<-reshape::melt(PDO,
                   id="Year")

colnames(PDO)<-c("Year",
                 "Month",
                 "PDO_Index")

PDO$Month <- as.numeric(PDO$Month)

PDO[PDO == 99.99] <- NA

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
#SOI
download.file("https://www.cpc.ncep.noaa.gov/data/indices/soi",
              destfile = "SOI.dat",
              mode = "wb",
              quiet = FALSE)

SOI<-read.table("SOI.dat",
                skip = 3,
                as.is = TRUE,
                header = TRUE,
                fill=T)

SOI$YEAR <- as.numeric(SOI$YEAR)

SOI <- SOI[!is.na(SOI$YEAR),]

SOI<-reshape::melt(SOI,id="YEAR")

colnames(SOI) <- c('Year',
                   'Month',
                   'SOI_Anomaly_Index')

SOI$SOI_Anomaly_Index[grepl(SOI$SOI_Anomaly_Index, pattern='9-999')] <- NA

SOI <- SOI %>%
  group_by(Year, Month) %>%
  mutate(SOI_Standardized_Index = as.numeric(SOI_Anomaly_Index[2]),
         SOI_Anomaly_Index = as.numeric(SOI_Anomaly_Index[1])) %>%
  filter(row_number()==1)

SOI$Month <- as.numeric(SOI$Month)

#NGPO
# Useful background:
#  http://www.o3d.org/npgo/
download.file("http://www.o3d.org/npgo/npgo.php",
              destfile="NPGO.txt",
              mode="wb",
              quiet = FALSE)

NPGO<-read.table("NPGO.txt",
                 skip=30,
                 as.is=TRUE,
                 header=FALSE,
                 fill=TRUE)

colnames(NPGO)<-c("Year",
                  "Month",
                  "NPGO_index")

NPGO$Year<-as.numeric(NPGO$Year)

NPGO<-subset(NPGO,is.na(NPGO$NPGO_index)==FALSE)

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
