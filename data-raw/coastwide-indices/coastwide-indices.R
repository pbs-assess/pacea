# Obtaining the oceanographic basin indices and compile them into pacea. Code is
#  best run line-by-line to check plots etc.
#
#  Converted from original code supplied by Chris Rooper. Moving updated calls
#  to the top, to save each data set as our new format.
#
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
                               values_to = "anom") %>%
  mutate(month = as.numeric(match(month, month.abb))) %>%
  rename(year = Year)

class(pdo_new) <- c("pacea_t",
                    class(pdo_new))

attr(pdo_new, "axis_name") <- "Pacific Decadal Oscillation"

if(check_index_changed(pdo, pdo_new)){
  pdo <- pdo_new
  usethis::use_data(pdo,
                    overwrite = TRUE)
  plot(pdo)
}

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
