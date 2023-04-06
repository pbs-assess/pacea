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

# If it's changed from what is currently saved then save the new version.
# (Tried testthat::expect_equal but it returned tibble of FALSE's)
# Will need this again so should make a function.
if(nrow(oni) != nrow(oni_new) |
   ncol(oni) != ncol(oni_new) |
   attr(oni, "axis_name") != attr(oni_new, "axis_name")){

  oni <- oni_new

  usethis::use_data(oni,
                    overwrite = TRUE)} else {
  # dimensions are the same so check values
  if(!(all(oni == oni_new))){
    oni <- oni_new

    usethis::use_data(oni,
                      overwrite = TRUE)
  }
}
# Without the all_equal() check, use_data does
#  overwrite the old file even if nothing has changed, so the date stamp changes,
#  but Git sees that nothing has changed so doesn't require a commit; seems best
#  to keep the expect_equal check, even though we could get away with not having
#  it (it will avoid confusing timestamps that are newer than the last commit of
#  a file).

plot(oni)    # to check it looks okay



stop("Got to here")

#NPI
# From
# https://climatedataguide.ucar.edu/climate-data/north-pacific-np-index-trenberth-and-hurrell-monthly-and-winter
# The North Pacific Index (NP index or NPI) is the area-weighted sea level
# pressure over the region 30 deg N-65 deg N, 160 deg E-140 deg W. The NP index is defined to measure interannual to decadal variations in the atmospheric circulation. The dominant atmosphere-ocean relation in the North Pacific is one where atmospheric changes lead changes in sea surface temperatures by one to two months. However, strong ties exist with events in the tropical Pacific, with changes in tropical Pacific SSTs leading SSTs in the north Pacific by three months.

download.file("https://climatedataguide.ucar.edu/sites/default/files/npindex_monthly.txt",
              destfile="NPI.txt",
              mode="wb",
              quiet = FALSE)

NPI<-read.table("NPI.txt",
                skip=1,
                as.is=TRUE,
                header=FALSE,
                fill=TRUE)

NPI[NPI==(-999)] <- NA

NPI<-data.frame(Year=floor(NPI$V1/100),
                Month=seq(1, 12, 1),
                NPI=NPI$V2)

Coastwide_Index_DF<-merge(merge(merge(merge(merge(merge(ENSO_MEI,ENSO_ONI,by=c("Year",'Month'),all=TRUE),AO,by=c("Year",'Month'),all=TRUE),NPGO,by=c("Year",'Month'),all=TRUE),PDO,by=c("Year",'Month'),all=TRUE),SOI,by=c("Year",'Month'),all=TRUE),NPI,by=c("Year",'Month'),all=TRUE)

# Poly_ID == -1 tells pacea that these values are coastwide
Coastwide_Index_DF$Poly_ID <- -1

Coastwide_Index_DF$Month <- as.numeric(Coastwide_Index_DF$Month)

Coastwide_Index_DF <-
  Coastwide_Index_DF[!is.na(Coastwide_Index_DF$Year) &
                       !is.na(Coastwide_Index_DF$Month),]

# Commenting for now so don't overwrite.
# use_data(Coastwide_Index_DF, overwrite = T)



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
