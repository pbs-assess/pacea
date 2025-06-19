# From Chris Rooper in Issue #57, then adapting below. Just step through line by
# line like for indices.

library(dplyr)

#Fraser river discharge at Hope monthly values
# download.file from "https://wateroffice.ec.gc.ca/
if(FALSE){         # not running
  url1 <-
    paste0("https://wateroffice.ec.gc.ca/services/daily_data/csv/inline?stations[]=08MF005&parameters[]=flow&start_date=1912-01-01&end_date=",as.Date(Sys.time()))

  download.file(url = url1,
                destfile = "fraserdischarge.csv")
  FraserDischarge <- read.csv("fraserdischarge.csv",
                              header=TRUE,
                              skip=0)
  FraserDischarge$Month <- format(as.Date(FraserDischarge$Date),
                                  "%m")
  FraserDischarge$Year <- format(as.Date(FraserDischarge$Date),
                                 "%Y")
  FraserDischarge <- aggregate(Value.Valeur~Month+Year,
                               data=FraserDischarge,
                               FUN="mean")
  FraserDischarge <- data.frame(Year=FraserDischarge$Year,
                                Month=FraserDischarge$Month,
                                Indicator="FraserDischarge",
                                Value=FraserDischarge$Value.Valeur)
}

# Adapting the above, which only went up to 2023. Think 2024 might still be
# provisional, can manually get 2024 from:

# https://wateroffice.ec.gc.ca/download/report_e.html?dt=6&df=csv&ext=zip

# Not sure how that knows I want station 08MF005. From
# https://wateroffice.ec.gc.ca/report/real_time_e.html?stn=08MF005&mode=Table&startDate=2024-01-01&endDate=2024-12-31&prm1=46&y1Max=&y1Min=&prm2=47&y2Max=&y2Min=
# and then download.

# Saved as .zip, manually unzip to here, and then:
raw_2024 <- read.csv("08MF005_QRD_20250618T2051.csv",
                     header = TRUE,
                     skip = 9) %>%
  as_tibble()

raw_2024
raw_2024 %>% tail()

daily_2024 <- mutate(raw_2024,
                      year = lubridate::year(lubridate::ymd_hms(Date..PST.)),
                      month = lubridate::month(lubridate::ymd_hms(Date..PST.))) %>%
  rename(discharge = Value..m..s.)

daily_2024
daily_2024 %>% tail()

monthly_2024 <- summarise(group_by(daily_2024,
                                   month),
                          year = unique(year),
                          month = unique(month),
                          mean_for_month = mean(discharge),   # mean daily value
                          max_for_month = max(discharge),     # max daily value
                          num_days = n()) %>%
  ungroup()        # not actually need as only grouping by one thing
monthly_2024   # Can see min is 29, so no missing days

monthly_2024 <- select(monthly_2024,
                       -"num_days") %>%
  relocate(year,
           month)
monthly_2024

url1 <-
  paste0("https://wateroffice.ec.gc.ca/services/daily_data/csv/inline?stations[]=08MF005&parameters[]=flow&start_date=1912-01-01&end_date=",as.Date(Sys.time()))

download.file(url = url1,
              destfile = "fraser_discharge.csv")
raw_full <- read.csv("fraser_discharge.csv",
                            header=TRUE,
                     skip=0) %>%
  as_tibble() %>%
  mutate_at(vars(X.ID,
                 Parameter.Paramètre,
                 Symbol.Symbole),
            factor)
raw_full
summary(raw_full)      # So can remove ID and Parameter (doing below). And it
                       # starts on 1st of a month.

# Think symbol might be to do with the collection type, website doesn't seem to say
with_symbol <- filter(raw_full,
                      !(Symbol.Symbole == ""))
with_symbol
with_symbol %>% tail()

# There are some of these near the end, so not sure. Just stick with them all
# for now.

daily_full <- mutate(raw_full,
                     year = lubridate::year(lubridate::ymd(Date)),
                     month = lubridate::month(lubridate::ymd(Date))) %>%
  rename(discharge = Value.Valeur) %>%
  select(-c(X.ID,
            Parameter.Paramètre,
            Symbol.Symbole))

daily_full %>% tail()

monthly_full <- summarise(group_by(daily_full,
                                   year,
                                   month),
                          year = unique(year),
                          month = unique(month),
                          mean_for_month = mean(discharge),   # mean daily value
                          max_for_month = max(discharge),     # max daily value
                          num_days = n()) %>%
  ungroup()

min(monthly_full$num_days)  # is 28, so no missing days.

monthly_full <- select(monthly_full,
                       -"num_days")

monthly_full

monthly_2024

expect_equal(names(monthly_full),
             names(monthly_2024))

fraser_discharge <- rbind(monthly_full,
                          monthly_2024)

# Mean of daily values over the month
fraser_discharge_mean <- select(fraser_discharge,
                                -max_for_month) %>%
  rename(value = mean_for_month)    # can then use indices plotting, need
                                    #  attribute for axis label for each time
                                    #  series, so doing mean and max separately.

class(fraser_discharge_mean) <- c("pacea_index",
                                  class(fraser_discharge_mean))

attr(fraser_discharge_mean, "axis_name") <-
  "Fraser River discharge - mean of daily values"

# plot.pacea_index(fraser_discharge_mean, value = "value")

usethis::use_data(fraser_discharge_mean,
                  overwrite = TRUE)

# Max of daily values over the month, call it peak
fraser_discharge_peak <- select(fraser_discharge,
                               -mean_for_month) %>%
  rename(value = max_for_month)

class(fraser_discharge_peak) <- c("pacea_index",
                                  class(fraser_discharge_peak))

attr(fraser_discharge_peak, "axis_name") <-
  "Fraser River discharge - peak of daily values"

# plot.pacea_index(fraser_discharge_peak, value = "value")

usethis::use_data(fraser_discharge_peak,
                  overwrite = TRUE)




stop("rest is for the help files")

# For help files:
For real-time data retrieved from the Wateroffice web site:
"Extracted from the Environment and Climate Change Canada Real-time Hydrometric Data web site (https://wateroffice.ec.gc.ca/mainmenu/real_time_data_index_e.html) on [DATE]"

For historical data retrieved from the Wateroffice web site:
"Extracted from the Environment and Climate Change Canada Historical Hydrometric Data web site (https://wateroffice.ec.gc.ca/mainmenu/historical_data_index_e.html) on [DATE]"

# From https://wateroffice.ec.gc.ca/report/statistics_e.html?stn=08MF005
Data Collection History
This table contains information pertaining to the historical changes of defined elements in the operation of a station.
Type	Operation schedule	Gauge type
1912 - 1949	Flow	Continuous	Manual
1950 - 1995	Flow	Continuous	Recorder
1996 - 2025	Flow & Level	Continuous	Recorder
Historical Hydrometric Remarks

DRAINAGE AREA INCLUDES THE 14 000 KM2 BEHIND KENNEY DAM
Annual Hydrometric Remarks

WATER LEVEL NOT AVAILABLE 2023-12-28 TO 2024-01-08.

Note for historic data the "Symbol.Symbole" column was not defined, and we have
kept all values. A `summary()` gives:
 Symbol.Symbole
  :38828
 A:  523
 B:  822
 E:  675

i.e. 38828 with no symbol There were some symbols near the end of the time
series, so it's not just a historical thing.


`fraser_discharge_mean` and `fraser_discharge_max` represent, for each month, the mean daily discharge and the maximum daily discharge, respectively. Add units - m3/s.
