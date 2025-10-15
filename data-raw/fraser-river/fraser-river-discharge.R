# From Chris Rooper in Issue #57, then adapting below. Just step through line by
# line like for indices.
# Fraser river discharge at Hope monthly values.
# Didn't fully re-run this all from scratch after reorganising (9/7/25), but it
# adds on to the existing saved objects now. The 2024 onwards data gets updated
# every day, but we check to see if we have a full month added on.

# BUT, can only get last 18 months of values. So need to just download the new
# calendar year and add onto it. OR, how about just check the final date we
# already have, then just extract from then to today, as have to do manually
# anyway. And need to just extend original since updating on 2025-10-15 can't
# get the early 2024 values, so need to stick with what we have in pacea already.
library(dplyr)
library(lubridate)
load_all()

# Adapting extraction up to end of 2023 from Chris's code. No need to keep
# re-running it.

# This goes up to end of 2023.
if(FALSE){              # Change if want to rerun, not sure when would want to
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
}

# 2025-10-15: so can only download 18 months worth, so end up losing Jan-Mar
# 2024. So need to change this to download the current calendar year, or even
# just since the last update. Need to think about. HERE


# Manually get 2024 onwards from the following (think automatically getting it
# might still have been provisional at some point):

# https://wateroffice.ec.gc.ca/download/report_e.html?dt=6&df=csv&ext=zip
# 9/7/25 - the above link (after I messed around with the one below and changed
# dates today) gives daily mean values to today, which is what we want. So try
# that again going forward, maybe it's just all of them up to today.

# Not sure how that knows I want station 08MF005. FRASER RIVER AT HOPE. Must be
# default, as came up on laptop2025 first time, using link below.

# From
# https://wateroffice.ec.gc.ca/report/real_time_e.html?stn=08MF005&mode=Table&startDate=2024-01-01&endDate=2024-12-31&prm1=46&y1Max=&y1Min=&prm2=47&y2Max=&y2Min=
# and then download.

# Saved the first one as .zip, manually unzip to here, and then (updating on
# 2025-07-09, can tell as date is in the filename:

# This is 2024 onwards, could change 2024 to 2024_onwards
raw_2024 <- read.csv("08MF005_QRD_20250709T2035.csv",
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

final_day <-
  lubridate::ceiling_date(lubridate::ymd_hms(daily_2024$Date..PST.[nrow(daily_2024)]))
final_day           # should have today or yesterday at the end (not sure
                    # how they calculated the mean)

monthly_2024 <- summarise(group_by(daily_2024,
                                   year,
                                   month),
                          year = unique(year),
                          month = unique(month),
                          mean_for_month = mean(discharge),   # mean daily value
                          max_for_month = max(discharge),     # max daily value
                          num_days = n()) %>%
  ungroup()
monthly_2024   # Can see no missing days, expect (usually) final month, so
               # remove that one if final dates isn't end of a month

if(final_day != ceiling_date(final_day, unit = "month") - days(1)){
  monthly_2024 <- monthly_2024[-nrow(monthly_2024), ]
}

monthly_2024 <- select(monthly_2024,
                       -"num_days")
monthly_2024

# Now combine new values to existing ones for mean
if(exists("monthly_full")){     # Then it's been created again in big loop above
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

  # Max of daily values over the month, call it peak
  fraser_discharge_peak <- select(fraser_discharge,
                                  -mean_for_month) %>%
    rename(value = max_for_month)    # can then use indices plotting, need
  #  attribute for axis label for each time
  #  series, so doing peak and max separately.

  class(fraser_discharge_peak) <- c("pacea_index",
                                    class(fraser_discharge_peak))

} else {

  # If monthly_full does not exist, just append new values to end of existing data

  # Mean:
  monthly_2024_mean <- select(monthly_2024,
                              -max_for_month) %>%
    rename(value = mean_for_month)

  fraser_discharge_mean_new <- rbind(fraser_discharge_mean,
                                     monthly_2024_mean) %>%   # can have duplicates
    dplyr::distinct()                                         # remove duplicates

  fraser_discharge_mean <- fraser_discharge_mean_new

  # And for peak:
  monthly_2024_peak <- select(monthly_2024,
                              -mean_for_month) %>%
    rename(value = max_for_month)

  fraser_discharge_peak_new <- rbind(fraser_discharge_peak,
                                     monthly_2024_peak) %>%   # can have duplicates
    dplyr::distinct()                                         # remove duplicates

  fraser_discharge_peak <- fraser_discharge_peak_new
}

# If change this then update plot.pacea_index():
# Copying from ALPI:
attr(fraser_discharge_mean, "axis_name") <- expression(paste(plain(Fraser) *
    " " * plain(River) * " " * plain(discharge) * " " *
    plain(mean) * " " * plain(of) * " " * plain(daily) * " " * plain(values) *
    ", " * m^3 * s^-1))

fraser_discharge_mean

fraser_discharge_mean %>% a() %>% tail(35)

summary(fraser_discharge_mean)

plot(fraser_discharge_mean)

usethis::use_data(fraser_discharge_mean,
                  overwrite = TRUE)

# If change then update plot.pacea_index()
attr(fraser_discharge_peak, "axis_name") <- expression(paste(plain(Fraser) *
    " " * plain(River) * " " * plain(discharge) * " " *
    plain(peak) * " " * plain(of) * " " * plain(daily) * " " * plain(values) *
    ", " * m^3 * s^-1))

plot.pacea_index(fraser_discharge_peak, value = "value")

fraser_discharge_peak

fraser_discharge_peak %>% tail()

summary(fraser_discharge_peak)

plot(fraser_discharge_peak)

usethis::use_data(fraser_discharge_peak,
                  overwrite = TRUE)

# Was going to add this to the help but couldn't find it again on website so
#just leaving here (also wasn't sure if it should be "flow continuous" or just
#"flow" and then "continuouse manual".
##' Type        Operation schedule       Gauge type
##' 1912-1949   Flow Continuous	         Manual
##' 1950-1995   Flow Continuous          Recorder
##' 1996-2025   Flow & Level Continuous  Recorder
