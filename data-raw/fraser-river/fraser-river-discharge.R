# From Chris Rooper in Issue #57, then adapting below. Just step through line by
# line like for indices.
# Fraser river discharge at Hope monthly values.
# Didn't fully re-run this all from scratch after reorganising (9/7/25), but it
# adds on to the existing saved objects now.

# Can only get last 18 months of values. So now am downloading the 6 months
# before what's in pacea up to the current today. Need need to just extend
# original since updating on 2025-10-15 can't
# get the early 2024 values, so need to stick with what we have in pacea
# already. Put in a check to see if data had got updated, and it turns out it
# was for June 2025, so doing some manual updating for that month.

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
# 2024. So changing this to download everything from the 1st of the month after
# what is in pacea, then can run the same code each time to update. Looks like
# no further quality control happens, as all values are listed as 'Final'. Maybe
# should redo the final, say, six months each time and check that they match.


# No need to rerun this for 2024 to 2025-07-09:

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
# 2025-07-09, can tell as date is in the filename; does cover 2024-01-01 even
# though 18 months on):

# This is 2024-01-01 to 2025-07-09, but need rest of July to get the full
# month. So keeping this, then continuing on below, by copying this example.
# SO raw_2024 etc. is really 2024 and Jan-Jun of 2025. Then continuing
# below. Don't need to rerun this as it's already in pacea.

if(FALSE){     # Change if want to rerun, but should not have to)
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

  } else {     # if old data not created above, just append 2024 (and some of
               # 2025) to what's in pacea. Shouldn't need (hence all this is
               # within the if(FALSE).

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
}

# 2025-10-15 Going forwards will redownload pacea's last six months of data all the
# way up to today. Do the wrangling. Check the pacea values agree with new
# calculautions (website implies no changes will get made, but just in case).

fraser_discharge_peak %>% tail()

fraser_discharge_mean %>% tail()

# On website enter the 1st of first month shown there (as tail shows 6 rows, so
# 6 months that we're redoing); I think both data sets should be the same. Get
# data up to today. So look at this and then change dates, and change to just
# 'Discharge (daily mean values)' with nothing for second axis. (The link from
# 2024 did reset other settings I think). Actually, when you download you get to
# choose anyway.

# https://wateroffice.ec.gc.ca/report/real_time_e.html?stn=08MF005&mode=Table&startDate=2024-01-01&endDate=2024-12-31&prm1=46&y1Max=&y1Min=&prm2=47&y2Max=&y2Min=
# and then download and unzip.

# https://wateroffice.ec.gc.ca/report/real_time_e.html?stn=08MF005&mode=Table&startDate=2025-01-01&endDate=2025-10-15&prm1=6&y1Max=&y1Min=&prm2=-1&y2Max=&y2Min=
raw_latest <- read.csv("08MF005_QRD_20251015T2202.csv",
                       header = TRUE
                       skip = 9) %>%
  as_tibble()

raw_latest
raw_latest %>% tail()

daily_latest <- mutate(raw_latest,
                       year = lubridate::year(lubridate::ymd_hms(Date..PST.)),
                       month = lubridate::month(lubridate::ymd_hms(Date..PST.))) %>%
  rename(discharge = Value..m..s.)

daily_latest
daily_latest %>% tail()

final_day <-
  lubridate::ceiling_date(lubridate::ymd_hms(daily_latest$Date..PST.[nrow(daily_latest)]))
final_day           # should have today or yesterday at the end (not sure
                    # how they calculated the mean)

monthly_latest <- summarise(group_by(daily_latest,
                                     year,
                                     month),
                            year = unique(year),
                            month = unique(month),
                            mean_for_month = mean(discharge),   # mean daily value
                            max_for_month = max(discharge),     # max daily value
                            num_days = n()) %>%
  ungroup()
monthly_latest   # Can see no missing days, expect (usually) final month, so
                 # remove that one if final dates isn't end of a month

if(final_day != ceiling_date(final_day, unit = "month") - days(1)){
   monthly_latest <- monthly_latest[-nrow(monthly_latest), ]
}

monthly_latest <- select(monthly_latest,
                         -"num_days")
monthly_latest

# Now append new values to end of existing pacea data

# Mean:

monthly_latest_mean <- select(monthly_latest,
                              -max_for_month) %>%
  rename(value = mean_for_month)

fraser_discharge_mean_new <- rbind(fraser_discharge_mean,
                                   monthly_latest_mean) %>%   # can have duplicates
  dplyr::distinct()                                         # remove duplicates

  # Check there are no duplicates for any month year combination
fraser_discharge_mean_new_check <- summarise(group_by(fraser_discharge_mean_new,
                                                      year,
                                                      month),
                                             num = n())
expect_equal(max(fraser_discharge_mean_new_check$num),
             1)

#  Failed, looks like June did get updated
if(TRUE){       # Look into if get a fail there, need to do manually
  filter(fraser_discharge_mean_new_check, num >1)
  filter(fraser_discharge_mean_new, year == 2025)

  # Had to reload and calculate daily_2024 from above. In future this will
  # change to the more recent file, so might need raw_latest_previous or something.
  daily_2024_duplicate <- filter(daily_2024,
                                 year == 2025,
                                 month == 6)
  daily_2024_duplicate

  daily_latest_duplicate <- filter(daily_latest,
                                   year == 2025,
                                   month == 6)
  daily_latest_duplicate

  plot(daily_2024_duplicate$discharge, type = "o")
  points(daily_latest_duplicate$discharge, pch = 16, col = "red")

  # Looks like a constant difference:
  daily_2024_duplicate$discharge -  daily_latest_duplicate$discharge
  #  [1]    0    0  -80 -140 -140 -130 -120 -130 -120 -130 -130 -130
  # [13] -130 -120 -130 -130 -130 -130 -130 -130 -120 -120 -120 -120
  # [25] -120 -120 -120 -120 -110 -120

  # So not quite

  # Presume the latest values are the best ones then, so replace previous
  # pacea ones with those.

  # So remove the original value, just do manually to check
  fraser_discharge_mean_new_2 <- filter(fraser_discharge_mean_new,
                                        !(year == 2025 &
                                          month == 6 &
                                          value < 5300))  # == is too exact
  expect_equal(nrow(fraser_discharge_mean_new),
               nrow(fraser_discharge_mean_new_2) + 1)
  fraser_discharge_mean_new_2 %>% tail()

  fraser_discharge_mean <- fraser_discharge_mean_new_2
}

# And for peak:
monthly_latest_peak <- select(monthly_latest,
                              -mean_for_month) %>%
    rename(value = max_for_month)

fraser_discharge_peak_new <- rbind(fraser_discharge_peak,
                                   monthly_latest_peak) %>%   # can have duplicates
  dplyr::distinct()                                         # remove duplicates

# Check there are no duplicates for any month year combination
fraser_discharge_peak_new_check <- summarise(group_by(fraser_discharge_peak_new,
                                                      year,
                                                      month),
                                             num = n())
expect_equal(max(fraser_discharge_peak_new_check$num),
             1)

# Need to use the latest version of downloaded data, again for June 2025:
#  Failed, looks like June did get updated
if(TRUE){       # Look into if get a fail there, need to do manually
  filter(fraser_discharge_peak_new_check, num >1)
  filter(fraser_discharge_peak_new, year == 2025)

  # So from the same cause as above. Not recalculating daily_2024_duplicate
  # etc. here

  # Presume the latest values are the best ones then, so replace previous
  # pacea ones with those.

  # So remove the original value (which comes first after the rbind), just do manually to check
  fraser_discharge_peak_new_2 <- filter(fraser_discharge_peak_new,
                                        !(year == 2025 &
                                          month == 6 &
                                          value < 7000))  # == is too exact
  expect_equal(nrow(fraser_discharge_peak_new),
               nrow(fraser_discharge_peak_new_2) + 1)
  fraser_discharge_peak_new_2 %>% tail()

  fraser_discharge_peak <- fraser_discharge_peak_new_2
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

