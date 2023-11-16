# Number of days that Mount Washington ski resort was open, according to
#  https://www.skicentral.com/mt.washington.html
# Including out of curiosity (related to El Nino?) and to develop ideas for a
#  phase plot function, though not saving the values in pacea, just doing some
#  draft code here. Also showed that can easily use Travis's anomaly functions
#  (just took the example code from the talk). Plotting ideas could be useful
#  for other data sets, no clear signal regarding SST or oni wrt ski season though.

# snow.zip from Robyn:
# Here is a summary of monthly total snowfall going back a couple of decades. I
# think this was hand entered data from ~1100m (Hawk station). Plus a little bit
# of R code I wrote to have a look at it.  I can't verify it's accuracy!
# Not citeable or completely trustworthy, so not committing.

# Run code line-by-line to update.

load_all()
library(dplyr)
library(tibble)
library(lubridate)

# Copied and pasted from above website, then saving below. Data aren't solely
#  related to environment, since opening and closing can depend on things like
#  recruitment (ski lift attendants, not herring) etc.
#
## Season 2023	12/09/22 to 04/12/23
## (124 days)
## Season 2022	12/12/21 to 04/03/22
## (112 days)
## Season 2021	Opened 12/04/20

## Season 2020	12/30/19 to 03/18/20
## (79 days)
## Season 2019	12/15/18 to 04/07/19
## (113 days)
## Season 2018	12/09/17 to 04/09/18
## (121 days)
## Season 2017	12/09/16 to 03/26/17
## (107 days)
## Season 2016	12/11/15 to 04/10/16
## (121 days)
## Season 2015	12/20/14 to 02/09/15
## (51 days)
## Season 2014	01/12/14 to 04/18/14
## (96 days)
## Season 2013	11/30/12 to 04/13/13
## (134 days)

# 2020 season close early due to covid.

# No data given for season 2021 closing - Andy skied on 25/3/21 (from Strava) so
# could put that as a minimum. Also 2021 season may have been different due to covid.

# year corresponds to the January; could maybe change that to be december, but
#  didn't open until 2014-01-12 for 'december-year' 2013

mtw_days <- tibble(year = 2013:2023,
                   days_open = c(134, 96, 51, 121, 107, 121, 113, NA, NA, 112,
                                 124),
                   opened = c("11/30/12", "01/12/14", "12/20/14", "12/11/15",
                              "12/09/16", "12/09/17", "12/15/18", "12/30/19", "12/04/20",
                              "12/12/21", "12/09/22"),
                   closed = c("04/13/13", "04/18/14", "02/09/15", "04/10/16",
                              "03/26/17", "04/09/18", "04/07/19", NA, NA,
                              "04/03/22", "04/12/23")) %>%   # 2020-03-18 set to
                                        # NA, since due to COVID. "03/25/21" was
                                        # still open (from Andy's Strava).
  mutate(opened = lubridate::mdy(opened),
         closed = lubridate::mdy(closed),
         opened_day = lubridate::yday(opened),
         closed_day = lubridate::yday(closed)) %>%
  mutate(opened_day = opened_day + 365 * (opened_day < 100))   # deals with 2014 season not
                                        # opening until January; need to tweak
                                        # this if dealing with leap years in any
                                        # update.

# Picking an example month for each of these
oni_fall <- filter(oni,
                   year >= 2012,
                   year < 2023,
                   month == 10)   # change this to see which is good


# Have checked they line up
plot(oni_fall$anomaly,
     mtw_days$opened_day)
     # type = "o")
plot(oni_fall$anomaly,
     mtw_days$days_open)


oni_winter <- filter(oni,
                   year >= 2013,
                   year < 2024,
                   month == 2)   # change this to see which is good
plot(oni_winter$anomaly,
     mtw_days$closed_day)


plot(oni_winter$anomaly,
     mtw_days$days_open)

mtw_days$closed - mtw_days$opened  # manually checked these match, ignoring the NA's


anom_dat <- calc_anom(oisst_month, time_period_return = "October", years_return = 2012:2022)

Shortest four seasons, take off a year to get the preceding October
rank(mtw_days$days_open)
plot(anom_dat, months.plot = "October", years.plot = c(2014, 2015, 2017, 2022) - 1)

Longest four seasons, again take off a year to get preceding October, and could
try other months:
plot(anom_dat, months.plot = "October", years.plot = c(2013, 2016, 2018, 2023) - 1)


# From Robyn, ski reports, though in very unusable formats (basically daily values):
# https://mountwashington.ca/newsletters/snowreports/

OLD:
plot(oni, xlim = c(lubridate::as_date("2011-01-01"), today()), lwd = 3)
par(new = TRUE)
plot(lubridate::as_date(mtw_days$year),
     mtw_days$days_open,
     type = "p",
     pch = 19,
     axes = FALSE,
     xlab = "",
     ylab = "")

axis(side = 4, at = pretty(range(mtw_days$days_open)))
mtext("MtW days open", side = 4, line = 3)
