# Code to download and save buoy temperature into pacea. Resulting file of over
#  170,000 sst's is only 371 kb, so fine to not move into pacea-data.
#  Adding NAs for missing dates gives 199,000 rows (still 170,000 sst's).
# Adapting from Andrea Hilborn's code in andrea-code/.
# 28/8/23: Had originally kept the DFO data when there was ECCC also, but turns
#  out the ECCC is the only one being updated for some of them (e.g. some had
#  nothing for 2023, but Andrea's plot did), so switching
#  that. So some of the 170,000 type numbers may bump up a lot with this change.

# 2023-11-09: See #26. Taking out the 5degC cut-off as overkill, so having just the
# 'at least one record every two hours' and 'at least 10 two-hour records per
# day' criteria for calculating daily means. AND redownloading data.

# Run line-by-line while developing. Can source to update data, check that
#  redownload_data = TRUE.

load_all()

library(rerddap)
library(dplyr)
library(lubridate)

# library(tibble)
# library(ggplot2)

redownload_data = TRUE         # FALSE while developing, TRUE to update.

# CIOOS flags for the DFO MEDS record, flags to include:

use_flags = c(4, 9, 11, 12, 13, 14, 15, 16)
# https://catalogue.cioospacific.ca/dataset/ca-cioos_b9c71eb2-b750-43d5-a50a-aee173916736
# Kellog et al. quality control description, which gives the flags: https://drive.google.com/file/d/1J6I8PFuDN0Ca-8wdjfmAWRmeylPGn_s4/view
# 16 is 'good'

# CIOOS buoy source, DFO had historical data so called DFO (still being updated though)

if(redownload_data){
  dfo_info <- rerddap::info("DFO_MEDS_BUOYS",
                            url = "https://data.cioospacific.ca/erddap/")
  saveRDS(dfo_info, "dfo_info.Rds")

  dfo_data_raw <- rerddap::tabledap(x = dfo_info,
                                    fields = c("time",
                                               "longitude",
                                               "latitude",
                                               "STN_ID",
                                               "SSTP",
                                               "SSTP_flags"))
  saveRDS(dfo_data_raw, "dfo_data_raw.Rds")
} else {
    dfo_data_raw <- readRDS("dfo_data_raw.Rds")     # A tabledap which is a tibble
}

dfo_data_raw      # 9.7 million rows

# Figuring out time zone conversions. Commenting out, but keeping for reference.
# orig_time <- dfo_data_raw$time[6000252]        # 7:20 am UTC
# orig_time                 # no time zone, but just characters (data are UTC)
# ymd_hms(orig_time)        # default assumes UTC: 7am in data becomes 7am UTC
# as.POSIXct(orig_time, format="%Y-%m-%dT%H:%M:%SZ")  # default assumes 7am in
#                                         # data is 7am PDT - wrong
# force_tz(ymd_hms(orig_time), "Canada/Pacific")     # 7am becomes 7am PDT wrong
# with_tz(ymd_hms(orig_time), "Canada/Pacific")      # 7am becomes 12am (or 11pm)
#                                         # depending on time of year, 12am in
#                                         # this example as DST
# with_tz(ymd_hms(orig_time), "Etc/GMT+8") # 7am becomes 11am day before
#                                          # it presumably should not then mess
#                                          # around with DST). Going with this.


# Filtering, using the flags, wrangling, etc. Quality control has already
#  occurred (see above reference, and the flags will get used here).
#  Losing the extra metadata of tabledap class (by using as_tibble()) as mutate
#  etc. didn't seem to work properly. Filtering first would be quicker (since
#  losing 6 million rows), but have to do as.numeric first. Converting UTC to
#  GMT -8 time zone (which is helpfully called GMT+8).

dfo_data <- as_tibble(dfo_data_raw) %>%
  mutate(time = with_tz(ymd_hms(time),
                        "Etc/GMT+8"),
         longitude = as.numeric(longitude),
         latitude = as.numeric(latitude),
         SSTP = as.numeric(SSTP),
         SSTP_flags = as.numeric(SSTP_flags),   # not as.factor due to NA's
         STN_ID = as.factor(STN_ID)) %>%
  rename(stn_id = STN_ID,
         sstp = SSTP,
         sstp_flags = SSTP_flags) %>%
  filter(!is.na(time),
         sstp > -10,                # Too cold
         longitude < -100,
         latitude < 60,
         sstp_flags %in% use_flags | is.na(sstp_flags))

# 4/4/25 (and 26/5/25, 14/10/25) get this, not sure if had that before:
#Warning message:
# There was 1 warning in `mutate()`.
# In argument: `time = with_tz(ymd_hms(time), "Etc/GMT+8")`.
# Caused by warning:
#  49469 failed to parse.


dfo_data       # 3.666 million rows when removing pre-1991 . Every few minutes has,
               #  a value, but that's for all stations
               # 3.768 million rows when not removing pre-1991. So 100,000 more rows.
               # Now removing 414 is.na(time). Strange - with_tz kept 414 in,
               # that I think were removed when using posixct. Dates seem fine
               # so keeping with with_tz.
               # 9/11/2023 update: 3.797 million rows
               # 29/1/2024 update: 3.811 million rows
               #  6/8/2024 update: 3.839 million rows
               #  20/9/2024 update: 3.845 million rows
               #  14/10/2025 update: 3.855 million rows

summary(dfo_data)   # Earliest is 1987, so not adding tons of data, yet not really worth
                    # excluding 1987-1991 for our purposes (Andrea did since
                    # looking at climatology)..
               # Latest is 2023-06-11 having just downloaded on 2023-06-14. OPP
               #  data below seems to be more recent.

# Issue #26 - criteria for excluding days with missing values. Discussion at
# IOS resulted in Charles suggesting :
#  - for hourly data require a measurement every 2 hours
#  - for 10 minute data require at least one every hour.
# Easier to just take the average in every two hour interval (midnight - 2am,
# 2am-4am, etc.), and only calculate daily means for days which have
# >= num_two_hour_intervals_required  two-hour intervals with data.

# lubridate way, after I'd figured out interval approach that have now deleted
# (commit ac09d9d and the subsequent one) because it would have taken five hours
# to run rather than < a second. Assign each time a two-hour-interval start time:
dfo_data_resolution_two_hours <- dfo_data %>%
  arrange(desc(latitude),
          longitude) %>%     # To order the stations sensibly
  select(-"sstp_flags") %>%
  mutate(two_hour_start = floor_date(time,
                                     unit = "2 hour"))

# Calculate mean of measurements within the same two-hour interval,
#  and assign each one a day
dfo_two_hourly_mean <- group_by(dfo_data_resolution_two_hours,
                                stn_id,
                                two_hour_start) %>%
  summarise(two_hour_mean_sst = mean(sstp)) %>%
  ungroup() %>%
  mutate(day = lubridate::date(two_hour_start))

# Calculate daily mean, also keep track of how many two-hour measurements in
# each day (no such quality control yet):
dfo_daily_mean_no_qc <- group_by(dfo_two_hourly_mean,
                                 stn_id,
                                 day) %>%
  summarise(daily_mean_sst = mean(two_hour_mean_sst),
            num_two_hourly_in_day = n()) %>%
  ungroup()

# table(dfo_daily_mean_no_qc$num_two_hourly_in_day) as of 2023-08-28 data
#     1      2      3      4      5      6      7      8      9     10     11
#  1091    898    775   1260   1935   1224   1427   3461   2852   4168   9761
#    12
#144083
# table(dfo_daily_mean_no_qc$num_two_hourly_in_day) as of 2023-11-09 data (download)
#     1      2      3      4      5      6      7      8      9     10     11
#  1091    899    778   1262   1936   1226   1432   3472   2863   4190   9818
#    12
#144630

# table(dfo_daily_mean_no_qc$num_two_hourly_in_day) as of 2025-10-14 data (download)
#    1    2     3      4      5      6      7      8      9     10     11    12
# 1150  922   792   1281   1949   1232   1444   3493   2898   4282  10014 146880



num_two_hour_intervals_required <- 10  # Number of two-hour intervals in a day
                                       # that must have sst values in order to
                                       # use the daily mean for that day.

dfo_daily_mean_enough_two_hours <- dfo_daily_mean_no_qc %>%
  filter(num_two_hourly_in_day >= num_two_hour_intervals_required)

# Daily mean values to use
dfo_daily_mean <- dfo_daily_mean_enough_two_hours %>%
  mutate(date = day,
         sst = daily_mean_sst) %>%
  select(date,
         stn_id,
         sst)                # reorder columns

dfo_daily_mean
max(dfo_daily_mean$date)
# 158,012 rows up to 2023-08-23
# 158,638            2023-11-09
# 159,407            2023-02-17  # max(dfo_daily_mean$date)
# 160,467            2024-07-30
# 161,166            2025-03-17
# 161,176            2025-03-17  # is max date again on 14/10/25, above line was
# earlier, but do have 10 more rows, though expect I wrote the 161,166 as the
# 'more rows' after the tibble displayed.
# i.e. older data not changed here, or being updated through time.

# Before doing two-hour quality control had less, not sure how many (can test by
#  changing num_two_hour_intervals_required)

dfo_daily_latest <- dfo_daily_mean %>%
  group_by(stn_id) %>%
  summarise(end_date = max(date))

dfo_daily_latest
# After doing the two-hour calculations above, on 2023-10-25 (before
# redownloading data), we had this, then the next one when I used
# lubridate::date() earlier:
# A tibble: 18 × 2
##    stn_id end_date
##    <fct>  <dttm>
##  1 C46004 2023-06-17 00:00:00
##  2 C46036 2023-08-23 00:00:00
##  3 C46131 2020-07-03 00:00:00
##  4 C46132 2022-05-14 00:00:00
##  5 C46134 2016-12-08 00:00:00
##  6 C46145 2023-08-21 00:00:00
##  7 C46146 2023-05-13 00:00:00
##  8 C46147 2023-08-23 00:00:00
##  9 C46181 2023-08-23 00:00:00
## 10 C46182 1991-11-05 00:00:00
## 11 C46183 2023-08-23 00:00:00
## 12 C46184 2023-08-23 00:00:00
## 13 C46185 2023-08-23 00:00:00
## 14 C46204 2023-08-23 00:00:00
## 15 C46205 2023-08-23 00:00:00
## 16 C46206 2022-04-10 00:00:00
## 17 C46207 2022-09-07 00:00:00
## 18 C46208 2023-08-03 00:00:00

## This is using date, dates match the ones above, but this is simpler and better.
## > dfo_daily_latest
## # A tibble: 18 × 2
##    stn_id end_date
##    <fct>  <date>
##  1 C46004 2023-06-17
##  2 C46036 2023-08-23
##  3 C46131 2020-07-03
##  4 C46132 2022-05-14
##  5 C46134 2016-12-08
##  6 C46145 2023-08-21
##  7 C46146 2023-05-13
##  8 C46147 2023-08-23
##  9 C46181 2023-08-23
## 10 C46182 1991-11-05
## 11 C46183 2023-08-23
## 12 C46184 2023-08-23
## 13 C46185 2023-08-23
## 14 C46204 2023-08-23
## 15 C46205 2023-08-23
## 16 C46206 2022-04-10
## 17 C46207 2022-09-07
## 18 C46208 2023-08-03


# > unique(dfo_data$stn_id)
# [1] C46206 C46004 C46184 C46036 C46208 C46205 C46207 C46181 C46204 C46145
#[11] C46183 C46182 C46185 C46146 C46131 C46147 C46132 C46134
# 83 Levels: C44131 C44137 C44138 C44139 C44140 C44141 C44142 C44143 ... WEL451

# Same length:
# expect_equal(length(unique_id_all_years), length(unique_id_after_1991))

# So the pre-1991 data adds another 100,000 rows (still multiple per day), but
#  no new stations. Worth keeping in for our purposes.



# This outlier has gone away now (strangely, since I fixed time zones). Keeping
# for now, but can delete when have plotting functions. TODO
# Just looking at first buoy, looks like an outlier early on (pre-1991). Should
#  analyse these once have plotting functions nicely done.
## buoy_1 <- filter(dfo_daily_mean, stn_id == "C46207")
## plot(buoy_1$date, buoy_1$sst, type = "o")
## which(buoy_1$sst > 20)   # 254  # This did give a value one time
## buoy_1[250:260, ]
## buoy_1_lm <- lm(buoy_1$sst ~ buoy_1$date)
## abline(buoy_1_lm)    # But need to have complete years (i.e. not start in spring and
                     # finish in fall), so can't be this simplistic.

# So have dfo_daily_mean which has all buoys.



# From Andrea, to keep all buoys that were older and went to 'relatively
#  present'.
# List of ones that she would get rid of, could be old naming convention, some
#  might only be a year. See metadata fields. Some might be useful for me (she's
#  interested more in 1991-2020 climatology and ongoing data), so I could maybe
#  look at. I want to not just look at 1991 onwards, so may need to do my own filtering.

buoy_list <- c("MEDS107",
               "MEDS097",
               "MEDS106",
               "MEDS102",
               "MEDS104",
               "MEDS095",
               "MEDS114",
               "MEDS111",
               "MEDS112",
               "MEDS115",
               "MEDS004",
               "MEDS113",
               "MEDS116",
               "MEDS118",
               "MEDS117",
               "MEDS121",
               "MEDS122",
               "MEDS123",
               "MEDS124",
               "MEDS001",
               "MEDS100",
               "MEDS108",
               "MEDS126",
               "MEDS226",
               "MEDS259",
               "MEDS257",
               "MEDS273",
               "MEDS503W",
               "MEDS211",
               "MEDS213",
               "MEDS502W",
               "MEDS285",
               "C46182",
               "MEDS317",
               "MEDS336",
               "MEDS103",
               "MEDS303",
               "C46138",
               "C46139",
               "MEDS350",
               "C46134")

# AH had
# sstdata = sstdata %>% filter(!(STN_ID %in% c("MEDS107",....[the above list]

filter(dfo_daily_mean,
       stn_id %in% buoy_list)    # 5477 out of 172,236 without two-hour calcs.
                                 # After two-hour calcs it's
                                 # 4661 out of 158,012.  Could come
                                 # back to at end after doing ECCC data.
unique(filter(dfo_daily_mean,
              stn_id %in% buoy_list)$stn_id)
# C46134 C46182

range(filter(dfo_daily_mean,
             stn_id == "C46134")$date)
# "2001-02-20" "2016-12-09"
# "2001-02-20 -08" "2016-12-08 -08"  # On 27/10/23 with floor_date()
# "2001-02-20" "2016-12-08"    # On 30/10/23 have this, now using lubridate::date()

# See 'Do a date test' below -- tried floor_date() above as it keeps track of
# time zones, unlike as.Date(), and then deciding on lubridate::date().

# So keep that one in (Andrea probably excluded as didn't give full climatology)
# Though filter(buoys_metadata, wmo_id == 46134) says start date of 1999-01-01.

range(filter(dfo_daily_mean,
             stn_id == "C46182")$date)
#  "1989-09-08" "1991-11-22"
# Less than 2 years of data, 30 years ago, so removing as likely not useful for
# anyone. And after two-hour and 5degC filtering, range is down to:
# "1989-11-29" "1991-11-05"

dfo_daily_mean <- filter(dfo_daily_mean,
                         stn_id != "C46182")


# OPP Buoys.
# Presumably Oceans Protection Plan. Makes sense, as starts in 2019.
# On 2023-06-14 redownloaded data and the latest value was only an hour beforehand!

if(redownload_data){
  opp_info <- info("ECCC_MSC_BUOYS",
                   url = "https://data.cioospacific.ca/erddap/")
  saveRDS(opp_info, "opp_info.Rds")

  opp_data_raw <- tabledap(x = opp_info,
                           fields = c("avg_sea_sfc_temp_pst10mts",
                                       # checked out the field with '1' after, no data
                                      "avg_sea_sfc_temp_pst10mts_data_flag",
                                      "avg_sea_sfc_temp_pst10mts_qa_summary",
                                      "date_tm",
                                      "latitude",
                                      "longitude",
                                      "avg_sea_sfc_temp_pst10mts_1_data_flag",
                                      "avg_sea_sfc_temp_pst10mts_1_qa_summary",
                                      # "result_time",
                                      "stn_nam",
                                      "time",
                                      "wmo_synop_id"))
  saveRDS(opp_data_raw, "opp_data_raw.Rds")
} else {
  opp_data_raw <- readRDS("opp_data_raw.Rds")
}

opp_data_raw   #  A tibble: 1,278,688 × 11. File size (of temp download file I
               #  think) is 119 Mb. First rows have NaN's.

# 14/10/25 the tibble is # A tibble: 2,174,473 × 12

# Andrea doesn't use the flags in these data, so not using here.
# But, we get the spurious large fluctations at the start of the data for C46304
# (opp_data_C463404 below and the plot).
# The only flags with values are
# > table(opp_data_raw$avg_sea_sfc_temp_pst10mts_qa_summary)
#
#    -10      -1       0      20     100
#  51257    3363     789   37167 1241984
# So, try keeping just flag 100 (92.5% of all of them, and these are 10-minute
# values so may actually not exclude many days of calculations).
# Doing below then keeping track of how it changes opp_daily_mean later.

# 14/10/25:
#    -10      -1       0      20     100
#  70900    5348    1264   44861 2017262

# Originally was keeping only
# the two buoys not in DFO data above, but now checking and using OPP when it is
# updated more recently than DFO.

# Standardising stn_id here to compare datasets
opp_data_raw$stn_id = as.factor(paste0("C",
                                       opp_data_raw$wmo_synop_id))

# Figure out stations in both data sets
dfo_and_opp_stn <- intersect(unique(dfo_daily_mean$stn_id),
                             unique(opp_data_raw$stn_id))
dfo_and_opp_stn   # seems to be 16 of the 17 in DFO data. Same: 14/10/25.

dfo_only_stn <- setdiff(unique(dfo_daily_mean$stn_id),
                             unique(opp_data_raw$stn_id))
dfo_only_stn    # 14/10/25: C46134

# Check the dates of those in both
dfo_ranges <- dfo_daily_mean %>%
  filter(stn_id %in% dfo_and_opp_stn) %>%
  group_by(stn_id) %>%
  summarise(start_dfo = lubridate::date(min(date)),
            end_dfo = lubridate::date(max(date)))
dfo_ranges

opp_ranges <- opp_data_raw %>%
  filter(stn_id %in% dfo_and_opp_stn) %>%
  group_by(stn_id) %>%
  mutate(time = with_tz(ymd_hms(time),
                        "Etc/GMT+8"),
         date = lubridate::date(time)) %>%
  summarise(start_opp = min(date, na.rm = TRUE),
            end_opp = max(date, na.rm= TRUE))
opp_ranges
## > opp_ranges # using date = as.Date(time), actually come out same as floor_date(...)
## # A tibble: 16 × 3
##    stn_id start_opp  end_opp
##    <fct>  <date>     <date>
##  1 C46004 2021-09-07 2023-06-18
##  2 C46036 2021-09-07 2023-08-28
##  3 C46131 2021-09-07 2023-08-28
##  4 C46132 2021-09-07 2023-08-28
##  5 C46145 2021-09-07 2023-08-28
##  6 C46146 2021-09-06 2023-05-14    # This is 2012-05-13 with lubridate::date(),
##  7 C46147 2021-09-07 2023-08-28    #  think it also changed to this with floor_date()
##  8 C46181 2021-09-07 2023-08-28
##  9 C46183 2021-09-07 2023-08-28
## 10 C46184 2021-09-07 2023-08-28
## 11 C46185 2021-09-07 2023-08-28
## 12 C46204 2021-09-07 2023-08-28
## 13 C46205 2021-09-07 2023-08-28
## 14 C46206 2021-09-07 2023-04-07
## 15 C46207 2021-09-07 2022-09-08
## 16 C46208 2021-09-07 2023-08-28

# 14/10/15 all 16 stations were up to date (data from today, all started
# 2021-09-07 as below)

dfo_opp_ranges <- left_join(dfo_ranges,
                            opp_ranges,
                            by = "stn_id") %>%
  relocate(stn_id,
           start_dfo,
           start_opp) %>%
  mutate(dfo_starts_first = (start_dfo < start_opp),
         dfo_ends_last = (end_dfo > end_opp),
         opp_later_by = end_opp - end_dfo)

as.data.frame(dfo_opp_ranges)
# They all have DFO starting strictly (<) first, so need DFO data for all
# these. One of them is 0 days difference. 14/10/25 - no 0's, shows all buoys
# have DFO starting before OPP, and DFO ending before OPP, so you need both data
# streams for all buoys.
filter(dfo_opp_ranges, dfo_ends_last == TRUE)

# So only three that have both ending on same day, but that looks to be because
# data has stopped coming in (as of 2023-08-28, they are June 2023, May 2023,
# and September 2022). So still makes sense to use OPP for those recent ones in
# case they come online again. After two-hour correction above for DFO, looks
# like end dates may be off by 1, but conclusion still holds. Think that's now
# fixed with the use of lubridate::date().
# 2023-11-09 - opp are all later now (and only two by 1 day, several 5).
# 2025-10-14 - opp all later by at least 211 days.

# So, want DFO data first, then add on OPP data after as either
#  (i) they are both 2-11 months ago, which is presumably something failing) - 3 buoys
#  (ii) OPP data is updated in real time, giving the 4-day lag for DFO data - 10 buoys
#  (iii) OPP data are 355, 470, and 1150 days more recent, so definitely need
#   that - 3 buoys
# Also need the DFO-only buoy, and the two OPP-only ones. May have incorrectly
#  used OPP in variable names when it should really all be ECCC.

# All start_opp are 2021-09-07, except one is a day earlier on 2021-09-06. So
# let's switch to opp on 2021-09-10 to ensure a full day of data and give a few
# days (see below - one buoy looked to have big daily variation straight away,
# maybe it was 'settling down'?). C46131 will
# still have a gap with no data, but less so than presently have.

switch_dfo_to_opp <- lubridate::date("2021-09-10")

# Adapting what originally had to now calc daily sst for all opp stations that
# we want, including the overlapping ones. Filtering based on ...qa_summary also
# (keeping track of resulting influence on opp_daily_mean later).
opp_data <- as_tibble(opp_data_raw) %>%
  filter(stn_id %in% c(dfo_and_opp_stn, as.factor(c("C46303", "C46304"))),
         avg_sea_sfc_temp_pst10mts_qa_summary == 100) %>%
  mutate(time = with_tz(ymd_hms(time),
                        "Etc/GMT+8"),
         longitude = as.numeric(longitude),
         latitude = as.numeric(latitude),
         stn_id = as.factor(stn_id),
         sst = as.numeric(avg_sea_sfc_temp_pst10mts)) %>%
  select(time,
         stn_id,
         sst) %>%
  filter(!is.na(time),
         !is.na(sst),
         sst > -50,    # There were 9,655 of with -51.3 and 6 with -51.2, presumably NA
         sst != 49.3)  # One value of that.

opp_data # A tibble: 569,537 × 3, was 346,659 with only C46303 and C46304
         # 14/10/25: 923,081 x 3, with ~250,000 each of  C46303 and C46304 but
         # also ~420,000 of all the rest.
summary(opp_data)

# Both these still account for time zone:
pull(opp_data, time)[1]
# "2019-09-30 16:00:00 -08"
pull(dfo_data, time)[1]
# "1987-09-20 15:13:00 -08"

opp_data_full_range <- opp_data %>%
   group_by(stn_id) %>%
   summarise(start_date = min(time),
             end_date = max(time))
sort(opp_data_full_range$end_date)   # Now this gives 18 buoys.
max(opp_data_full_range$end_date)
# "2023-08-28 09:00:00 -08"
# So latest measurements were just over an hour before I downloaded them!
# 2024-02-20 it was 10 minutes ago!

# For all buoys (in above, commenting out: # filter(wmo_synop_id %in% c("46303",
#"46304")) %>%), though these now include all Canadian probably. Can look into
#further if needed, but a bit time consuming; these are the end dates (not
#updating this):
##  [1] "2021-10-19 08:05:00 -08" "2021-11-06 00:05:00 -08"
##  [3] "2021-11-08 11:34:00 -08" "2022-06-21 13:50:00 -08"
##  [5] "2022-09-08 13:10:00 -08" "2022-09-20 12:05:00 -08"
##  [7] "2022-09-27 13:05:00 -08" "2022-11-30 21:05:00 -08"
##  [9] "2022-12-14 18:05:00 -08" "2022-12-19 17:05:00 -08"
## [11] "2023-05-13 20:15:00 -08" "2023-06-11 16:20:00 -08"
## [13] "2023-06-12 15:15:00 -08" "2023-06-14 14:05:00 -08"
## [15] "2023-06-14 14:15:00 -08" "2023-06-14 14:15:00 -08"
## [17] "2023-06-14 14:20:00 -08" "2023-06-14 14:20:00 -08"
## [19] "2023-06-14 14:25:00 -08" "2023-06-14 14:25:00 -08"
## [21] "2023-06-14 14:25:00 -08" "2023-06-14 14:25:00 -08"
## [23] "2023-06-14 15:00:00 -08" "2023-06-14 15:05:00 -08"
## [25] "2023-06-14 15:05:00 -08" "2023-06-14 15:05:00 -08"
## [27] "2023-06-14 15:05:00 -08" "2023-06-14 15:05:00 -08"
## [29] "2023-06-14 15:05:00 -08" "2023-06-14 15:05:00 -08"
## [31] "2023-06-14 15:05:00 -08" "2023-06-14 15:05:00 -08"
## [33] "2023-06-14 15:05:00 -08" "2023-06-14 15:05:00 -08"
## [35] "2023-06-14 15:05:00 -08" "2023-06-14 15:05:00 -08"
## [37] "2023-06-14 15:05:00 -08" "2023-06-14 15:05:00 -08"
## [39] "2023-06-14 15:05:00 -08" "2023-06-14 15:05:00 -08"
## [41] "2023-06-14 15:05:00 -08" "2023-06-14 15:10:00 -08"
## [43] "2023-06-14 15:10:00 -08" "2023-06-14 15:10:00 -08"
## >

# Checked the lats and lons, which are exact so haven't included now
# range(filter(opp_data, stn_id == "46303")$longitude)  # -123.43 -123.43
# range(filter(opp_data, stn_id == "46304")$longitude)  # -123.357 -123.357

# range(filter(opp_data, stn_id == "46303")$latitude)  # 49.025 49.025
# range(filter(opp_data, stn_id == "46304")$latitude)  # 49.30167 49.30167

# attr(opp_data$time,"tzone") <- "UTC" # AH: KEEPING PST FOR NOW FOR BETTER DAY AVGS
#  AE: there is a -08 coded in the times (it doesn't always show up in tibble
#  view), so not adding.

# Want to look at plot of fine-scale data:
opp_data_example <- filter(opp_data,
                           stn_id == "C46208")[1:150, ]   # Every hour
par(mfrow = c(2,1))
plot(opp_data_example$time,
     opp_data_example$sst,
     type = "o")

# Different raw resolution, every ten minutes
opp_data_example_2 <- filter(opp_data,
                             stn_id == "C46303")[1:150, ]
plot(opp_data_example_2$time,
     opp_data_example_2$sst,
     type = "o")
par(mfrow = c(1,1))

# Can calculate daily variation
opp_daily_range <- opp_data %>%
  mutate(day = lubridate::date(time)) %>%
  group_by(date = day,
           stn_id) %>%
  summarise(sst_abs_range = diff(range(sst))) %>%
  ungroup()

opp_daily_range

# > opp_daily_range   # with day = as.Date(time) above
## # A tibble: 11,638 × 3
##    date       stn_id sst_abs_range
##    <date>     <fct>          <dbl>
##  1 2019-10-01 C46303         8
##  2 2019-10-01 C46304        10.8
##  3 2019-10-02 C46303         6.4
##  4 2019-10-02 C46304         8.5

## This is correct, but now using one after.
## opp_daily_range   # with day = floor_date(time, unit = "day") above
## # A tibble: 11,634 × 3
##    date                stn_id sst_abs_range
##    <dttm>              <fct>          <dbl>
##  1 2019-09-30 00:00:00 C46303          5.5
##  2 2019-09-30 00:00:00 C46304          8
##  3 2019-10-01 00:00:00 C46303          7.4
##  4 2019-10-01 00:00:00 C46304          9.9

## This is current, using lubridate::date() above:, so now has dates that are correct.
## > opp_daily_range
## # A tibble: 11,634 × 3
##    date       stn_id sst_abs_range
##    <date>     <fct>          <dbl>
##  1 2019-09-30 C46303          5.5
##  2 2019-09-30 C46304          8
##  3 2019-10-01 C46303          7.4
##  4 2019-10-01 C46304          9.9


# Do a date test:
opp_daily_range_test <- opp_data %>%
  mutate(day_from_as.Date = as.Date(time),
         day_from_floor_date = floor_date(time,
                                          unit = "day"),
         day_from_lubridate_date = lubridate::date(time))
opp_daily_range_test[1:10, ]  %>% select(-c("stn_id", "sst")) %>% as.data.frame()
# Clearly shows that as.Date is giving the date in UTC, not the local date.
#  So going back and changing as.Date where appropriate, originally to
#  floor_date but lubridate::date() is good. Need to pull a single element to
#  check time zone:
pull(opp_daily_range_test, day_from_as.Date)[1]
#  "2019-10-01"     # bumped it to UTC
pull(opp_daily_range_test, day_from_floor_date)[1]
# "2019-09-30 -08"
pull(opp_daily_range_test, day_from_lubridate_date)[1]
# [1] "2019-09-30"   # So doesn't have a time zone (that's fine), but has not
# switched to UTC, so is correct.


# Having fixed all that, this now correctly keeps track of time zone:
pull(opp_daily_range, date)[1]
# "2019-09-30 -08"  whereas with as.Date used it was incorrectly:



# opp_daily_range gives 10.8 for C46304 [doesn't seem to now], for first day (and still does with keeping only 100
# for qa). And many other large values. Look into
# the fine-scale data: (at IOS we figured out the sensor was probably turned on
# while still on the deck). Deal with after doing daily calculations with the
# two-hour criteria, here work out the day-stn_id combinations to not use:

# Lots of the following plots and comments were based on excluding > 5, now doing 9.8 to just have
# one excluded (and not break the code).
opp_exclude_daily_range_large_than <- 9.8      #  exclude days with larger daily
                                        #  ranges than this (degC).
opp_exclude_large_daily_range <- filter(opp_daily_range,
                                        sst_abs_range > opp_exclude_daily_range_large_than) %>%
  select(-c("sst_abs_range")) %>%
  cbind("large_daily" = TRUE)
# Only 104 values with cutoff at 5, but see Andrea's analyses in #26. Then use later to exclude these data-stn_id
# combinations. But 72 are 46303 (South Georgia Strait):
opp_exclude_large_daily_range %>% as.data.frame() %>% summary()

# 46303 (S Georgia Strait)
opp_data_C46303 <- filter(opp_data,
                           stn_id == "C46303")

plot(opp_data_C46303$time[1:1000],
     opp_data_C46303$sst[1:1000],
     type = "o",
     xlab = "Time",
     ylab = "SST")

plot(opp_data_C46303$time,
      opp_data_C46303$sst,
      type = "l",
      xlab = "Time",
      ylab = "SST")

# Want to flag then colour-code the days with big fluctuations, and zoom in to
# look at the 10-minute data in detail.
opp_data_C46303_with_fluct <- mutate(opp_data_C46303,
                                     date = lubridate::date(time))

opp_data_C46303_with_fluct_flagged <- left_join(opp_data_C46303_with_fluct,
                                                opp_exclude_large_daily_range,
                                                by = c("stn_id",
                                                       "date")) %>%
  mutate(large_daily = (!is.na(large_daily) & (large_daily))) %>%
  mutate(col = as.factor(ifelse(large_daily, "red", "black")))

plot(opp_data_C46303_with_fluct_flagged$time[1:4000],
     opp_data_C46303_with_fluct_flagged$sst[1:4000],
     type = "p",
     xlab = "Time",
     ylab = "SST",
     col = opp_data_C46303_with_fluct_flagged$col[1:4000],
     pch = 20)
#     xlim = c(ymd("2019-09-30"), ymd("2020-09-30")))  HERE not working, want to
#     plot one year
# That plot shows the early days that would get filtered out (but not the first
# day, but that will later since it starts at 4pm. But also shows not obvious
# what value for fluctuation to use.

# This is 46304 (Entrance English Bay), the first one I'd looked at with the
# largest range of all, but could have a big range from river outflow.
opp_data_C46304 <- filter(opp_data,
                           stn_id == "C46304")

plot(opp_data_C46304$time[1:1000],
     opp_data_C46304$sst[1:1000],
     type = "o",
     xlab = "Time",
     ylab = "SST")

# Plotting full time series (170,000 points) shows how hard it will be to
# manually exclude some, though looks like a few similar patterns of the
# temperatures showing large fluctuations after a no-data time.
plot(opp_data_C46304$time,
      opp_data_C46304$sst,
      type = "l",
      xlab = "Time",
      ylab = "SST")


# Looking into large daily fluctuations, but this is before doing the two-hour
# quality control. Need to look at this in conjuction with final
# values. Could plot max and min
hist(opp_daily_range$sst_abs_range, xlab = "Daily range")
one_buoy <- filter(opp_daily_range, stn_id == "C46304")
plot(one_buoy$date, one_buoy$sst_abs_range, type = "o")

h = hist(opp_daily_range$sst_abs_range, xlab = "Daily range")
tibble(midpoints = h$mids, counts = h$counts)

# Not sure what this was for:
#Calc is not quite right I think, this seems strange, though it's a time
# zone thing - these may still be in UTC.
# min(filter(opp_data_C46304, as.Date(time) == "2019-10-01")$sst)
# max(filter(opp_data_C46304, as.Date(time) == "2019-10-01")$sst)
# filter(opp_data_C46304, as.Date(time) == "2019-10-01") %>% as.data.frame()

# Overlay the daily ranges (won't see the small ones). Not working, don't worry
# lines(filter(opp_daily_range, stn_id == "C46304")$date,
#       filter(opp_daily_range, stn_id == "C46304")$sst_abs_range,
#       col = "red")


# Was going to check for anomalous values at start of time series later after
# calculating daily values, but the anomalous ones 10-minute ones above kind of
# get averaged out to give a reasonable daily average. So needs to be based on
# the opp_daily_range of 10-minute values, not on how the daily average looks.


# Repeating above DFO calcs for OPP data

opp_data_resolution_two_hours <- opp_data %>%
  mutate(two_hour_start = floor_date(time,
                                     unit = "2 hour"))

# Calculate mean of measurements within the same two-hour interval,
#  and assign each one a day
opp_two_hourly_mean <- group_by(opp_data_resolution_two_hours,
                            stn_id,
                            two_hour_start) %>%
  summarise(two_hour_mean_sst = mean(sst)) %>%
  ungroup() %>%
  mutate(day = lubridate::date(two_hour_start))

# Calculate daily mean, also keep track of how many two-hour measurements in
# each day (no quality control yet):
opp_daily_mean_no_qc <- group_by(opp_two_hourly_mean,
                                 stn_id,
                                 day) %>%
  summarise(daily_mean_sst = mean(two_hour_mean_sst),
            num_two_hourly_in_day = n()) %>%
  ungroup()

table(opp_daily_mean_no_qc$num_two_hourly_in_day)
# Before lubridate::date(), prob with as.Date()
#    1     2     3     4     5     6     7     8     9    10    11    12
#  118    94    74    73    64    50    28    44    53    56   222 10949
# With lubridate::date(), so more exact I think. Lost a few more than before if
# <10 is cut-off. But still not throwing much out.
#    1     2     3     4     5     6     7     8     9    10    11    12
#  147   125   105    94    90    71    66    74    88   124   292 10358

opp_daily_mean_enough_two_hours <- opp_daily_mean_no_qc %>%
  filter(num_two_hourly_in_day >= num_two_hour_intervals_required)

# Daily mean values to use
opp_daily_mean <- opp_daily_mean_enough_two_hours %>%
  mutate(date = day,
         sst = daily_mean_sst) %>%
  left_join(opp_exclude_large_daily_range,
            by = c("stn_id",
                   "date")) %>%
  filter(is.na(large_daily)) %>%         # Remove the large_daily = TRUE days
  select(date,
         stn_id,
         sst)                # keep and reorder columns
opp_daily_mean
# 11,227 rows up to 2023-08-27. Now 10,671 after taking out flags and two-hour
# quality control and remove large daily fluctuations (> 5 degC)
# Now 10,669 after lubridate::date().

# Before doing two-hour quality control had less, not sure how many (can test by
#  changing num_two_hour_intervals_required)

# Check on the one that had big fluctations earlier:
one_buoy_mean <- filter(opp_daily_mean,
                        stn_id == "C46304")
plot(one_buoy_mean$date, one_buoy_mean$sst, type = "o")   # looks okay
min(one_buoy_mean$date)
# Before the two-hour calcs, we had
min(one_buoy$date)
# So that first day is now screened out.

opp_daily_latest <- opp_daily_mean %>%
  group_by(stn_id) %>%
  summarise(end_date = max(date))

opp_daily_latest
# After doing the two-hour calculations above, on 2023-10-25 (before
# redownloading data), before using lubridate::date() we have:
# A tibble: 18 × 2
##    stn_id end_date
##    <fct>  <dttm>
##  1 C46004 2023-06-17 00:00:00
##  2 C46036 2023-08-27 00:00:00
##  3 C46131 2023-08-27 00:00:00
##  4 C46132 2023-08-27 00:00:00
##  5 C46145 2023-08-13 00:00:00
##  6 C46146 2023-05-12 00:00:00  # This becomes a day earlier because final day
##  is missing three two-hour chunks: filter(opp_data, stn_id == "C46146") %>%
##  tail(n = 30) %>% as.data.frame()  [actually had it here as 2023-05-13, but
##  just hadn't updated I think]
##  7 C46147 2023-08-27 00:00:00
##  8 C46181 2023-08-27 00:00:00
##  9 C46183 2023-08-27 00:00:00
## 10 C46184 2023-08-27 00:00:00
## 11 C46185 2023-08-27 00:00:00
## 12 C46204 2023-08-27 00:00:00
## 13 C46205 2023-08-27 00:00:00
## 14 C46206 2022-11-30 00:00:00
## 15 C46207 2022-09-07 00:00:00
## 16 C46208 2023-08-03 00:00:00
## 17 C46303 2023-08-19 00:00:00
## 18 C46304 2023-08-27 00:00:00

## Now have, with lubridate_date():
## > opp_daily_latest
## # A tibble: 18 × 2
##    stn_id end_date
##    <fct>  <date>
##  1 C46004 2023-06-17
##  2 C46036 2023-08-27
##  3 C46131 2023-08-27
##  4 C46132 2023-08-27
##  5 C46145 2023-08-13
##  6 C46146 2023-05-11
##  7 C46147 2023-08-27
##  8 C46181 2023-08-27
##  9 C46183 2023-08-27
## 10 C46184 2023-08-27
## 11 C46185 2023-08-27
## 12 C46204 2023-08-27
## 13 C46205 2023-08-27
## 14 C46206 2022-11-30
## 15 C46207 2022-09-07
## 16 C46208 2023-08-03
## 17 C46303 2023-08-19
## 18 C46304 2023-08-27

# End of repeating DFO calcs for opp data

opp_daily_mean # A tibble: 11,227 x 3 with data saved on 2023-08-23, run on
# 2023-10-25, with NO removal of flags (was 11,840 before the 2-hour quality control,
# may have had downloaded less then also)
# Redoing with keeping only flags of 100, results in opp_daily_mean tibble of
# size:
#  10,774 × 3, so keeping 100 flags only loses us 453 days, 4%.
# 10,671 × 3 when removing the 103 days that have daily fluctuations > 5degC
# 10,669 x 3 after using lubridate::date().
# 21,243 x 3 on 15/10/25

summary(opp_daily_mean)

expect_equal(colnames(dfo_daily_mean),
             colnames(opp_daily_mean))

# Could check the overlapping values agree? Maybe, and see emails from Andrea
# (26/10/2023), as quite tricky and probably not worthwhile.

# Check the dfo_only_stn still has only historical data. If this test fails then need
# to add the later data in (expect buoy is not going to be restarted). Else is okay
# to just filter all dfo_daily_mean data before swtich_dfo_to_opp.
expect_true(max((filter(dfo_daily_mean, stn_id == dfo_only_stn))$date) <
            switch_dfo_to_opp)

# opp_daily_mean has the two non-dfo stations that we need to keep, and then
# just use the values after switch_dfo_to_opp for the remaining stations.
opp_daily_mean_to_use <- bind_rows(filter(opp_daily_mean,
                                          !(stn_id %in% dfo_and_opp_stn)),
                                   filter(opp_daily_mean,
                                          stn_id %in% dfo_and_opp_stn,
                                          date >= switch_dfo_to_opp))

# Join together, filtering by the switch-over day, then fill in missing-sst dates with NAs, so plotting gives gaps
buoy_sst_new = full_join(filter(dfo_daily_mean,
                                date < switch_dfo_to_opp),
                         opp_daily_mean_to_use) %>%
  group_by(stn_id) %>%
  tidyr::complete(date = seq(from = min(date),
                             to = max(date),
                             by = "day")) %>%
  relocate(date) %>%
  droplevels() %>%
  ungroup()
                        # for complete, fill = list(sst = NA)) not needed since
                        # NA is default; grouping fills in the stn_ids. Did
                        # change the column order though, hence need relocate.

class(buoy_sst_new) <- c("pacea_buoy",
                         class(buoy_sst_new))

buoy_sst_new # 2023-11-09: 202,927
             # 2025-10-14: 216,285
buoy_sst     # 201,435 with same data as 28 August 2023, but having done the
             #  two-hour quality control on all the data. So only actually lose
             #  206 daily means, which seems fine and makes sense..
             # 201,641 on 28 August 2023 after adding in ECCC data instead of
             # DFO ones. Have added with the switch to ECCC values. Previously had 199,625 values
             # (165 extra are from updating in time) Adding days: 15 + 1131 + 450 + 15 + 45 + 45 +
             # 240 + 45 =~ 1986
             # 201,430 after two-hour resolution requirement and 5degC
             #  removal. Number of opp days that the 5degC requirement actually
             #  removed in the end was only 58:
             #  opp_exclude_large_daily_range %>% filter(date >
             #    switch_dfo_to_opp) %>% nrow()  # is 58
             # 201,426 after using lubridate::date()
             #  opp_exclude_large_daily_range %>% filter(date > switch_dfo_to_opp) %>% nrow()  # is 56 now

# Decide if changing them

buoy_sst_new %>% tail()
buoy_sst %>% tail()

summary(buoy_sst_new)
summary(buoy_sst)

# This update adds in
nrow(buoy_sst_new) - nrow(buoy_sst)
#  new daily means. Could use in commit message.
# 14/10/25 - 4096 new daily means (hadn't updated for
# Had some warnings (4/4/2025) so checking that past values haven't changed.
# buoy_sst_new_overlap <- group_by(buoy_sst_new,
#                                  date,
#                                  stn_id)
# buoy_sst_new_overlap %>% summarise(unique(sst))  # Need to work on.


stop("Do some quick thinking before updating.")  # Could add in code like for indices

buoy_sst <- buoy_sst_new

usethis::use_data(buoy_sst,
                  overwrite = TRUE)

# Andrea example code (off top of head) for looking at flags:
# sstdata %>%
#     ggplot(aes(x = time, y = SSTP, colour = as.factor(flaglayer))) +
#     geom_point() +
#     facet_wrap(~STN_ID)
