# Code to download and save buoy temperature into pacea. Resulting file of over
#  170,000 sst's is only 371 kb, so fine to not move into pacea-data.
#  Adding NAs for missing dates gives 199,000 rows (still 170,000 sst's).
# Adapting from Andrea Hilborn's code in andrea-code/.
# 28/8/23: Had originally kept the DFO data when there was ECCC also, but turns
#  out the ECCC is the only one being updated for some of them (e.g. some had
#  nothing for 2023, but Andrea's plot did), so switching
#  that. So some of the 170,000 type numbers may bump up a lot with this change.


# Run line-by-line while developing. Can source to update data, check that
#  redownload_data = TRUE.

load_all()

library(rerddap)
library(dplyr)
library(lubridate)

# library(tibble)
# library(ggplot2)

redownload_data = FALSE         # FALSE while developing, TRUE to update.

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

  dfo_data_raw <- tabledap(x = dfo_info,
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

dfo_data       # 3.666 million rows when removing pre-1991 . Every few minutes has,
               #  a value, but that's for all stations
               # 3.768 million rows when not removing pre-1991. So 100,000 more rows.
               # Now removing 414 is.na(time). Strange - with_tz kept 414 in,
               # that I think were removed when using posixct. Dates seem fine
               # so keeping with with_tz.

summary(dfo_data)   # Earliest is 1987, so not adding tons of data, yet not really worth
                    # excluding 1987-1991 for our purposes (Andrea did since
                    # looking at climatology)..
               # Latest is 2023-06-11 having just downloaded on 2023-06-14. OPP
               #  data below seems to be more recent. TODO If really wanted more recent
               #  could switch to using OPP for some buoys, bit of work though.

# Issue #26 - criteria for excluding days with missing values. Discussion at
# IOS resulted in Charles suggesting :
#  - for hourly data require a measurement every 2 hours
#  - for 10 minute data require at least one every hour.
# Easier to just take the average in every two hour interval (midnight - 2am,
# 2am-4am, etc.), and only calculate daily means for days which have
# >= num_two_hour_intervals_required  two-hour intervals with data.

# lubridate way, after I'd figured out interval approach that have now deleted
# (commit ac09d9d and the subsequent one). Assign each time a two-hour-interval
# start time:
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
  mutate(day = floor_date(two_hour_start,
                          unit = "day"))

# Calculate daily mean, also keep track of how many two-hour measurements in
# each day (no quality control yet):
dfo_daily_mean_no_qc <- group_by(dfo_two_hourly_mean,
                                 stn_id,
                                 day) %>%
  summarise(daily_mean_sst = mean(two_hour_mean_sst),
            num_two_hourly_in_day = n()) %>%
  ungroup()

# table(dfo_daily_mean$num_two_hourly_in_day)
#     1      2      3      4      5      6      7      8      9     10     11
#  1091    898    775   1260   1935   1224   1427   3461   2852   4168   9761
#    12
#144083

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

# 158,012 rows up to 2023-08-23
# Before doing two-hour quality control had less, not sure how many (can test by
#  changing num_two_hour_intervals_required)

dfo_daily_latest <- dfo_daily_mean %>%
  group_by(stn_id) %>%
  summarise(end_date = max(date))

dfo_daily_latest
# After doing the two-hour calculations above, on 2023-10-25 (before
# redownloading data), we have:
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
       stn_id %in% buoy_list)    # 5477 out of 172,236.
                                 # After two-hour calcs it's
                                 # 4661 out of 158,012.  Could come
                                 # back to at end after doing ECCC data.
unique(filter(dfo_daily_mean,
              stn_id %in% buoy_list)$stn_id)
# C46134 C46182

range(filter(dfo_daily_mean,
             stn_id == "C46134")$date)
# "2001-02-20" "2016-12-09"
# So keep that one in (Andrea probably excluded as didn't give full climatology)
# Though filter(buoys_metadata, wmo_id == 46134) says start date of 1999-01-01.

range(filter(dfo_daily_mean,
             stn_id == "C46182")$date)
#  "1989-09-08" "1991-11-22"
# Less than 2 years of data, 30 years ago, so removing as likely not useful for
# anyone.

dfo_daily_mean <- filter(dfo_daily_mean,
                         stn_id != "C46182")

# OPP Buoys. Presumably Oceans Protection Plan. Makes sense, as starts in 2019.
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

# Andrea doesn't use the flags in these data, so not using here. Originally was keeping only
# the two buoys not in DFO data above, but now checking and using OPP when it is
# updated more recently than DFO.

# Standardising stn_id here to compare datasets
opp_data_raw$stn_id = as.factor(paste0("C",
                                       opp_data_raw$wmo_synop_id))

# Figure out stations in both data sets
dfo_and_opp_stn <- intersect(unique(dfo_daily_mean$stn_id),
                             unique(opp_data_raw$stn_id))
dfo_and_opp_stn   # seems to be 16 of the 17 in DFO data

dfo_only_stn <- setdiff(unique(dfo_daily_mean$stn_id),
                             unique(opp_data_raw$stn_id))
dfo_only_stn

# Check the dates of those in both
dfo_ranges <- dfo_daily_mean %>%
  filter(stn_id %in% dfo_and_opp_stn) %>%
  group_by(stn_id) %>%
  summarise(start_dfo = min(date),
            end_dfo = max(date))
dfo_ranges

opp_ranges <- opp_data_raw %>%
  filter(stn_id %in% dfo_and_opp_stn) %>%
  group_by(stn_id) %>%
  mutate(time = with_tz(ymd_hms(time),
                        "Etc/GMT+8"),
         date = as.Date(time)) %>%
  summarise(start_opp = min(date, na.rm = TRUE),
            end_opp = max(date, na.rm= TRUE))
opp_ranges

dfo_opp_ranges <- left_join(dfo_ranges,
                            opp_ranges,
                            by = "stn_id") %>%
  relocate(stn_id, start_dfo, start_opp) %>%
  mutate(dfo_starts_first = (start_dfo < start_opp),
         dfo_ends_last = (end_dfo >= end_opp),
         opp_later_by = end_opp - end_dfo)

as.data.frame(dfo_opp_ranges)
# They all have DFO starting strictly (<) first, so need DFO data for all these.
filter(dfo_opp_ranges, dfo_ends_last == TRUE)

# So only three that have both ending on same day, but that looks to be because
# data has stopped coming in (as of 2023-08-28, they are June 2023, May 2023,
# and September 2022). So still makes sense to use OPP for those recent ones in
# case they come online again.

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

switch_dfo_to_opp <- as.Date("2021-09-10")

# Adapting what originally had to now calc daily sst for all opp stations that
# we want, including the overlapping ones.
opp_data <- as_tibble(opp_data_raw) %>%
  filter(stn_id %in% c(dfo_and_opp_stn, "C46303", "C46304")) %>%
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
                                                          # First record is now correctly
                                                          #  2019-09-30 16:00:00, GMT-8
summary(opp_data)

## # Previously had this, only adding the two new buoys on (copying this above to
## # use it): TODO can delete once finished these updates
## opp_data <- as_tibble(opp_data_raw) %>%
##   filter(wmo_synop_id %in% c("46303", "46304")) %>%
##   mutate(time = with_tz(ymd_hms(time),
##                         "Etc/GMT+8"),
##          longitude = as.numeric(longitude),
##          latitude = as.numeric(latitude),
##          stn_id = as.factor(wmo_synop_id),
##          sst = as.numeric(avg_sea_sfc_temp_pst10mts)) %>%
##   select(time,
##          stn_id,
##          sst) %>%
##   filter(!is.na(time),
##          !is.na(sst))

#opp_data_posix    # was saved using posix
#filter(opp_data_raw, wmo_synop_id == "46303")[1, "time"]  # First record of both
                                                          # is midnight
                                                          # 2019-10-01, but
                                                          # posix assumes it's PST/PDT.
opp_data_full_range <- opp_data %>%
   group_by(stn_id) %>%
   summarise(start_date = min(time),
             end_date = max(time))
sort(opp_data_full_range$end_date)   # Now this gives 18 buoys.
# "2023-06-14 15:00:00 -08" "2023-06-14 15:10:00 -08"
# So latest measurements are just over an hour before I downloaded them!

# For all buoys (in above, commenting out: # filter(wmo_synop_id %in% c("46303",
#"46304")) %>%), though these now include all Canadian probably. Can look into
#further if needed, but a bit time consuming; these are the end dates:
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

# TODO come back to, see Issue, discuss with Andrea and Charles
# Want to look at plot of fine-scale data:
opp_data_example <- filter(opp_data,
                           stn_id == "C46208")[1:150, ]   # Looks better
                                        # resolution there

plot(opp_data_example$time,
     opp_data_example$sst,
     type = "o")

# Can calculate daily variation
opp_daily_range <- opp_data %>%
  mutate(time_day = as.Date(time)) %>%
  group_by(date = time_day,
           stn_id) %>%
  summarise(sst_abs_range = diff(range(sst))) %>%
  ungroup()

opp_daily_range

# Gives 10.8 for C46304, for first day. And many other large values. Look into
# the fine-scale data:
opp_data_C46304 <- filter(opp_data,
                           stn_id == "C46304")

plot(opp_data_C46304$time[1:200],
     opp_data_C46304$sst[1:200],
     type = "o",
     xlab = "Time",
     ylab = "SST")

# TODO calc is not quite right I think, this seems strange, though it's a time
# zone thing - these may still be in UTC.
min(filter(opp_data_C46304, as.Date(time) == "2019-10-01")$sst)
max(filter(opp_data_C46304, as.Date(time) == "2019-10-01")$sst)
filter(opp_data_C46304, as.Date(time) == "2019-10-01") %>% as.data.frame()

# Overlay the daily ranges (won't see the small ones). Not working, don't worry
lines(filter(opp_daily_range, stn_id == "C46304")$date,
      filter(opp_daily_range, stn_id == "C46304")$sst_abs_range,
      col = "red")


# Calculate daily mean.
opp_daily_mean <- opp_data %>%
  mutate(time_day = as.Date(time)) %>%
  group_by(date = time_day,
           stn_id) %>%
  summarise(sst = mean(sst)) %>%     # TODO (started above) Should check the data are
                                     # representative through the day. And for
                                     # DFO data.
  ungroup()

opp_daily_mean # A tibble: 11,840 × 3
summary(opp_daily_mean)

# Keep these as comments, prob don't need them and data filtering has since
# changed, and all makes sense now.

# Just manually saying which are unique (*), not duplicated in other data
#  though now removing C46182 above because it was only for 2 years, 30 years ago.
# unique(dfo_daily_mean$stn_id)
# C46004 C46036 C46131 C46132 C46134* C46145 C46146 C46147 C46181 C46182*
# C46183 C46184 C46185 C46204 C46205 C46206 C46207 C46208

# sort(unique(opp_daily_mean$wmo_synop_id))
# 46004 46036 46131 46132 46145 46146 46147 46181 46183 46184 46185 46204
# 46205 46206 46207 46208 46303* 46304*
# Those two * are the only ones we use in opp now. - Nope, now doing others.

expect_equal(colnames(dfo_daily_mean),
             colnames(opp_daily_mean))

# Should check the overlapping values agree? TODO



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
buoy_sst = full_join(filter(dfo_daily_mean,
                            date < switch_dfo_to_opp),
                     opp_daily_mean_to_use) %>%
  group_by(stn_id) %>%
  tidyr::complete(date = seq.Date(from = min(date),
                                  to = max(date),
                                  by = "day")) %>%
  relocate(date) %>%
  droplevels() %>%
  ungroup()
                        # for complete, fill = list(sst = NA)) not needed since
                        # NA is default; grouping fills in the stn_ids. Did
                        # change the column order though, hence need relocate.

class(buoy_sst) <- c("pacea_buoy",
                     class(buoy_sst))

buoy_sst     # 201,641 on 28 August 2023 after adding in ECCC data instead of
             # DFO ones. Have added with the switch to ECCC values. Previously had 199,625 values
             # (165 extra are from updating in time) Adding days: 15 + 1131 + 450 + 15 + 45 + 45 +
             # 240 + 45 =~ 1986

usethis::use_data(buoy_sst,
                  overwrite = TRUE)

# Remove erroneous buoys - shouldn't need as only using two extra ones from OPP
#data_buoy_full3 <- data_buoy_full %>%
#  group_by(name_key) %>%
#  mutate(numobs = n()) %>%
#  filter(numobs > 1) %>%
#  ungroup()
