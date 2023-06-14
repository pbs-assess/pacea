# Code to download and save buoy temperature into pacea. If too big then will
#  save into pacea-data repo. Adapting from Andrea Hilborn's
#  code in andrea-code/

# Run line-by-line while developing. To make plotting functions go back to
#  original code (I'll just delete those parts from here).

# TODO - buoys-metadata -> buoy-metadata

load_all()

library(rerddap)
library(dplyr)
library(ggplot2)
library(lubridate)
# library(tibble)

# library(rnaturalearthdata)
# library(rnaturalearthhires)
# library(rnaturalearth)
# library(stringr)

#theme_set(theme_bw())

# Andrea: all buoys are EC buoys. DFO has historical data (but it seems to
# be getting updated with a lag of only a few days), EC is recent data and do
# not have flags (she didn't filter by flags as the flags looked wrong). Some
# data will overlap and should be identical. TODO add to help.

# Andrea took out pre-1991 data to look at 1991-2020 climatology and compare
# with recent years. We should keep it all, which seems fine.

# TODO She used Pacific Time (not sure about time changes) to do daily averages, then
# maybe converted back to UTC. Maybe it makes sense to just work in PDT, tell R
# not to deal with time changes.


redownload_data = FALSE       # FALSE while developing, TRUE to update.

# CIOOS flags for the DFO MEDS record, flags to include:

use_flags = c(4, 9, 11, 12, 13, 14, 15, 16)
# https://catalogue.cioospacific.ca/dataset/ca-cioos_b9c71eb2-b750-43d5-a50a-aee173916736
# Kellog et al. quality control description, which gives the flags: https://drive.google.com/file/d/1J6I8PFuDN0Ca-8wdjfmAWRmeylPGn_s4/view
# 16 is 'good'

# CIOOS buoy source

# TODO rename with dfo in title

if(redownload_data){
  sst_info <- rerddap::info("DFO_MEDS_BUOYS",
                            url = "https://data.cioospacific.ca/erddap/")
  saveRDS(sst_info, "sst_info.Rds")

  sst_data_raw <- tabledap(x = sst_info,
                           fields = c("time",
                                      "longitude",
                                      "latitude",
                                      "STN_ID",
                                      "SSTP",
                                      "SSTP_flags"))
  saveRDS(sst_data_raw, "sst_data_raw.Rds")
} else {
    # sst_info <- readRDS("sst_info.Rds")    # Don't actually need again
    sst_data_raw <- readRDS("sst_data_raw.Rds")     # A tabledap which is a tibble
}

sst_data_raw      # 9.7 million rows

# Filtering, using the flags, wrangling, etc. Quality control has already
#  occurred (see above reference, and the flags will get used)
#  Losing the extra metadata of tabledap class (by using as_tibble()) as mutate
#  etc. didn't seem to work properly

sst_data <- as_tibble(sst_data_raw) %>%
  mutate(time = as.POSIXct(time,
                           format="%Y-%m-%dT%H:%M:%SZ"),
         longitude = as.numeric(longitude),
         latitude = as.numeric(latitude),
         SSTP = as.numeric(SSTP),
         SSTP_flags = as.numeric(SSTP_flags),   # not as.factor due to NA's
         STN_ID = as.factor(STN_ID)) %>%
  rename(stn_id = STN_ID,
         sstp = SSTP,
         sstp_flags = SSTP_flags) %>%
  filter(!is.na(time),
         # time >= as.POSIXct("1991-01-01T00:00:00"),
         sstp > -10,                # Too cold
         longitude < -100,
         latitude < 60,
         sstp_flags %in% use_flags | is.na(sstp_flags))

sst_data       # 3.666 million rows when removing pre-1991 . Every few minutes,
               # but is for all stations
               # 3.767 million rows when not removing pre-1991. So 100,000 more rows.
               # Now removing 414 is.na(time) but not resaving these

summary(sst_data)   # Earliest is 1987, so not adding tons (and not really worth
                    # excluding 1987-1991 for our purposes).

# Commenting these to not overwrite. But were not filtering by flags at all,
# which is okay.
# unique_id_after_1991 <- unique(sst_data$stn_id)
# saveRDS(unique_id_after_1991, "unique_id_after_1991.Rds")
# > unique(sst_data$stn_id)
# [1] C46206 C46004 C46184 C46036 C46208 C46205 C46207 C46181 C46204 C46145
#[11] C46183 C46182 C46185 C46146 C46131 C46147 C46132 C46134
# 83 Levels: C44131 C44137 C44138 C44139 C44140 C44141 C44142 C44143 ... WEL451

# unique_id_all_years <- unique(sst_data$stn_id)
# saveRDS(unique_id_all_years, "unique_id_all_years.Rds")
# unique_id_all_years

# Same length:
# expect_equal(length(unique_id_all_years), length(unique_id_after_1991))

# So the pre-1991 data adds another 100,000 rows (still multiple per day), but
#  no new stations. Worth keeping in for our purposes.



# Don't think I need the max date calculations really
## # AH:
## sst_data <- sst_data %>%
##   group_by(STN_ID) %>%
##   mutate(max_date = as.Date(max(time, na.rm=T))) %>%
##   ungroup()


## yearfilt = plot_yrs[2]    # more for plotting
## yearfilt = 2022
## # sst_data <- sst_data %>% filter(year(max_date) >= yearfilt)
## sst_data$max_date <- NULL
## # FILTER OUT DATA PAST CURRENT DATE
## time_curr <- Sys.time()
## attr(time_curr, "tzone") <- "UTC"
## sst_data <- sst_data %>% filter(time <= time_curr)


## metadata <- sst_data %>% group_by(STN_ID) %>%
##   summarise(start_date = min(time, na.rm=T),
##             end_date = max(time, na.rm=T),
##             life_span = end_date - start_date,
##             lon = mean(longitude, na.rm=T),
##             lat = mean(latitude, na.rm=T))
## units(metadata$life_span) <- "days"
## metadata$life_span_yrs <- metadata$life_span/365 # need to fix suffix...
## metadata$life_span_yrs = as.numeric(metadata$life_span_yrs)



# Daily mean values
# TODO: switch to PDT, wait to see what AH says
sst_daily_mean <- sst_data %>%
  arrange(desc(latitude), longitude) %>%
  # mutate_at(vars(name_key), funs(factor(., levels=unique(.)))) %>%
  group_by(stn_id,
           date = as.Date(time)) %>%
  summarise(sstp_mean_day = mean(sstp)) %>%
  ungroup()

# Just looking at first buoy, looks like an outlier early on (pre-1991). Should
#  analyse these once have plotting functions nicely done.
buoy_1 <- filter(sst_daily_mean, stn_id == "C46207")
plot(buoy_1$date, buoy_1$sstp_mean_day)
which(buoy_1$sstp_mean_day > 20)   # 254
buoy_1[250:260, ]

# So have sst_daily_mean which has all buoys.

# From Andrea, to keep all buoys that were older and went to 'relatively
#  present'. TODO
#
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

filter(sst_daily_mean, stn_id %in% buoy_list)    # 5477 out of 172,236. Come
                                        # back to at end after doing ECCC data. TODO




# OPP Buoys ####  OPP? Presumably Oceans Protection Plan. TODO AH asking.
#  Starts in 2019, so could well be.

if(redownload_data)) {
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

opp_data_raw   #  A tibble: 1,277,438 × 11. File size (of temp download file I think) is 119 Mb

# Andrea doesn't use the flags in these data, so not using here
opp_data <- as_tibble(opp_data_raw) %>%
  filter(wmo_synop_id %in% c("46303", "46304")) %>%
  mutate(time = as.POSIXct(time,
                            format="%Y-%m-%dT%H:%M:%SZ",
                            tz = "America/Los_Angeles"),   # TODO check how this
                                        # changes values, be consistent
         longitude = as.numeric(longitude),
         latitude = as.numeric(latitude),
         stn_id = as.factor(wmo_synop_id),
         sst = as.numeric(avg_sea_sfc_temp_pst10mts)) %>%
  select(time,
         stn_id,
         # latitude,         # Don't need, since match buoys_metadata - see below
         # longitude,
         sst) %>%
  filter(!is.na(time),
         !is.na(sst))

opp_data # A tibble: 346,320 × 3
summary(opp_data)

# range(filter(opp_data, stn_id == "46303")$longitude)  # -123.43 -123.43
# range(filter(opp_data, stn_id == "46304")$longitude)  # -123.357 -123.357

# range(filter(opp_data, stn_id == "46303")$latitude)  # 49.025 49.025
# range(filter(opp_data, stn_id == "46304")$latitude)  # 49.30167 49.30167

# So don't need to keep those values, delete them earlier. They match the values
#  in buoys_metadata.

# attr(opp_data$time,"tzone") <- "UTC" # AH: KEEPING PST FOR NOW FOR BETTER DAY AVGS

# opp_data$time_hr = round_date(opp_data$time, unit = "hour")

opp_data$time_day = as.Date(opp_data$time)
opp_agg = opp_data %>%
  group_by(year = year(time),
           time=time_day,
           wmo_synop_id) %>%
  summarise(sst = mean(avg_sea_sfc_temp_pst10mts,
                       na.rm=T)) %>%
  ungroup()

opp_agg # A tibble: 11,271 × 4

# Okay, it's only 2019 to 2023.

# Just manually saying which are unique (*), not duplicated in other data
unique(sst_daily_mean$stn_id)
# C46004 C46036 C46131 C46132 C46134* C46145 C46146 C46147 C46181 C46182*
# C46183 C46184 C46185 C46204 C46205 C46206 C46207 C46208

sort(unique(opp_agg$wmo_synop_id))
# 46004 46036 46131 46132 46145 46146 46147 46181 46183 46184 46185 46204
# 46205 46206 46207 46208 46303* 46304*

range(filter(sst_daily_mean, stn_id == "C46134")$date)
# "2001-02-20" "2016-12-09"
# So keep that one in (Andrea probably excluded)
# Though filter(buoys_metadata, wmo_id == 46134) says start date of 1999-01-01.

range(filter(sst_daily_mean, stn_id == "C46182")$date)
#  "1989-09-08" "1991-11-22"
# Hence ignore that one in the first data set. TODO

summary(filter(opp_agg, wmo_synop_id %in% c(46303, 46304)))
# Still two NA's in dates should get rid of earlier

# AVERAGE AND COMBINE DATA SOURCES

# Now all in buoys_metadata
# source(paste0(here::here(), "/../../../Pacific_SST_Monitoring/scripts/POI_latlon.R"))

buoys  <- buoys_metadata
buoys$stn_id <- paste0("C", buoys_metadata$wmo_id) # Buoy latlon from saved metadata

colnames(sst_daily_mean)
# names(sst_daily_mean)[names(sst_daily_mean) == "SSTP_mean_day"] <- "SSTP"
colnames(opp_agg)

names(opp_agg)[names(opp_agg) == "sst"] <- "sst_mean_day"
names(opp_agg)[names(opp_agg) == "time"] <- "date"
opp_agg$stn_id = paste0("C", opp_agg$wmo_synop_id)
opp_agg <- opp_agg %>%
  select(date,
         stn_id,
         sst)

data_buoy_full = full_join(sst_daily_mean,
                           opp_agg)


# Joining to buoy metadata
data_buoy_full2 <- full_join(data_buoy_full,
                             buoys,
                             by = "stn_id")

buoys$name_key <- paste(buoys$stn_id, buoys$buoy_name)    # Redo these as
                                        # needed, once figure them out

data_buoy_full2$name_key <- paste(data_buoy_full2$stn_id, data_buoy_full2$buoy_name)


# Remove erroneous buoys
data_buoy_full3 <- data_buoy_full2 %>%
  group_by(name_key) %>%
  mutate(numobs = n()) %>%
  filter(numobs > 1) %>%
  ungroup()


# For climatology, which I don't really need.
sstclim <- data_buoy_full3 %>%
  filter(year(date) <= 2020, # Period 1991-2020 for climatology
         !(stn_id %in% c("C46303", "C46304","C46182","C46134"))) %>%
  group_by(name_key,
           jday = yday(date)) %>%
  summarise(SSTP_10perc_day = quantile(SSTP, probs = 0.1, na.rm=T),
         SSTP_90perc_day = quantile(SSTP, probs = 0.9, na.rm=T),
         SSTP_clim_mean_day = mean(SSTP, na.rm=T),
         SSTP_clim_n = sum(!is.na(SSTP)),
         col_key = unique(col_key),
         numobs_stn = sum(!is.na(SSTP))) %>%
  ungroup() %>% filter(numobs_stn > 0)
sstclim$fakedate = as.Date(paste(plot_yrs[1], sstclim$jday), format = "%Y %j")


# Buoy map ####
world <- ne_countries(scale = 10, returnclass = "sf")#, country = c("Canada")
buoyplot <- data_buoy_full3 %>% filter(year(date) >= plot_yrs[1]) %>%
  dplyr::select(name_key, lon, lat, col_key) %>% distinct()
buoyplot$test <- str_extract(string=buoyplot$name_key,
                             pattern = "C[0-9]{5}")

g <- ggplot() +
  geom_sf(data = world, colour = "grey70", fill = "grey95") +
  geom_point(data = buoyplot, aes(x = lon, y = lat, fill = col_key), shape = 21, size = 3) +
  scale_fill_identity(labels = buoyplot$name_key,
                      breaks = buoyplot$col_key, guide = "legend") +
  coord_sf(xlim = c(-140, -121.), ylim = c(46.7, 55.8), expand = F) +
  xlab(NULL) + ylab(NULL) +
  scale_y_continuous(breaks = seq(48,56,2)) +
  labs(fill = NULL) +
  ggrepel::geom_text_repel(data = buoyplot,
                           aes(x = lon, y = lat, label = name_key),
                           size = 2.5) + #, nudge_x = 1, nudge_y = -0.1
  theme(legend.position = "none") +
  labs(caption = paste0("DFO_MEDS_BUOYS data last updated: ", max(sst_daily_mean$date,na.rm=T),
                        "\nECCC_MSC_BUOYS data last updated: ",
                        if_else(max(opp_agg$date,na.rm=T) <= Sys.Date(), max(opp_agg$date,na.rm=T), Sys.Date()),
                       "\nPlot last updated:", Sys.Date()))
# ggsave("Buoy_quickmap_colours.png", units = "in", width = 4, height = 4, scale = 1.9)

# Get the current year and last year's data
data_buoy_full3$fakedate = as.Date(paste(plot_yrs[1], yday(data_buoy_full3$date)), format = "%Y %j")

prevyr = data_buoy_full3 %>% filter(year(date) == plot_yrs[1])
currentyr = data_buoy_full3 %>% filter(year(date) == plot_yrs[2])

dayseq = seq.Date(as.Date(paste0(plot_yrs[1], "-01-01")),
                  as.Date(paste0(plot_yrs[1], "-12-31")), by = "day")
currentyr = currentyr %>%
  # filter(year(date) == plot_yrs[2]) %>%
  group_by(name_key) %>%
  tidyr::complete(fakedate = seq.Date(min(fakedate), max(fakedate), by = "day")) %>%
  ungroup() %>%
  arrange(name_key, fakedate)

prevyr = prevyr %>%
  # filter(year(date) == plot_yrs[1]) %>%
  group_by(name_key, col_key) %>%
  tidyr::complete(fakedate = dayseq) %>%
  ungroup() %>%
  arrange(name_key, fakedate)

# Time series plot ####
s <- prevyr %>%
  arrange(STN_ID, fakedate) %>%          # STN_ID %in% c("C46206","C46146")) %>%
  ggplot() +
  facet_wrap(~name_key, ncol = 3) +
  geom_ribbon(data = sstclim, aes(x = fakedate, ymin = SSTP_10perc_day, ymax = SSTP_90perc_day), fill = "grey70", colour = NA, alpha = 0.5) +
  geom_path(data = sstclim, aes(x = fakedate, y = SSTP_clim_mean_day), colour = "grey30", linewidth = 0.8) +
  # Heat dome - for 2021-2022 plots
  # geom_vline(xintercept = as.Date("2021-06-26"), linetype = "dashed") +
  geom_path(data = prevyr %>% filter((year(date) == plot_yrs[1]) | is.na(SSTP)), aes(x = fakedate, y = SSTP, colour = col_key), size = 0.6) +

  geom_path(data = currentyr %>% filter((year(date) == plot_yrs[2]) | is.na(SSTP)),aes(x = fakedate, y = SSTP), colour = "black", size = 1.4, lineend = "round") +
  geom_path(data = currentyr %>% filter(year(date) == plot_yrs[2] | is.na(SSTP)), aes(x = fakedate, y = SSTP), colour = "white", size = 0.5, lineend = "round") +
  scale_colour_manual(values = (glasbey_mod[1:(nrow(buoyplot))])) +
  scale_colour_identity(breaks = buoyplot$col_key) +
  ylab(expression("Mean Daily SST " ( degree*C))) +
  xlab(NULL) +
  scale_x_date(labels = scales::date_format("%b"), breaks = "2 months") +
  theme(legend.position = "none",
        strip.background = element_rect(colour = "grey70", fill = "grey95"))
 # s
# ggsave("Daily_mean_SSTP.png", width = 6, height = 4, scale = 1.6)

# Assemble and save ####

library(cowplot)

gg <- plot_grid(s, g,rel_heights = c(1,1), rel_widths = c(1.1,0.9))

ggsave(filename = paste0("Daily_mean_buoy_overview_",plot_yrs[2],".png"),
       plot = gg, width = 10, height = 7,
       units = "in", scale = 1.25)

# TODO See if any raw data are from Wallace, do up a figure. 45deg 48 min N, 63deg 28 min W.
