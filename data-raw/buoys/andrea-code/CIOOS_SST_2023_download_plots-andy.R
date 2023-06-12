rm(list=ls())
library(rerddap)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())
library(rnaturalearthdata)
library(rnaturalearthhires)
library(rnaturalearth)
library(lubridate)
library(stringr)

LOAD_DATA = FALSE

plot_yrs = c(2022, 2023)

# Palette:
glasbey_32 <- c("#0000FF","#FF0000","#00FF00","#000033","#FF00B6","#005300",
                "#FFD300","#009FFF","#9A4D42","#00FFBE","#783FC1","#1F9698",
                "#FFACFD","#B1CC71","#F1085C","#FE8F42","#DD00FF","#201A01",
                "#720055","#766C95","#02AD24","#C8FF00","#886C00","#FFB79F",
                "#858567","#A10300","#14F9FF","#00479E","#DC5E93","#93D4FF",
                "#004CFF","#F2F318")
glasbey_mod <- glasbey_32[!(glasbey_32 %in% c("#B1CC71","#201A01","#F1085C","#720055","#766C95"))]
# CIOOS flags for the DFO MEDS record
use_flags = c(4, 9, 11, 12, 13, 14, 15, 16) # https://catalogue.cioospacific.ca/dataset/ca-cioos_b9c71eb2-b750-43d5-a50a-aee173916736

# CIOOS buoy source ####

if (LOAD_DATA == FALSE) {
  cache_delete_all() # USE IF NOT UPDATING PROPERLY
  sstInfo <- info("DFO_MEDS_BUOYS", url = "https://data.cioospacific.ca/erddap/")
  sstdata <- tabledap(x = sstInfo,
                      fields = c("time","longitude","latitude","STN_ID", "SSTP", "SSTP_flags"))
  # Filter out bad SST values
  sstdata$SSTP <- as.numeric(sstdata$SSTP)
  sstdata$SSTP[sstdata$SSTP < -10] <- NA

  # Apply flags
  sstdata$SSTP_flags <- as.numeric(sstdata$SSTP_flags)
  sstdata$flagcol <- if_else((sstdata$SSTP_flags %in% use_flags) | is.na(sstdata$SSTP_flags), 1, 0)

  # Convert other fields
  sstdata$longitude <- as.numeric(sstdata$longitude)

  sstdata = sstdata %>% filter(longitude < -100,
                               flagcol == 1)


  sstdata$time <- as.POSIXct(sstdata$time, format="%Y-%m-%dT%H:%M:%SZ")
  sstdata = sstdata[sstdata$time >= as.POSIXct("1991-01-01T00:00:00"),]

  sstdata$latitude <- as.numeric(sstdata$latitude)
  sstdata = sstdata %>% filter(latitude < 60)
  saveRDS(sstdata,"cioos_buoy_backup2.rds")
} else if (LOAD_DATA == TRUE) {
  sstdata <- readRDS("cioos_buoy_backup2.rds")
}


sstdata <- sstdata %>% group_by(STN_ID) %>% mutate(max_date = as.Date(max(time,na.rm=T))) %>% ungroup()
yearfilt = plot_yrs[2]
yearfilt = 2022
# sstdata <- sstdata %>% filter(year(max_date) >= yearfilt)
sstdata$max_date <- NULL
# FILTER OUT DATA PAST CURRENT DATE
time_curr <- Sys.time()
attr(time_curr, "tzone") <- "UTC"
sstdata <- sstdata %>% filter(time <= time_curr)

metadata <- sstdata %>% group_by(STN_ID) %>%
  summarise(start_date = min(time, na.rm=T),
            end_date = max(time, na.rm=T),
            life_span = end_date - start_date,
            lon = mean(longitude, na.rm=T),
            lat = mean(latitude, na.rm=T))
units(metadata$life_span) <- "days"
metadata$life_span_yrs <- metadata$life_span/365 # need to fix suffix...
metadata$life_span_yrs = as.numeric(metadata$life_span_yrs)

# OPP Buoys ####

if (LOAD_DATA == FALSE) {

  oppInfo <- info("ECCC_MSC_BUOYS", url = "https://data.cioospacific.ca/erddap/")
  oppdata <- tabledap(x = oppInfo,
                      fields = c("avg_sea_sfc_temp_pst10mts", # checked out the field with '1' after, no data
                                 "avg_sea_sfc_temp_pst10mts_data_flag",
                                 "avg_sea_sfc_temp_pst10mts_qa_summary",
                                 "date_tm",
                                 "latitude",
                                 "longitude",
                                 "avg_sea_sfc_temp_pst10mts_1_data_flag",
                                 "avg_sea_sfc_temp_pst10mts_1_qa_summary",
                                 # "result_time",
                                 "stn_nam","time","wmo_synop_id"))
  oppdata$longitude = as.numeric(oppdata$longitude)
  oppdata$latitude = as.numeric(oppdata$latitude)
  oppdata = oppdata %>% filter(longitude <= -120)
  oppdata$avg_sea_sfc_temp_pst10mts = as.numeric(oppdata$avg_sea_sfc_temp_pst10mts)

  oppdata$avg_sea_sfc_temp_pst10mts[oppdata$avg_sea_sfc_temp_pst10mts <= -1.89] <- NA
  # oppdata <- oppdata %>% filter(wmo_synop_id %in% c("46303","46304"))
  saveRDS(oppdata,"opp_buoy_backup.rds")
} else if (LOAD_DATA == TRUE) {
  oppdata <- readRDS("opp_buoy_backup.rds")
}
oppdata$time <- as.POSIXct(oppdata$time, format="%Y-%m-%dT%H:%M:%SZ", tz = "America/Los_Angeles")

metaopp = oppdata %>% group_by(stn_nam) %>%
  summarise(start_date = min(time, na.rm=T),
            end_date = max(time, na.rm=T),
            life_span = end_date - start_date,
            lon = mean(longitude, na.rm=T),
            lat = mean(latitude, na.rm=T))
units(metaopp$life_span) <- "days"
metaopp$life_span_yrs <- metaopp$life_span/365 # need to fix suffix...
metaopp$life_span_yrs = as.numeric(metaopp$life_span_yrs)
metaopp$start_date = as.Date(metaopp$start_date)
metaopp$end_date = as.Date(metaopp$end_date)
metaopp$life_span <- NULL
# attr(oppdata$time,"tzone") <- "UTC" # KEEPING PST FOR NOW FOR BETTER DAY AVGS
oppdata <- oppdata %>% filter(year(time) >= plot_yrs[1])
# oppdata$time_hr = round_date(oppdata$time, unit = "hour")
oppdata$time_day = as.Date(oppdata$time)
oppagg = oppdata %>% group_by(year = year(time), time=time_day, wmo_synop_id) %>%
  summarise(sst = mean(avg_sea_sfc_temp_pst10mts, na.rm=T)) %>% ungroup()

# AVERAGE AND COMBINE DATA SOURCES
# Daily mean values
sstmean <- sstdata %>%
  arrange(desc(latitude), longitude) %>%
  # mutate_at(vars(name_key), funs(factor(., levels=unique(.)))) %>%
  group_by(STN_ID, date = as.Date(time)) %>%
  summarise(SSTP_mean_day = mean(SSTP, na.rm=T)) %>%
  ungroup()

source("scripts/POI_latlon.R")
buoys$STN_ID <- paste0("C", buoys$wmo_id) # Buoy latlon from file
colnames(sstmean)
names(sstmean)[names(sstmean) == "SSTP_mean_day"] <- "SSTP"
colnames(oppagg)
names(oppagg)[names(oppagg) == "sst"] <- "SSTP"
names(oppagg)[names(oppagg) == "time"] <- "date"
oppagg$STN_ID = paste0("C",oppagg$wmo_synop_id)
oppagg <- oppagg %>% select(date, STN_ID, SSTP)

databuoyfull = full_join(sstmean, oppagg)
# Joining to buoy metadata
databuoyfull2 <- full_join(databuoyfull, buoys, by = "STN_ID")
buoys$name_key <- paste(buoys$STN_ID, buoys$buoy_name)
databuoyfull2$name_key <- paste(databuoyfull2$STN_ID, databuoyfull2$buoy_name)
# Remove erroneous buoys
databuoyfull3 <- databuoyfull2 %>% group_by(name_key) %>%
  mutate(numobs = n()) %>% filter(numobs > 1) %>% ungroup()

sstclim <- databuoyfull3 %>%
  filter(year(date) <= 2020, # Period 1991-2020 for climatology
         !(STN_ID %in% c("C46303", "C46304","C46182","C46134"))) %>%
  group_by(name_key, jday = yday(date)) %>%
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
buoyplot <- databuoyfull3 %>% filter(year(date) >= plot_yrs[1]) %>%
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
  labs(caption = paste0("DFO_MEDS_BUOYS data last updated: ", max(sstmean$date,na.rm=T),
                        "\nECCC_MSC_BUOYS data last updated: ",
                        if_else(max(oppagg$date,na.rm=T) <= Sys.Date(), max(oppagg$date,na.rm=T), Sys.Date()),
                       "\nPlot last updated:", Sys.Date()))
# ggsave("Buoy_quickmap_colours.png", units = "in", width = 4, height = 4, scale = 1.9)

# Get the current year and last year's data
databuoyfull3$fakedate = as.Date(paste(plot_yrs[1], yday(databuoyfull3$date)), format = "%Y %j")

prevyr = databuoyfull3 %>% filter(year(date) == plot_yrs[1])
currentyr = databuoyfull3 %>% filter(year(date) == plot_yrs[2])

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


beepr::beep()
