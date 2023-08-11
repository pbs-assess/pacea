

## OISST NOAA erddap data download and processing
## Getting data from NOAA website for modelled sst
# SST and anomalies using CoastWatch Data
library(rerddap)
library(dplyr)
options(dplyr.summarise.inform = FALSE)
library(ggplot2)
library(lubridate)


#####
# parameters

# bounding box of bc_eez from pacea
eez_ext <- st_bbox(bc_eez)

# Area
latlim = eez_ext[c(2,4)]
lonlim = eez_ext[c(1,3)]

# date 
date_init <- "01-01"
date_fin <- "12-31"
timechar <- "T12:00:00Z"

# 14 day lag; 0.25 degree grid
sstInfo <- info("ncdcOisst21Agg_LonPM180", url = "https://coastwatch.pfeg.noaa.gov/erddap/")


#####
# 7 day mean sst from 1981 to 1 month before present month
out.dat <- data.frame()

for(i in 1981:(lubridate::year(Sys.Date()))){
  start_date <- paste0(i, "-", date_init, timechar)
  end_date <- paste0(i, "-", date_fin, timechar)
  timelim <- c(start_date, end_date)
  
  # data starts on 1981-09-01
  if(i==1981){
    start_date <- paste0(i, "-", "09-01", timechar)
    timelim <- c(start_date, end_date)
  }
  
  # get data for current year up to 2 months before present
  if(i==2023){
    month <- lubridate::month(Sys.Date()) - 1
    end_date <- paste0(as.Date(paste0(i, "-", month, "-01")) - 1, timechar)
    timelim <- c(start_date, end_date)
  }
  
  sstdata <- griddap(sstInfo, latitude = latlim, longitude = lonlim, 
                     time = timelim)
  sstdata_7day <- sstdata$data %>% 
    filter(!is.na(sst)) %>%
    mutate(date = as.Date(time),
           year = lubridate::year(date),
           week = lubridate::week(date)) %>%
    group_by(latitude, longitude, year, week) %>% 
    summarise(sst_7day = mean(sst, na.rm = TRUE),
              sst_7daysd = sd(sst, na.rm = TRUE),
              sst_7dayn = sum(!is.na(sst)),
              start_date = min(as.Date(time)),
              end_date = max(as.Date(time))) %>%
    ungroup()
  
  sstdata_7day_sf <- st_as_sf(sstdata_7day, coords = c("longitude", "latitude")) %>%
    st_set_crs("EPSG: 4326")
  
  # mask with eez boundary
  tdat <- sstdata_7day_sf[bc_eez,]
  
  # write to output file.
  out.dat <- out.dat %>% rbind(tdat)
  
}

# rename output and save data
oisst_7day <- out.dat
use_data(oisst_7day, compress = "xz")





#####
# monthly mean sst from 1981 to 2 month before present month
out.dat <- data.frame()

for(i in 1981:(lubridate::year(Sys.Date()))){
  start_date <- paste0(i, "-", date_init, timechar)
  end_date <- paste0(i, "-", date_fin, timechar)
  timelim <- c(start_date, end_date)
  
  # data starts on 1981-09-01
  if(i==1981){
    start_date <- paste0(i, "-", "09-01", timechar)
    timelim <- c(start_date, end_date)
  }
  
  # get data for current year up to 2 months before present
  if(i==2023){
    month <- lubridate::month(Sys.Date()) - 1
    end_date <- paste0(as.Date(paste0(i, "-", month, "-01")) - 1, timechar)
    timelim <- c(start_date, end_date)
  }
  
  sstdata <- griddap(sstInfo, latitude = latlim, longitude = lonlim, 
                     time = timelim)
  sstdata_month <- sstdata$data %>% 
    filter(!is.na(sst)) %>%
    mutate(date = as.Date(time),
           year = lubridate::year(date),
           month = lubridate::month(date)) %>%
    group_by(latitude, longitude, year, month) %>% 
    summarise(sst_month = mean(sst, na.rm = TRUE),
              sst_monthsd = sd(sst, na.rm = TRUE),
              sst_montn = sum(!is.na(sst)),
              start_date = min(as.Date(time)),
              end_date = max(as.Date(time))) %>%
    ungroup()
  
  sstdata_month_sf <- st_as_sf(sstdata_month, coords = c("longitude", "latitude")) %>%
    st_set_crs("EPSG: 4326")
  
  # mask with eez boundary
  tdat <- sstdata_month_sf[bc_eez,]
  
  # write to output file.
  out.dat <- out.dat %>% rbind(tdat)
  
}

# rename output and save data
oisst_month <- out.dat
use_data(oisst_month, compress = "xz")




#####  HERHERERE
# 30-year climatology 1991-2020 - CAN USE data already downloaded and processed
out.dat <- data.frame()

for(i in 1991:2020){
  start_date <- paste0(i, "-", date_init, timechar)
  end_date <- paste0(i, "-", date_fin, timechar)
  timelim <- c(start_date, end_date)
  
  sstdata <- griddap(sstInfo, latitude = latlim, longitude = lonlim, 
                     time = timelim)
  sstdata_month <- sstdata$data %>% 
    filter(!is.na(sst)) %>%
    mutate(date = as.Date(time),
           year = lubridate::year(date),
           month = lubridate::month(date)) %>%
    group_by(latitude, longitude, year, month) %>% 
    summarise(sst_month = mean(sst, na.rm = TRUE),
              sst_monthsd = sd(sst, na.rm = TRUE),
              sst_montn = sum(!is.na(sst)),
              start_date = min(as.Date(time)),
              end_date = max(as.Date(time))) %>%
    ungroup()
  
  sstdata_month_sf <- st_as_sf(sstdata_month, coords = c("longitude", "latitude")) %>%
    st_set_crs("EPSG: 4326")
  
  # mask with eez boundary
  tdat <- sstdata_7day_sf[bc_eez,]
  
  # write to output file.
  out.dat <- out.dat %>% rbind(tdat)
  
}
if (OIPROCESS == TRUE) {
  # OI Data - Current and 30 year climatology (1990-2020) ####
  # Datasets: 
  # ncdcOisst21NrtAgg_LonPM180 (2020-present, 4-day lag)
  # ncdcOisst21Agg_LonPM180 (1981-present, 17-day lag)
  
  # OI Rolling 7-day average ####
  sstInfo <- info("ncdcOisst21Agg_LonPM180", url = "https://coastwatch.pfeg.noaa.gov/erddap/")
  sstdata <- griddap(sstInfo, latitude = latlim, longitude = lonlim, 
                     time = timelim)
  print(paste(length(unique(sstdata$data$time)),"DAYS FOUND"))
  
  sstdata_7day <- sstdata$data %>% 
    filter(!is.na(sst)) %>%
    group_by(latitude, longitude) %>% 
    summarise(sst_7day = mean(sst, na.rm=T),
              sst_7daysd = sd(sst, na.rm=T),
              sst_7dayn = sum(!is.na(sst))) %>%
    ungroup() %>% 
    mutate(start_date = start_date,
           end_date = end_date)
  saveRDS(sstdata_7day, "data/OI_SST7day_rollingavgbackup_current.rds")
  
  # OI 7-day climatology ####
  sstvar = info("ncdcOisst21Agg_LonPM180", url = "https://coastwatch.pfeg.noaa.gov/erddap/")
  yr7days <- list()
  ct=1
  for (i in 1991:2020) {
    start = as.Date(paste0(i, yday(start_date)), format = "%Y%j")
    end = as.Date(paste0(i, yday(end_date)), format = "%Y%j")
    timesub = c(start, end)
    print(timesub)
    sstyr <- griddap(sstvar, latitude = latlim, longitude = lonlim, 
                     time = timesub)
    print(paste(length(unique(sstyr$data$time)), "days"))
    roll7 = sstyr$data %>%
      filter(!is.na(sst)) %>%
      group_by(lat, lon) %>%
      summarise(sst_7day = mean(sst, na.rm=T),
                sst_7dayn = sum(!is.na(sst))) %>%
      ungroup()
    yr7days[[ct]] <- roll7
    ct=ct+1
  }
  yr7days <- do.call(rbind, yr7days)
  
  yr7days <- yr7days %>% 
    filter(!is.na(sst_7day)) %>% 
    group_by(lat, lon) %>% 
    summarise(sst_7day_clim = mean(sst_7day, na.rm=T),
              sst_7day_climsd = sd(sst_7day, na.rm=T),
              sst_7day_climn = sum(sst_7dayn),
              sst_7day_90 = quantile(sst_7day, probs = 0.9)) %>% 
    ungroup()
  saveRDS(object = yr7days, file = "data/OI_SST7day_rollingavgbackup_climatology.rds")
  rm(roll7,sstyr)
  
}
gc()





