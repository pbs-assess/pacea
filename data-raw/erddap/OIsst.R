

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
    group_by(latitude, longitude, year, week) %>% 
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

# rename output and save data
oisst_month <- out.dat
use_data(oisst_7day, compress = "xz")
