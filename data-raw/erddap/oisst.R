

## OISST NOAA erddap data download and processing
## Getting data from NOAA website for modelled sst
# SST and anomalies using CoastWatch Data
#
# NOTE: NOAA data are current to 14 day lag. Therefore, updating to most recent previous month should be done after the 15th of the current month
#
library(rerddap)
library(dplyr)
options(dplyr.summarise.inform = FALSE)
library(ggplot2)
library(lubridate)
library(sf)
sf_use_s2(FALSE)

library(devtools)

# set working directory from current path script
setwd(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/../../"))

load_all()

#####
# parameters

# bounding box of bc_eez from pacea
eez_ext <- st_bbox(bc_eez)

# Area
latlim = eez_ext[c(2,4)]
lonlim = eez_ext[c(1,3)]


# if want to if only want to update data = FALSE; or download all data = TRUE
DL_ALL_DATA <- FALSE

# which data to process
DATA_MONTH <- TRUE
DATA_7DAY <- TRUE

#####
# downloading only recent data

if(!DL_ALL_DATA){
  print("Updating data...")
  
  # current date
  date_current <- Sys.Date()
  year_current <- lubridate::year(date_current)
  month_current <- lubridate::month(date_current)
  
  # oisst_7day
  oisst_7day_orig <- oisst_7day
  
  # set start date from start of year of latest date in oisst_7day data
  # this ensures that each calendar week is calculated correctly
  timechar <- "T12:00:00Z"
  data_enddate <- max(oisst_7day_orig$end_date)
  #start_date <- paste0(data_enddate + 1, timechar)
  start_date <- paste0(year_current,"-01-01", timechar)
  
  # set end date, and time limit
  end_date <- paste0(as.Date(paste0(year_current, "-", month_current, "-01")) - 1, timechar)
  timelim <- c(start_date, end_date)
  
  # remove current year data from oisst data
  oisst_7day_orig <- oisst_7day_orig %>% 
    filter(year != year_current)
  
  # ERRDAP datat: 14 day lag; 0.25 degree grid
  sstInfo <- info("ncdcOisst21Agg_LonPM180", url = "https://coastwatch.pfeg.noaa.gov/erddap/")
  
  # download data
  sstdata <- griddap(sstInfo, latitude = latlim, longitude = lonlim,
                     time = timelim)
  
  # 7day means
  if(DATA_7DAY){
    sstdata_7day <- sstdata$data %>%
      filter(!is.na(sst)) %>%
      mutate(date = as.Date(time),
             year = lubridate::year(date),
             week = lubridate::week(date)) %>%
      group_by(latitude, longitude, year, week) %>%
      summarise(tsst = mean(sst, na.rm = TRUE),
                sst_sd = sd(sst, na.rm = TRUE),
                sst_n = sum(!is.na(sst)),
                start_date = min(as.Date(time)),
                end_date = max(as.Date(time))) %>%
      ungroup() %>%
      rename(sst = tsst)
    
    sstdata_7day_sf <- st_as_sf(sstdata_7day, coords = c("longitude", "latitude")) %>%
      st_set_crs("EPSG: 4326")
    
    # mask with eez boundary
    sstdata_7day_sf <- sstdata_7day_sf[bc_eez,]
    
    # add to original file
    oisst_7day_new <- oisst_7day_orig %>% rbind(sstdata_7day_sf)
    
    # check duplicates
    dups <- oisst_7day_new %>%
      select(year, week, geometry) %>%
      duplicated()
    
    # write out new data
    if(sum(dups) == 0){
      # rename output and save data
      oisst_7day <- oisst_7day_new
      class(oisst_7day) <- c("pacea_oi", class(oisst_7day))
      attr(oisst_7day, "units") <- "Temperature (\u00B0C)"
      use_data(oisst_7day, compress = "xz", overwrite = TRUE)
      
      print("oisst_7day finished")
    } else {
      stop("Duplicated rows (year:week:geometry) in oisst_7day_new")
    }
  }
  
  
  ##########
  # month data
  if(DATA_MONTH){
    # remove current year data from oisst data
    oisst_month_orig <- oisst_month %>% 
      filter(year != year_current)
    
    # monthly means
    sstdata_month <- sstdata$data %>%
      filter(!is.na(sst)) %>%
      mutate(date = as.Date(time),
             year = lubridate::year(date),
             month = lubridate::month(date)) %>%
      group_by(latitude, longitude, year, month) %>%
      summarise(tsst = mean(sst, na.rm = TRUE),
                sst_sd = sd(sst, na.rm = TRUE),
                sst_n = sum(!is.na(sst)),
                start_date = min(as.Date(time)),
                end_date = max(as.Date(time))) %>%
      ungroup() %>%
      rename(sst = tsst)
    
    sstdata_month_sf <- st_as_sf(sstdata_month, coords = c("longitude", "latitude")) %>%
      st_set_crs("EPSG: 4326")
    
    # mask with eez boundary
    sstdata_month_sf <- sstdata_month_sf[bc_eez,]
    
    # add to original file
    oisst_month_new <- oisst_month_orig %>% rbind(sstdata_month_sf)
    
    # check duplicates
    dups <- oisst_month_new %>%
      select(year, month, geometry) %>%
      duplicated()
    
    # write out new data
    if(sum(dups) == 0){
      # rename output and save data
      oisst_month <- oisst_month_new
      class(oisst_month) <- c("pacea_oi", class(oisst_month))
      attr(oisst_month, "units") <- "Temperature (\u00B0C)"
      use_data(oisst_month, compress = "xz", overwrite = TRUE)
      
      print("oisst_month finished")
    } else {
      stop("Duplicated rows (year:month:geometry) in oisst_month_new")
    }
  }
}



#####
# settings for downloading and processing all data

if(DL_ALL_DATA){
  # date
  date_init <- "01-01"
  date_fin <- "12-31"
  timechar <- "T12:00:00Z"
  
  # current date
  date_current <- Sys.Date()
  year_current <- lubridate::year(date_current)
  month_current <- lubridate::month(date_current)
  
  # 14 day lag; 0.25 degree grid
  sstInfo <- info("ncdcOisst21Agg_LonPM180", url = "https://coastwatch.pfeg.noaa.gov/erddap/")
  
  #####
  # 7 day mean sst from 1981 to previous month before present month
  out.dat_7day <- data.frame()
  
  # month mean sst
  out.dat_month <- data.frame()
  
  for(i in 1981:year_current){
    start_date <- paste0(i, "-", date_init, timechar)
    end_date <- paste0(i, "-", date_fin, timechar)
    timelim <- c(start_date, end_date)
    
    # data starts on 1981-09-01
    if(i==1981){
      start_date <- paste0(i, "-", "09-01", timechar)
      timelim <- c(start_date, end_date)
    }
    
    # get data for current year up to the previous month before present (data have 2 week time lag) - midmonth update will be sufficient for previous months data
    # if current date is at start of year:
    if(i==year_current){
      if(month_current > 1){
        end_date <- paste0(as.Date(paste0(i, "-", month_current, "-01")) - 1, timechar)
        timelim <- c(start_date, end_date)
      } else {
        next()
      }
    } 
    
    # download data
    sstdata <- griddap(sstInfo, latitude = latlim, longitude = lonlim,
                       time = timelim)
    
    if(DATA_7DAY){
      # process 7day mean
      sstdata_7day <- sstdata$data %>%
        filter(!is.na(sst)) %>%
        mutate(date = as.Date(time),
               year = lubridate::year(date),
               week = lubridate::week(date)) %>%
        group_by(latitude, longitude, year, week) %>%
        summarise(tsst = mean(sst, na.rm = TRUE),
                  sst_sd = sd(sst, na.rm = TRUE),
                  sst_n = sum(!is.na(sst)),
                  start_date = min(as.Date(time)),
                  end_date = max(as.Date(time))) %>%
        ungroup() %>%
        rename(sst = tsst)
      
      sstdata_7day_sf <- st_as_sf(sstdata_7day, coords = c("longitude", "latitude")) %>%
        st_set_crs("EPSG: 4326")
      
      # mask with eez boundary
      sstdata_7day_sf <- sstdata_7day_sf[bc_eez,]
      
      # write to output file.
      out.dat_7day <- out.dat_7day %>% rbind(sstdata_7day_sf)
    }
    
    if(DATA_MONTH){
      # process month mean
      sstdata_month <- sstdata$data %>%
        filter(!is.na(sst)) %>%
        mutate(date = as.Date(time),
               year = lubridate::year(date),
               month = lubridate::month(date)) %>%
        group_by(latitude, longitude, year, month) %>%
        summarise(tsst = mean(sst, na.rm = TRUE),
                  sst_sd = sd(sst, na.rm = TRUE),
                  sst_n = sum(!is.na(sst)),
                  start_date = min(as.Date(time)),
                  end_date = max(as.Date(time))) %>%
        ungroup() %>%
        rename(sst = tsst)
      
      sstdata_month_sf <- st_as_sf(sstdata_month, coords = c("longitude", "latitude")) %>%
        st_set_crs("EPSG: 4326")
      
      # mask with eez boundary
      sstdata_month_sf <- sstdata_month_sf[bc_eez,]
      
      # write to output file.
      out.dat_month <- out.dat_month %>% rbind(sstdata_month_sf)
    }
  }
  
  if(DATA_7DAY){
    # rename output and save data - 7day
    oisst_7day <- out.dat_7day
    class(oisst_7day) <- c("pacea_oi", class(oisst_7day))
    attr(oisst_7day, "units") <- "Temperature (\u00B0C)"
    use_data(oisst_7day, compress = "xz", overwrite = TRUE)
  }
  
  if(DATA_MONTH){
    # rename output and save data - month
    oisst_month <- out.dat_month
    class(oisst_month) <- c("pacea_oi", class(oisst_month))
    attr(oisst_month, "units") <- "Temperature (\u00B0C)"
    use_data(oisst_month, compress = "xz", overwrite = TRUE)
  }
}

