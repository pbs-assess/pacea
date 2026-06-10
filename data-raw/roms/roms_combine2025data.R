

# joining 2025 data with bccm up to 2024 data
library(devtools)
library(dplyr)
library(sf)
library(ggplot2)

sf_use_s2(FALSE)  # remove spherical geometry (s2) for sf operations

# load pacea
load_all()

# list all bccm data files to 2024 and the separate 2025 files
bccm24.files <- list.files("../pacea-data/data-bccm-full/", pattern = "full_02")
bccm25.files <- list.files("../pacea-data/data-bccm-full/", pattern = "full_2025")

# pacea-data path
dpath <- "../pacea-data/data-bccm-full/"

version <- "03"

for(i in 1:length(bccm24.files)){
  print(paste0("i = ", i))
  
  ## bccm 2024
  # file path
  fpath <- bccm24.files[i]
  
  # data object name
  dataname <- substr(fpath, 1, nchar(fpath) - 7)
  
  # split string of fpath
  fpathstring <- strsplit(fpath, "_")
  
  # depth string name
  tdepth <- fpathstring[[1]][2]
  
  # variable string name
  tvar <- fpathstring[[1]][3]
  if(tvar %in% c("u", "v")){
    tvar <- paste(fpathstring[[1]][c(3,4)], collapse = "_")
  }
  if(tdepth %in% c("phytoplankton", "primaryproduction")){
    tvar <- fpathstring[[1]][2]
  }
  
  # load 2024 data
  load(paste0(dpath, fpath))
  
  # rename 2024 data
  tdat24 <- get(dataname)

  
  ## bccm 2025
  # file path
  fpath25 <- bccm25.files[grep(tdepth, bccm25.files)]
  fpath25 <- fpath25[grep(tvar, fpath25)]
  
  if(length(fpath25) > 1){
    print(fpath)
    stop("more than one file matched to 2024 data")
  }
  
  if(length(fpath25) < 1){
    print(i)
    print(fpath)
    stop("can't find a match for existing data")
  }
  
  
  # data object name
  dataname25 <- substr(fpath25, 1, nchar(fpath25)- 9)
  
  # load 2025 data
  load(paste0(dpath, fpath25))
  
  # rename 2025 data
  tdat25 <- get(dataname25)

  # Need to rename column names for 2025 data - Do I still need this? Double check; Shouldn't need this anymore.
  # cnames <- paste(rep(2025, each=12), 1:12, sep="_")
  # names(tdat25)[1:12] <- cnames
  
  # confirm the geometry columns match before column binding
  if(!identical(st_geometry(tdat24), st_geometry(tdat25))){
    print(fpath)
    stop("geometry columns don't match")
  }
  
  # convert geometry to string
  tdat24$geostring <- as.character(st_geometry(tdat24))
  tdat25$geostring <- as.character(st_geometry(tdat25))
  
  # drop geometry for new 2025 data and join
  tdat_all <- tdat24 |> left_join(st_drop_geometry(tdat25), by = join_by("geostring" == "geostring")) |>
    select(-geostring) |>
    select(-geometry, everything(), geometry)
  class(tdat_all) <- c("pacea_st", class(tdat_all)[c(1,3,4,5)])
  
  # assign data original name 
  assign(dataname, tdat_all)
  
  # crerate filename
  filename <- paste0(dpath, dataname, "_", version, ".rds")
  
  # save object. when loaded, it will have the same object name
  do.call("save", list(as.name(dataname), file = filename, compress = "xz"))
  
  # remove files that aren't needed
  rm(list = c(dataname))
  rm(fpath, fpathstring, tdepth, tvar, dataname, 
     fpath25, dataname25,
     tdat24, tdat25, tdat_all, filename)
}



load(paste0(dpath, "bccm_primaryproduction_full_03.rds"))
load(paste0(dpath, "bccm_phytoplankton_full_03.rds"))

bccm3.files <- list.files(dpath, pattern = "_03")


## checking files to make sure name and dimensions are okay
for(i in 1:length(bccm3.files)){
  fpath <- bccm3.files[i]
  
  load(paste0(dpath, fpath))
  
  # data object name
  dataname <- substr(fpath, 1, nchar(fpath) - 7)
  
  # rename data
  tdat <- get(dataname)
  
  print(paste0("i = ", i))
  print(dim(tdat))
  
  rm(list = dataname)
  rm(fpath, tdat)
}




#####
#
#
# mask original raw data layer to get proper masked areas for certain depths


library(devtools)
library(dplyr)
library(terra)
library(gstat)
library(sf)
library(stars)
library(ncdf4)
library(ggplot2)
library(concaveman)
library(parallel)
library(foreach)

sf_use_s2(FALSE)  # remove spherical geometry (s2) for sf operations

# load pacea
load_all()

# list of interpolated files
bccm.files <- list.files("../pacea-data/data-bccm-full/", pattern = "_03")

# Set up parallel cluster (from https://www.blasbenito.com/post/02_parallelizing_loops_with_r/)

num_cores <- parallel::detectCores() - 2   # Think memory might get limited if
# do too many
# Create the cluster
my_cluster <- parallel::makeCluster(
  num_cores,
  type = "PSOCK")

my_cluster

# Register it to be used by %dopar%
doParallel::registerDoParallel(cl = my_cluster)

# Set up directories

pacea_dir <- here::here()   # Will give local pacea/
pacea_data_dir <- paste0("../pacea-data/data-bccm-full/")  # Where to save .rds
# files. Then uploading to Zenodo, not
# commiting those to GitHub.
# TODO add in mkdir if it's not already there.

#####
# START - load data to environment

# transform bc_coast
tbc <- bc_coast |> st_transform(crs = "EPSG: 3005") 

# convert to multilinestring
tbc.line <- st_cast(tbc, "MULTILINESTRING")



#####
# PARAMETERS

# pacea-data path
dpath <- "../pacea-data/data-bccm-full/"

# bccm raw data folder
raw_dir <-"bccm-output-to-2025"

list.files(paste0(pacea_dir,"/data-raw/roms/bccm-raw-data/", raw_dir, "/"))
# file name characters for depth range
startchar <- 27
endchar <- startchar + 2

substr(list.files(paste0(pacea_dir,"/data-raw/roms/bccm-raw-data/", raw_dir, "/")), 
       startchar, endchar)

nc_filenames <- list.files(paste0(pacea_dir,
                                  "/data-raw/roms/bccm-raw-data/", raw_dir, "/"),
                           pattern = "TSOpH.nc")

# function argument
llnames <- c("x", "y")
nmax <- 4

# column names. See hotssea-data-interpolation.R for automated version (if
# there's a time_counter in the .nc file).
cnames <- paste(rep(2025, each=12), 1:12, sep="_")

# version of data update
version <- "03"

# surface mask layer - extent of original model output
snc_dat <- nc_open(paste0(pacea_dir,
                          "/data-raw/roms/bccm-raw-data/", "bccm-output-to-2024",
                          "/bcc42_era5glo12r6_mon1993to2024_surTSOpH_v3.nc"))  ## keeping the same file outline as the 2024 interpolation
snc_lon <- as.vector(ncvar_get(snc_dat, "lon_rho"))
snc_lat <- as.vector(ncvar_get(snc_dat, "lat_rho"))
svar <- as.vector(ncvar_get(snc_dat, "temp", count = c(-1, -1, 1)))
nc_close(snc_dat)

# entire curvilinear rectangle
sdat <- data.frame(x = snc_lon, y = snc_lat, value = svar) %>%
  st_as_sf(coords = c("x", "y"), crs = "EPSG:4326") %>%
  st_transform(crs = "EPSG:3005")

# perimeter outline of datapoints with values (islands not shown - Haida gwaii, van island)
sroms_cave <- sdat %>%
  na.omit() %>%
  concaveman::concaveman()

# outline of all datapoints (includes islands and coastline) - uses 2km buffer around each point (of raw ROMS data)
# could increase buffer - what would that do?
if(!exists("sroms_buff")){
  sroms_buff <- sdat %>%
    na.omit() %>%
    st_geometry() %>%
    st_buffer(dist = 2000) %>%
    st_union() %>%
    st_as_sf()
}

rm(snc_dat, snc_lon, snc_lat, svar, sdat)
# END parameters
#####

for(i in 1:length(nc_filenames)){
  this_filename <- nc_filenames[i]
  
  devtools::load_all()                 # Else get error in not finding %>%
  
  nc_dat <- ncdf4::nc_open(paste0(pacea_dir,
                                  "/data-raw/roms/bccm-raw-data/", raw_dir, "/",
                                  this_filename))
  
  # load lon-lat and mask layers from netcdf
  nc_lon <- as.vector(ncdf4::ncvar_get(nc_dat, "lon_rho"))
  nc_lat <- as.vector(ncdf4::ncvar_get(nc_dat, "lat_rho"))
  
  # depth from file name
  if(substr(this_filename, startchar, endchar) %in% c("bot", "sur")){
    ti <- substr(this_filename, startchar, endchar)
    if(ti == "bot") {ti <- "bottom"} else {ti <- "surface"}
  } else {
    ti <- strsplit(substr(this_filename, startchar, nchar(this_filename)), "_")[[1]][1]
  }
  
  nc_var <- ncdf4::ncvar_get(nc_dat, "temp")
  
  ## IMPORTANT - REPLACE NEGATIVE AND INFINITE VALUES WITH NA (for oxygen)
  nc_var[which(is.infinite(nc_var))] <- NA
  
  # if(length(which(nc_var < 0)) > 0) {
  #   stop("negative values in data")
  # }
  
  nc_varmat <- apply(nc_var, 3, c)
  
  
  # put into dataframe and sf object
  dat <- data.frame(x = nc_lon, y = nc_lat) %>% cbind(nc_varmat)
  dat_sf <- sf::st_as_sf(dat,
                         coords = c("x", "y"),
                         crs = "EPSG:4326")
  tdat_sf <- sf::st_transform(dat_sf,
                              crs = "EPSG: 3005")[,1]
  
  rm(nc_lon, nc_lat, dat, dat_sf, nc_var, nc_varmat)
  
  # create polygon for cropping ROMS data
  roms_cave <- tdat_sf[,1] %>%
    na.omit() %>%
    concaveman::concaveman()
  roms_buff <- tdat_sf[,1] %>%
    na.omit() %>%
    sf::st_geometry() %>%
    sf::st_buffer(dist = 2000) %>%
    sf::st_union() %>%
    sf::st_as_sf()
  
  rm(tdat_sf)
  
  
  # subset file list by depth
  sub.bccm.files <- bccm.files[grep(ti, bccm.files)]
  
  if(ti == "surface"){
    sub.bccm.files <- c(sub.bccm.files, 
                        bccm.files[grep("phytoplankton", bccm.files)],
                        bccm.files[grep("primaryproduction", bccm.files)])
  }
  
  for(j in 1:length(sub.bccm.files)){
    fpath <- sub.bccm.files[j]
    
    load(paste0(dpath, fpath))
    
    # data object name
    dataname <- substr(fpath, 1, nchar(fpath) - 7)
    
    # rename data
    int.dat <- get(dataname)
    
    # split string of fpath
    fpathstring <- strsplit(fpath, "_")
    
    # depth string name
    tdepth <- fpathstring[[1]][2]
    
    # variable string name
    tvar <- fpathstring[[1]][3]
    if(tvar %in% c("u", "v")){
      tvar <- paste(fpathstring[[1]][c(3,4)], collapse = "_")
    }
    if(tdepth %in% c("phytoplankton", "primaryproduction")){
      tvar <- fpathstring[[1]][2]
    }
    
    # mask using raw data shape
    temp.dat <- int.dat[roms_cave,]
    temp.dat <- temp.dat[roms_buff,]
    
    # mask using bc_coast shapefile?
    # temp.dat <- st_filter(temp.dat, tbc, .predicate = Negate(st_intersects)) 
    
    # assign data original name 
    assign(dataname, temp.dat)
    
    # crerate filename
    filename <- paste0(dpath, dataname, "_", version, ".rds")
    
    # save object. when loaded, it will have the same object name
    do.call("save", list(as.name(dataname), file = filename, compress = "xz"))
    
    # remove files that aren't needed
    rm(list = c(dataname))
    rm(fpath, fpathstring, tdepth, tvar, dataname, int.dat, temp.dat, filename)
    
  }
  
  nc_close(nc_dat)
  
  rm(this_filename, ti, sub.bccm.files)
  
}



#####
#
# Correcting surface temperatures values
#

# Quite a few cells with teperature values that are extremely low (<-5). 
# Will be using a threshold of -1.9 and put those values as NA

# Other variables also have low outlier values, but they are feasible values. High outliers tend to be within a reasonable range
# See "pacea/data-raw/roms/bccmto2025_interpolated_plots.pdf" for the plots

library(devtools)
library(dplyr)
library(sf)
library(ggplot2)

sf_use_s2(FALSE)  # remove spherical geometry (s2) for sf operations

# load pacea
load_all()

# pacea-data path
dpath <- "../pacea-data/data-bccm-full/"


version <- "03"

## bccm
# file path
fpath <- "bccm_surface_temperature_full_03.rds"

# data object name
dataname <- substr(fpath, 1, nchar(fpath) - 7)

# split string of fpath
fpathstring <- strsplit(fpath, "_")

# depth string name
tdepth <- fpathstring[[1]][2]

# variable string name
tvar <- fpathstring[[1]][3]
if(tvar %in% c("u", "v")){
  tvar <- paste(fpathstring[[1]][c(3,4)], collapse = "_")
}
if(tdepth %in% c("phytoplankton", "primaryproduction")){
  tvar <- fpathstring[[1]][2]
}

# load data
load(paste0(dpath, fpath))

# rename data
tdat <- get(dataname)

# create vector of all values to get quantiles
tdat_vec <- as.vector(as.matrix(unname(st_drop_geometry(tdat))))
summary(tdat_vec)

## we see values well below freezing, even for seawater. Will use -1.9 as the cutoff threshold


threshold <- -1.9

# 1. Identify all time columns (everything except the geometry column)
time_cols <- setdiff(names(tdat), attr(tdat, "sf_column"))

# 2. Replace values below -1.9 with NA across all time columns
cleaned_tdat <- tdat %>%
  mutate(across(all_of(time_cols), ~ ifelse(.x < threshold, NA_real_, .x)))



### Save original data
# assign data original name 
assign(dataname, tdat)

# crerate filename
filename <- paste0(dpath, dataname, "_", version, "originalwithoutliers.rds")

# save object. when loaded, it will have the same object name
do.call("save", list(as.name(dataname), file = filename, compress = "xz"))



### Save new data without outliers
# assign data original name 
assign(dataname, cleaned_tdat)

# crerate filename
filename <- paste0(dpath, dataname, "_", version, ".rds")

# save object. when loaded, it will have the same object name
do.call("save", list(as.name(dataname), file = filename, compress = "xz"))




