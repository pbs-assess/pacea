# Interpolating the data for the full BCCM grid, not just inshore_poly and
# offshore_poly.
# Copying roms-data-interpolation.R to then edit, based on
#  hotssea-data-interpolation.R in which worked out the parallel stuff.
# ROMs data from Angelina Pena - full bottom and surface variables that Andy requested

# Run with option <- 1 then option <- 2.
# 1/11/24 overnight:
# Full option 1 took 10 hours to do all 20 files. Using 6 cores. pH files all
# got made last, but each depth range is its own file (so pH should have got
# made with the others), so I'm thinking memory got too full and those got
# reserved to the end. So if do again maybe just do 4 cores. CPUE was maxed out,
# and memory still 12Gb used after it finished (I thought things were cleaned
# up). Ah, each R session is still open, I forget to add the closing parallel thing.
# Full option 2:
# Only two files, think took about 2 hours each. Not sure why not parallel though.

# From hotssea-data-interpolation.R, think these ideas still hold, but sticking
# with filename stuff that Travis did for first bccm variables.
# Filenames that we save to (as .rds in pacea-data/data-bccm-full/) do need to match what we're calling them in pacea, but can have
#  version number at the end.

# To clarify, there are:
#  - .nc files as saved by Angelica. Have more than one variable in them
# (unlike hotssea). Greig.
#  - .rds files that get saved to pacea-data/data-bccm-full/ with filename <object_name>-01.rds for
#       version number.
#  - <object_name> when loaded from pacea-data/data-bccm-full and loaded into R using
#   <object_name>()
#  - object names must then be added to bccm_data_full for download functions TODO

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
pacea_data_dir <- paste0(here::here(),
                         "/../pacea-data/data-bccm-full/")  # Where to save .rds
                                        # files. Then uploading to Zenodo, not
                                        # commiting those to GitHub.
# TODO add in mkdir if it's not already there.

#####
# START - load data to environment

# transform bc_coast
tbc <- bc_coast

# convert to multilinestring
tbc.line <- st_cast(tbc, "MULTILINESTRING")

#####
# PARAMETERS

# Do OPTION 1 or 2. Then re-run.
option <- 2

# OPTION 1 FOR LOOPING THROUGH VARIABLES FOR EACH DEPTH
# loop variables
if(option == 1){

  nc_filenames <- list.files(paste0(pacea_dir,
                                    "/data-raw/roms/"),
                             pattern = "TSOpH.nc")
  jvars <- c("temp", "salt", "Oxygen", "pH")

  # index table
  vars_fullname <- c("temperature", "salinity", "oxygen", "pH")
  vars_units <- c("Temperature (\u00B0C)",
                  "Salinity (ppt)",
                  "Dissolved oxygen content (mmol-oxygen m^-3)",
                  "pH")
  jvars_table <- cbind(jvars, vars_fullname, vars_units)
}

# OPTION 2 FOR LOOPING THROUGH ONLY SURFACE VARIABLES (PRIMARY PRODUCTION)
# loop variables
if(option == 2){
  nc_filenames <- list.files(paste0(pacea_dir,
                                    "/data-raw/roms/"),
                             pattern = "zInt_PT.nc")
  jvars <- c("phytoplankton", "PTproduction")

  # index table
  vars_fullname <- c("phytoplankton", "primaryproduction")
  vars_units <- c("Phytoplankton (mmol-nitrogen m^-2)",
                  "Total primary production (gC m^-2 d^-1)")
  jvars_table <- cbind(jvars, vars_fullname, vars_units)
}

# function argument
llnames <- c("x", "y")
nmax <- 4

# column names. See hotssea-data-interpolation.R for automated version (if
# there's a time_counter in the .nc file).
cnames <- paste(rep(1993:2019, each=12), 1:12, sep="_")

# version of data update
version <- "01"

# processing times output
proctimes <- vector()

# surface mask layer
snc_dat <- nc_open(paste0(pacea_dir,
                          "/data-raw/roms/bcc42_era5glo12r4_mon1993to2019_surTSOpH.nc"))
snc_lon <- as.vector(ncvar_get(snc_dat, "lon_rho"))
snc_lat <- as.vector(ncvar_get(snc_dat, "lat_rho"))
svar <- as.vector(ncvar_get(snc_dat, "temp", count = c(-1, -1, 1)))

sdat <- data.frame(x = snc_lon, y = snc_lat, value = svar) %>%
  st_as_sf(coords = c("x", "y"), crs = "EPSG:4326") %>%
  st_transform(crs = "EPSG:3005")

sroms_cave <- sdat %>%
  na.omit() %>%
  concaveman::concaveman()
sroms_buff <- sdat %>%
  na.omit() %>%
  st_geometry() %>%
  st_buffer(dist = 2000) %>%
  st_union() %>%
  st_as_sf()

nc_close(snc_dat)
rm(snc_dat, snc_lon, snc_lat, svar, sdat)
# END parameters
#####

# Loop through all files, saving to an .rds (so nothing output from each loop)
foreach(i = 1:length(nc_filenames)) %dopar% {
  this_filename <- nc_filenames[i]

  devtools::load_all()                 # Else get error in not finding %>%

  nc_dat <- ncdf4::nc_open(paste0(pacea_dir,
                           "/data-raw/roms/",
                           this_filename))

  # load lon-lat and mask layers from netcdf
  nc_lon <- as.vector(ncdf4::ncvar_get(nc_dat, "lon_rho"))
  nc_lat <- as.vector(ncdf4::ncvar_get(nc_dat, "lat_rho"))

  # depth from file name
  if(substr(this_filename, 33, 35) %in% c("bot", "sur")){
    ti <- substr(this_filename, 33, 35)
    if(ti == "bot") {ti <- "bottom"} else {ti <- "surface"}
  } else {
    ti <- strsplit(substr(this_filename, 33, nchar(this_filename)), "_")[[1]][1]
  }

  for(j in jvars) {
    # j <- jvars[1]
    # start <- Sys.time()

    nc_var <- ncdf4::ncvar_get(nc_dat, j)
    nc_varmat <- apply(nc_var, 3, c)

    # put sst into dataframe and sf object
    dat <- data.frame(x = nc_lon, y = nc_lat) %>% cbind(nc_varmat)
    dat_sf <- sf::st_as_sf(dat,
                           coords = c("x", "y"),
                           crs = "EPSG:4326")
    tdat_sf <- sf::st_transform(dat_sf,
                                crs = "EPSG: 3005")

    # create polygon for cropping ROMS data
    roms_cave <- tdat_sf %>%
      na.omit() %>%
      concaveman::concaveman()
    roms_buff <- tdat_sf %>%
      na.omit() %>%
      sf::st_geometry() %>%
      sf::st_buffer(dist = 2000) %>%
      sf::st_union() %>%
      sf::st_as_sf()

    # interpolate data
    # 2 km res
    # output2 <- point2rast(data = tdat_sf, spatobj = inshore_poly, loc = llnames, cellsize = 2000, nnmax = nmax, as = "SpatRast")
    # 6 km res
    # output6 <- point2rast(data = tdat_sf, spatobj = offshore_poly, loc = llnames, cellsize = 6000, nnmax = nmax, as = "SpatRast")
  # 2x2 on full domain

    # Took maybe an hour for temp, avg0to40m. Should be the same for any though I
    # think. Think it's doing over the whole grid, even interpolating over land. For
    # which there isn't tons. 200,000 cellsize took about 4 mins for each
    # complete file.
    output2_full <- point2rast(data = tdat_sf,
                               spatobj = bccm_hotssea_poly,
                               loc = llnames,
                               cellsize = 2000,
                               nnmax = nmax,
                               as = "SpatRast")
    ## > output2_full
    ## class       : SpatRaster
    ## dimensions  : 710, 651, 324  (nrow, ncol, nlyr)
    ## resolution  : 2000, 2000  (x, y)
    ## extent      : 108882.9, 1410883, -165260, 1254740  (xmin, xmax, ymin, ymax)
    ## coord. ref. : NAD83 / BC Albers (EPSG:3005)
    ## source(s)   : memory
    ## names       :         1,        2,        3,         4,         5,         6, ...
    ## min values  :  4.572711, 5.013850, 5.028921,  5.877672,  7.549509,  9.270278, ...
    ## max values  : 10.229637, 9.604419, 9.817786, 10.668157, 12.789614, 13.939012, ...
    ## > dim(output2_full)
    ## [1] 710 651 324


    # crop out grid cells with polygon masks
    #    t2_sf2 <- output2 %>%
    #      mask(bccm_eez_poly) %>%
    #      mask(inshore_poly) %>%
    #     stars::st_as_stars() %>%  ## check here for converting to points (not raster)
    #      st_as_sf()

    # Quick:
    t2_sf2_full <- output2_full %>%
      #      mask(bccm_eez_poly) %>%
      #      mask(inshore_poly) %>%
      stars::st_as_stars() %>%  ## check here for converting to points (not raster)
      sf::st_as_sf()


    ## t2_sf6 <- output6 %>%
    ##   mask(bccm_eez_poly) %>%
    ##   mask(offshore_poly) %>%
    ##   stars::st_as_stars() %>%
    ##   st_as_sf()

    # _full - not sure if need any of this, don't think so
    # mask 2k grid with 6k grid, then combine grids
    #    t2_sf26a <- t2_sf2[!st_intersects(st_union(t2_sf6), t2_sf2, sparse=FALSE, prepared=TRUE),] %>%
    #      rbind(t2_sf2[st_intersects(st_union(t2_sf6), t2_sf2, sparse=FALSE, prepared=TRUE),]) %>%
    #      rbind(t2_sf6)



    # Still don't understand the reason for four here:
    #dim(t2_sf2_full)
    #> [1] 462210    325

    # 1. use roms_cave TODO change that name plus some of these
    t2_sf26b <- t2_sf2_full[roms_cave,]
    #> dim(t2_sf26b)
    # > [1] 164454    325

    # 2. use roms_buff to get haida gwaii outline and shore
    t2_sf26b <- t2_sf26b[roms_buff,]
    #dim(t2_sf26b)
    #[1] 161025    325

    # 3. use default surface roms_cave
    t2_sf26b <- t2_sf26b[sroms_cave,]
    # same dim
    # 4. use default surface roms_buff
    t2_sf26 <- t2_sf26b[sroms_buff,]
    # same dim

    # data should have 41,288 grid cells
    # if(nrow(t2_sf26) != 41288){
    #   out.msg <- paste0(as.symbol(t2_sf26), " nrows = ", nrow(get(obj_name)),
    #                     ". nrows not equal to 13,377,312...wrangle to long format (or somethinig else) failed.")
    #   stop(out.msg)
    # }

    # assign column names as year_month
    names(t2_sf26)[1:(ncol(t2_sf26) - 1)] <- cnames

    # covert to long format data - Don't do long format as it is too big
    # t3_sf26 <- t2_sf26 %>%
    #   tidyr::pivot_longer(cols = !last_col(), cols_vary = "slowest", names_to = "date", values_to = "value")  %>%
    #   mutate(year = substr(date, 1, 4),
    #          month = substr(date, 6, 7)) %>%
    #   dplyr::select(-date) %>%
    #   relocate(value, .after = last_col()) %>%
    #   relocate(geometry, .after = last_col())

    # round to 6 decimal places to reduce file size
    # t3_sf26[, "value"] <- t3_sf26$value %>% # for long format
    #   round(digits = 6)
    t3_sf26 <- t2_sf26 %>%
      sf::st_drop_geometry() %>%
      round(digits = 6) %>%
      sf::st_as_sf(geometry = sf::st_geometry(t2_sf26))

    # assign pacea class
    class(t3_sf26) <- c("pacea_st", "sf", "tbl_df", "tbl", "data.frame")

    # assign units attribute
    attr(t3_sf26, "units") <- jvars_table[which(jvars_table[, 1] == j), 3]

    attr(t3_sf26, "bccm_full") <- TRUE  # To then use to automatically
                                        # extend the plotting

    # name file and write data
    tj <- jvars_table[which(jvars_table[, 1] == j), 2]
    if(ti == "zInt"){
      obj_name <- paste("bccm", tj, "full", sep = "_")
    } else {
      obj_name <- paste("bccm", ti, tj, "full", sep = "_")
    }

    filename <- paste0(pacea_data_dir,
                       obj_name,
                       "_",
                       version,
                       ".rds")

    assign(obj_name, t3_sf26)

    # Doesn't actually save as .rds file, but sticking with Travis's
    # conventions.

    do.call("save", list(as.name(obj_name), file = filename, compress = "xz"))

    # filesize is 120Mb. Ugh. 4x the existing ones., took 2 hours to process (TODO
    # close .nc files like Andrea said). sum(is.na(...)) is 0. So efficiently
    # saved.

    #end <- Sys.time()
    #jtime <- end-start
    #print(jtime)
    #names(jtime) <- paste(ti, tj, sep="_")
    #proctimes <- c(proctimes, jtime)

    # remove files
    rm(dat, dat_sf, tdat_sf, roms_cave, roms_buff,
       output2, output6, t2_sf2, t2_sf6, t2_sf26,
       t2_sf26a, t2_sf26b, t3_sf26, nc_var, nc_varmat)
    rm(list = obj_name)
    gc()
  }
  ncdf4::nc_close(nc_dat)
}

parallel::stopCluster(cl = my_cluster)
