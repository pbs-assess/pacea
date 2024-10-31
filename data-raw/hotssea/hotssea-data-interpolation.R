# TODO this will save the one I already processed, but in a different
# directory (pacea-data/data-hotssea/ instead of data/hotssea). Maybe check
# they're equal, see mask code.

# Andy starting again from Travis's roms-data-interpolation.R, incorporating Greig's additions as I go
# along (easier to follow what was commented out by Greig).

# HOTSSea data from Greig Oldford - Salinity and Temperature 1980-2018
# model depths vary by grid cell, usually only slightly at the surface.
# Fields are therefore processed accounting for varying depth level spans.

# Run this from the data-raw/hotssea/ directory

# Filenames that we save to (as .rds in pacea-data/) do need to match what we're calling them in pacea, but can have
#  version number at the end. So must be in hotssea_data object. And want them
#  somewhat automated from the filenames Greig used.

# To clarify, there are:
#  - .nc files as saved by Greig. Must have one of salinity or temperature in
#   the filename, and that is what is contained in the data (unlike BCCM which had
#   multiple variables in one .nc file). Currently have _1980to2018 in filename
#   which gets removed automatically here to create <object_name>.
#  - .rds files that get saved to pacea-data/ with filename <object_name>-01.rds for
#       version number, with 'avg' added in compared to Greig's filenames to be
#       consistent with Travis's
#  - <object_name> when downloaded from pacea-data/ and loaded into R using
#   <object_name>()
#  - object names must then be added to hotssea_data object

library(devtools)
library(dplyr)
library(terra)
library(gstat)
library(sf)
library(stars)
library(ncdf4)
library(ggplot2)
library(concaveman)
library(stringr)
library(parallel)
library(foreach)

sf_use_s2(FALSE)  # remove spherical geometry (s2) for sf operations
load_all()        # load pacea


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
                         "/../pacea-data/data-hotssea/")  # Where to save .rds files
               # Lots of code in get-pacea-data.R would need changing to add a
               # hotssea-data directory, which was the original plan. Hard to
               # test all that and time consuming, so putting it all in one directory.
               # Will probably upload to Zenodo and not commit them to
               #  pacea-data/ as it's getting big. But that's a separate step to
               #  making them here.


#####
# START - load data to environment

# transform bc_coast
tbc <- bc_coast

# convert to multilinestring
tbc.line <- st_cast(tbc, "MULTILINESTRING")


#####
# PARAMETERS


# OPTION 1 FOR LOOPING THROUGH VARIABLES FOR EACH DEPTH
# loop variables
# Absolute path of directory where .nc files are saved, change version number here.
nc_dir <- paste0(pacea_dir,
                 "/data-raw/hotssea/hotssea-version-1.02.4")

nc_filenames <- list.files(nc_dir,
                           pattern = ".nc")
nc_filenames

## ifiles <- list.files("hotssea-version-2", pattern = ".nc")
# names within .nc for each variable:
jvars <- c("votemper", "vosaline")   # French since NEMO

# index table
vars_fullname <- c("temperature",
                   "salinity")      # these are in the .nc filenames
vars_units <- c("Temperature (\u00B0C)",
                "Salinity (PSU)")

# Table of variables, full names, and units for plotting
jvars_table <- cbind(jvars,
                     vars_fullname,
                     vars_units)
jvars_table

# function argument
llnames <- c("x", "y")
nmax <- 4

# version of data update in pacea-data/, not the same as the version number
# Greig has used when uploading to Zenodo. For that see the nc_filenames call
# above and below.
version <- "01"

# processing times output
# proctimes <- vector()

# surface mask layer
surf_nc_dat <- nc_open(paste0(nc_dir,
                              "/hotssea_1980to2018_surface_temperature_mean.nc"))
surf_nc_lon <- as.vector(ncvar_get(surf_nc_dat, "nav_lon"))
surf_nc_lat <- as.vector(ncvar_get(surf_nc_dat, "nav_lat"))
surf_var <- as.vector(ncvar_get(surf_nc_dat, "votemper", count = c(-1, -1, 1)))
summary(surf_var)
# For surface, now have:
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#  1.467   5.570   6.678   6.541   7.782   9.911   30087

# Might need this if have 0's still
# svar[svar == 0] <- NA                  # 0's are NA's (e.g. on land), so set to
                                       # NA here. Now there are NA's and no 0's but maybe
                                       # not always so check.

# Adapting Greig's automated suggestion
surf_nc_time_counter <- ncvar_get(surf_nc_dat, "time_counter")
surf_nc_time_dates <- as.POSIXct(surf_nc_time_counter,
                                 origin = "1900-01-01",
                                 tz = "UTC")

# Removes leading zeros for months, so matches Travis's style.
cnames <-
  stringr::str_glue("{lubridate::year(surf_nc_time_dates)}_{lubridate::month(surf_nc_time_dates)}") %>%
  as.vector()
cnames

# These are points
surf_dat <- data.frame(x = surf_nc_lon,
                       y = surf_nc_lat,
                       value = surf_var) %>%
  st_as_sf(coords = c("x", "y"),
           crs = "EPSG:4326") %>%
  st_transform(crs = "EPSG:3005")

# plot(surf_dat) # looks good
expect_equal(summary(surf_var),
             summary(surf_dat$value))   # have only changed
                                        # co-ordinate system so far

surf_hotssea_cave <- surf_dat %>%
  na.omit() %>%
  concaveman::concaveman()
  # plot(surf_hotssea_cave)  # used to plot as a rectangle-ish when we had 0's
  # (that we interpreted as real values), but now a concave
  # outline around everything (one single outline, no islands) because we've used NA's

# usethis::use_data(surf_hotssea_cave, overwrite = TRUE)   # save for now to check
                                        # with domain of bccm, then delete (maybe).
                                        # Doing that comparison in
                                        # make-mask-layer.full.R - looks like
                                        # not needed now.
# Plot two ways.
ggplot() +
  geom_sf(data = bc_coast) +
  geom_sf(data = surf_hotssea_cave, col = NA, fill = "red")

ggplot() +
  geom_sf(data = surf_hotssea_cave, col = NA, fill = "red") +
  geom_sf(data = bc_coast)



## mask with coastline, I think this is kind of a fix as we've used surface info
surf_hotssea_buff <- surf_dat %>%
  na.omit() %>%
  st_geometry() %>%
  st_buffer(dist = 1500) %>%
  st_union() %>%
  st_as_sf()
  # plot(surf_hotssea_buff) # This is now the non-NA values (when they were 0's
  # it loked like hotssea_cave, and so shows islands.

#usethis::use_data(surf_hotssea_buff, overwrite = TRUE)   # save for now to check
                                        # with domain of bccm, then delete (maybe).
                                        # Doing that comparison in make-mask-layer.full.R
                                        # Looks like not used.

usethis::use_data(surf_dat, overwrite = TRUE)   # save for now to check
                                        # with domain of bccm, then delete maybe.
                                        # Doing that comparison in
                                        # make-mask-layer.full.R - is used
                                        # there, so best to keep. Maybe rename
                                        # it to something more descriptive.

nc_close(surf_nc_dat)
# rm(snc_dat, snc_lon, snc_lat, svar, sdat)

# END parameters
#####


# Loop through all files, saving to an .rds (so nothing output from each loop)
foreach(i = 1:length(nc_filenames)) %dopar% {
  this_filename <- nc_filenames[i]

  devtools::load_all()                 # Else get error in not finding %>%
  # Automatically create object name, and then the variable name to extract
  obj_name <- stringr::str_replace(this_filename, "1980to2018_", "") %>%
    stringr::str_replace(".nc", "")

  stopifnot(!(stringr::str_detect(this_filename, "temperature") &
              stringr::str_detect(this_filename, "salinity")))

  if(stringr::str_detect(this_filename, "temperature")){
    j <- "votemper"
  }

  if(stringr::str_detect(this_filename, "salinity")){
    j <- "vosaline"
  }

  nc_dat <- ncdf4::nc_open(paste0(nc_dir,
                           "/",
                           this_filename))

  # load lon-lat and mask layers from netcdf
  nc_lon <- as.vector(ncdf4::ncvar_get(nc_dat, "nav_lon"))
  nc_lat <- as.vector(ncdf4::ncvar_get(nc_dat, "nav_lat"))

  # start <- Sys.time()

  nc_var <- ncdf4::ncvar_get(nc_dat, j)
  nc_varmat <- apply(nc_var, 3, c)

  nc_varmat[nc_varmat == 0] <- NA                  # 0's are NA's (e.g. on land), so set to
  # NA here. Though Greig now has NA's in there.

  # Put sst into dataframe and sf object
  dat <- data.frame(x = nc_lon,
                    y = nc_lat) %>%
    cbind(nc_varmat)
  dat_sf <- sf::st_as_sf(dat,       # NOTE stars::st_as_sf exists also, not sure
                                    # which Travis used.
                     coords = c("x", "y"),
                     crs = "EPSG:4326")    # Okay for Greig's
  tdat_sf <- sf::st_transform(dat_sf,      # ALSO exists stars::st_transform()
                          crs = "EPSG: 3005")                 # BC Albers

  # Calculations earlier were for surface to give the surf_hotssea_cave and
  # surf_hotssea_buff that get used below. Presume they're still needed. Also
  # surface calculations give the cnames (column names.
  # Then here inside the loop
  # they're done for each object (because deep ones won't have the same coverage).
  # Some looks like overkill but I think the conversions were needed to get to the
  # same format (presumably these could just use the surface ones).
  hotssea_cave <- tdat_sf %>%
    na.omit() %>%
    concaveman::concaveman()

  hotssea_buff <- tdat_sf %>%
    na.omit() %>%
    sf::st_geometry() %>%
    sf::st_buffer(dist = 1500) %>%
    sf::st_union() %>%
    sf::st_as_sf()

  # This took 6 minutes with hotssea_buff, but need the full poly to spatial
  #  geometries match up with bccm, for Elise and Philina (2 is for 2x2 km,
  #  which is all we're doing here though):
  tictoc::tic()
  output2 <- point2rast(data = tdat_sf,
                        spatobj = bccm_hotssea_poly,
                        loc = llnames,
                        cellsize = 2000,
                        nnmax = nmax,
                        as = "SpatRast")
  tictoc::toc()   # 1.3 hours for 18/10/24 version, 1.24 for 29/10/24 with 2000
                  # cellsize not 1500
  # This is a "SpatRaster" object, doesn't plot well
  # plot(output2) # with roms_buff gave fancy artwork. Looks wrong but could be the
  # plotting as it's a SpatRaster.
  # plot(output2)  - now gives something sensible, using my new hotssea_poly

  # crop out grid cells with polygon masks
  t2_sf <- output2 %>%
    terra::mask(hotssea_poly) %>%      # Back to hotssea_poly
    stars::st_as_stars() %>%  ## check here for converting to points (not raster)
    sf::st_as_sf()

  # This is needed as we used hotssea_poly above, and t2_sf has 31951 features.
  #  This may not all be needed, but just
  #  leave in as it's all a bit subtle and Travis spent a lot of time figuring it out.
  ##### BC MASK OPTION 2 - Using roms outline
  # 1. use roms_cave
  t2_sfb <- t2_sf[hotssea_cave, ]

  # 2. use roms_buff to get haida gwaii outline and shore - do, as likely
  # needed for elsewhere:
  t2_sfb <- t2_sfb[hotssea_buff,]

  # 3. use default surface roms_cave
  t2_sfb <- t2_sfb[surf_hotssea_cave,]

  # 4. use default surface roms_buff
  t2_sf <- t2_sfb[surf_hotssea_buff,]    # Carefull, going back to t2_sf
  # With NA stuff is now:
  # Simple feature collection with 10731 features and 468 fields
  # Whereas was:
  # Simple feature collection with 34515 features and 468 fields
  # Geometry type: POLYGON

  # assign column names as year_month
  names(t2_sf)[1:(ncol(t2_sf) - 1)] <- cnames

  # round to 6 decimal places to reduce file size
  t3_sf <- t2_sf %>%
    sf::st_drop_geometry() %>%
    round(digits = 6) %>%
    sf::st_as_sf(geometry = sf::st_geometry(t2_sf))

  class(t3_sf) <- c("pacea_st",
                    "sf",
                    "tbl_df",
                    "tbl",
                    "data.frame")

  # assign units attribute
  attr(t3_sf, "units") <- jvars_table[which(jvars_table[, 1] == j), 3]
  attr(t3_sf, "restrict_plotting_range") <- TRUE      # To then use to automatically restrict
  # the plotting
  attr(t3_sf, "salinity_unit") <- "PSU"      # To automate the axes labels

  filename <- paste0(pacea_data_dir,
                     obj_name,
                     "_",
                     version,
                     ".rds")
  assign(obj_name, t3_sf)

  do.call("save", list(as.name(obj_name), file = filename, compress = "xz"))
  # 10/10/24 version was 13.9 Mb. Okay to add to pacea-data I think.
  # 19/10/24 version is 13.2Mb.

  # Don't worry about timing, probably for when Travis was testing.
  # end <- Sys.time()
  # jtime <- end-start
  # print(jtime)
  # names(jtime) <- paste(depth_range_i, tj, sep="_")
  # proctimes <- c(proctimes, jtime)

  # Manually add names to data-raw/data-key/hotssea_data_list.csv  TODO

  # Save memory:
  ncdf4::nc_close(nc_dat)
  rm(nc_dat,
     nc_lon,
     nc_lat,
     nc_var,
     nc_varmat,
     dat,
     dat_sf,
     tdat_sf,
     hotssea_cave,
     hotssea_buff,
     output2,
     t2_sf,
     t2_sfb,
     t3_sf,
     obj_name)
  gc()
}

parallel::stopCluster(cl = my_cluster)
