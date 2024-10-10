# Andy starting again from Travis's roms-data-interpolation.R, incorporating Greig's additions as I go
# along (easier to follow what was commented out by Greig). Copy this over to
# hottsee-data-interpolation.R when finalised.

# HOTSSea data from Greig Oldford - Salinity and Temperature 1980-2018
# model depths vary by grid cell, usually only slightly at the surface.
# Fields are therefore processed accounting for varying depth level spans.

# Run this from the data-raw/hotssea/ directory

# Filenames that we save to (as .rds in pacea-data/) do need to match what we're calling them in pacea, but can have
#  version number at the end. So must be in hotssea_data object. And want them
#  somewhat automated from the filenames Greig used.

# To clarify, there are:
#  - .nc files as saved by Greig
#  - .rds files that get savd to pacea-data/ with filename <object_name>-01.rds for
#       version number
#  - <object_name> when downloaded from pacea-data/ and loaded into R using <object_name>()

library(devtools)
library(dplyr)
library(terra)
library(gstat)
library(sf)
library(stars)
library(ncdf4)
library(ggplot2)
library(concaveman)

sf_use_s2(FALSE)  # remove spherical geometry (s2) for sf operations

# load pacea
load_all()
pacea_dir <- here::here()   # Will give local pacea/
pacea_data_dir <- paste0(here::here(),
                         "/../pacea-data/data/")  # Where to save .rds files
               # Lots of code in get-pacea-data.R would need changing to add a
               # hotssea-data directory, which was the original plan. Hard to
               # test all that and time consuming, so putting it all in one directory.


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
                 "/data-raw/hotssea/hotssea-version-1.02.3")

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
proctimes <- vector()

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

surf_dat <- data.frame(x = surf_nc_lon,
                       y = surf_nc_lat,
                       value = surf_var) %>%
  st_as_sf(coords = c("x", "y"),
           crs = "EPSG:4326") %>%
  st_transform(crs = "EPSG:3005")

# plot(sdat) # looks good
expect_equal(summary(surf_var),
             summary(surf_dat$value))   # have only changed
                                        # co-ordinate system so far

surf_hotssea_cave <- surf_dat %>%
  na.omit() %>%
  concaveman::concaveman()
  # plot(surf_hotssea_cave)  # used to plot as a rectangle-ish when we had 0's
  # (that we interpreted as real values), but now a concave
  # outline around everything (one single outline, no islands) because we've used NA's

## mask with coastline, I think this is kind of a fix as we've used surface info
surf_hotssea_buff <- surf_dat %>%
  na.omit() %>%
  st_geometry() %>%
  st_buffer(dist = 1500) %>%
  st_union() %>%
  st_as_sf()
  # plot(surf_hotssea_buff) # This is now the non-NA values (when they were 0's
  # it loked like hotssea_cave, and so shows islands.

# TODO put back in when possible
# rm(snc_dat, snc_lon, snc_lat, svar, sdat)
# END parameters
#####


for(i in nc_filenames[1]{  # TODO put back in for all of them
 i <- nc_filenames[2]   # for running line by line, doing temp
 nc_dat <- nc_open(paste0(nc_dir,
                          "/",
                          i))

  # load lon-lat and mask layers from netcdf
  nc_lon <- as.vector(ncvar_get(nc_dat, "nav_lon"))
  nc_lat <- as.vector(ncvar_get(nc_dat, "nav_lat"))

  # depth from file name
  # hotssea_1980to2018_monthly_0to4m_tempsalin_avg.nc
  # This was Travis's, will have to tailor, the 33,35 looks a bit risky TODO
  ## if(substr(i, 33, 35) %in% c("bot", "sur")){
  ##   ti <- substr(i, 33, 35)
  ##   if(ti == "bot") {ti <- "bottom"} else {ti <- "surface"}
  ## } else {
  ##   ti <- strsplit(substr(i, 33, nchar(i)), "_")[[1]][1]
  ## }
  # temp hardcode till fix above - GO
  depth_range_i <- "avg0to30m"   # was ti but too cryptic  TODO automate this

 # Greig has temp and salinity in separate files, so we can make this
 # simpler. Don't think need the j loop, just check filename and do votemper
 # etc. depending on the filename. So somewhat automatic.

 HERE HERE - automate some of this
  #for(j in c("votemper")){   # jvars) {   # TODO put back in for loop
     j <- "votemper"  # for running line-by-line
    start <- Sys.time()

    nc_var <- ncvar_get(nc_dat, j)
    nc_varmat <- apply(nc_var, 3, c)

    nc_varmat[nc_varmat == 0] <- NA                  # 0's are NA's (e.g. on land), so set to
                                       # NA here.


    # put sst into dataframe and sf object
    dat <- data.frame(x = nc_lon,
                      y = nc_lat) %>%
      cbind(nc_varmat)
    dat_sf <- st_as_sf(dat,
                       coords = c("x", "y"),
                       crs = "EPSG:4326")    # Okay for Greig's
 tdat_sf <- st_transform(dat_sf,
                         crs = "EPSG: 3005")                 # BC Albers

 # Gives this, where 39468 = 132 cells x 299 cells I think no need to crop
 #> tdat_sf
 # Simple feature collection with 39468 features and 468 fields
 # Geometry type: POINT
 # Dimension:     XY
 # Bounding box:  xmin: 972441.8 ymin: 209502.4 xmax: 1351568 ymax: 676720.4
 # Projected CRS: NAD83 / BC Albers
# First 10 features:
#   1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
#1  0 0 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#2  0 0 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#3  0 0 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0

# plot(tdat_sf) gives 9 plots, and warning: plotting the first 9 out of 468
# attributes; use max.plot = 468 to plot all. But maps of first 9 look different
# to each other. Must be differences further down. Attributes are columns
    # (fields) I think corresponding to months, since 1980 to 2018 monthly, and
    # (2018-1980)*12 = 456.

    # 0's may be the masking?

# plot(dat_sf) and plot(tdat_sf) look pretty similar, slightly rotated
# (different projection)

# Andy tried manually to crop with this, but should jsut have used same approach
# of cave and buffer, so commenting these out:
## stopifnot(ncol(tdat_sf) == 469)    # if fails then change 468 below, note we
##                                    # remove geometry column

## tdat_sf_tib <- tibble::as_tibble(tdat_sf) %>%
##   dplyr::select(-c("geometry")) %>%
##   dplyr::rowwise() %>%
##   dplyr::mutate(min = min(c_across(`1`:`468`)),
##                 max = max(c_across(`1`:`468`)))  # Takes a few minutes

## # This is stuff Andy added that I think messes things up and is why it doesn't plot
## only_zeros_indices <- which(tdat_sf_tib$min == 0 & tdat_sf_tib$max == 0)  # length(only_zeros_indices) is 30086
## tdat_sf_cropped <- tdat_sf[-only_zeros_indices, ]
## tdat_sf_cropped
## dim(tdat_sf_cropped)
## plot(tdat_sf_cropped)   # works

# This was for ROMS, so adapting now:
# create polygon for cropping ROMS data
# The s Travis had is for surface, I hadn't noticed that. So putting surf_ in
# ones above. Then here inside the loop
# they're done for each object (because deep ones won't have the same coverage).
# Some looks lke overkill but I think the conversions were needed to get to the
# same format.
    hotssea_cave <- tdat_sf %>%
      na.omit() %>%
      concaveman::concaveman()

    hotssea_buff <- tdat_sf %>%
      na.omit() %>%
      st_geometry() %>%
      st_buffer(dist = 1500) %>%
      st_union() %>%
      st_as_sf()

# This is what takes a few minutes (maybe 10):    TODO trying hotssea_buff not
# hotssea_poly, since don't need full rectangle (might speed it up?)
    output2 <- point2rast(data = tdat_sf,
                          spatobj = hotssea_buff,  #poly,
                          loc = llnames,
                          cellsize = 1500,       # Want 1500 not 2000
                          nnmax = nmax,
                          as = "SpatRast")

    # This is a "SpatRaster" object, doesn't plot well
    #plot(output2) # with roms_buff gave fancy artwork. Looks wrong but could be the
    # plotting as it's a SpatRaster.
    # plot(output2)  - now gives something sensible, using my new hotssea_poly

    # 6 km res, not needed here
    #    output6 <- point2rast(data = tdat_sf, spatobj = offshore_poly, loc = llnames, cellsize = 6000, nnmax = nmax, as = "SpatRast")

# crop out grid cells with polygon masks
    t2_sf <- output2 %>%
      terra::mask(hotssea_poly) %>%  #      mask(inshore_poly) %>%
      stars::st_as_stars() %>%  ## check here for converting to points (not raster)
      st_as_sf()

    # mask gives a warning but I think still uses terra::mask:
    # Warning message:
    #  In findGeneric(f, envir) :
    # 'mask' is a formal generic function; S3 methods will not likely be found

    # Not needed presumably:
    #    t2_sf6 <- output6 %>%
    #      mask(bccm_eez_poly) %>%
    #      mask(offshore_poly) %>%
    #      stars::st_as_stars() %>%
    #     st_as_sf()

    # plot(t2_sf2)  nope, each does look the same though

    # TODO not sure if this step adds anything in beyond just combining the grids
    # mask 2k grid with 6k grid, then combine grids
#    t2_sf26a <- t2_sf2[!st_intersects(st_union(t2_sf6), t2_sf2, sparse=FALSE, prepared=TRUE),] %>%
#      rbind(t2_sf2[st_intersects(st_union(t2_sf6), t2_sf2, sparse=FALSE, prepared=TRUE),]) %>%
#      rbind(t2_sf6)


 # TODO might not need this now as already masked above; TODO check if thiese
 # actually change from step to step, and if not then comment out.
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

    # Simple feature collection with 34515 features and 468 fields
    # Geometry type: POLYGON

    # BCCM:
    # Now has 10731 cells, [nrow(t2_sf) =  10731], because have excluded the
    # zeros. Previously had the rectangular grid:
    # data should have 41,288 grid cells
    # if(nrow(t2_sf26) != 41288){
    #   out.msg <- paste0(as.symbol(t2_sf26), " nrows = ", nrow(get(objname)),
    #                     ". nrows not equal to 13,377,312...wrangle to long format (or somethinig else) failed.")
    #   stop(out.msg)
    # }

    # assign column names as year_month
    names(t2_sf)[1:(ncol(t2_sf) - 1)] <- cnames

    # round to 6 decimal places to reduce file size
    t3_sf <- t2_sf %>%
      st_drop_geometry() %>%
      round(digits = 6) %>%
      st_as_sf(geometry = st_geometry(t2_sf))

 # TODO comment me out, this is for testing saving of .nc file and loading into
 # pacea, without being too big
 t3_sf <- dplyr::select(t3_sf,
                        c(`2018_4`:`2018_5`,
                          "geometry"))

    # assign pacea class

    # Travis had this, but I think safer to do line below since it isn't
    #  actually a tibble, though I saw somewhere else it needed these in the
    #  right order.
    class(t3_sf) <- c("pacea_st",
                      "sf",
                      "tbl_df",
                      "tbl",
                      "data.frame")

# This works here while still just an sf object:
# plot(tdat_sf_cropped_2, cex = 0.6, pch = 16)

# class(tdat_sf_cropped_2) <- c("pacea_st",
#                              class(tdat_sf_cropped_2))


    # assign units attribute
    attr(t3_sf, "units") <- jvars_table[which(jvars_table[, 1] == j), 3]
    attr(t3_sf, "restrict_plotting_range") <- TRUE      # To then use to automatically restrict
                                        # the plotting
    attr(t3_sf, "salinity_unit") <- "PSU"      # To automate the axes labels

# doesn't work though, using plot.pacea_st(), even after doing th cropping
# etc. above.
# AHA think because variable names are slightly different
# plot(tdat_sf_cropped_2, cex = 0.6, pch = 16)


 HERE - adapt this to be objname and filename somewhat automatically - see HERE
 above also
    # name file and write data
    tj <- jvars_table[which(jvars_table[, 1] == j), 2]
#    if(depth_range_i == "zInt"){
#      objname <- paste("hotssea", tj, sep = "_")  # TODO
#    } else {
      objname <- paste("hotssea", depth_range_i, tj, "mean", sep = "_")    # TODO add in mean/min/max automatically
#    }

# TODO filenames do need to match what we're calling them in pacea, but can have
# version number at the end. So must be in hotssea_data object.

 filename <- paste0(pacea_data_dir,
                    objname,
                    "_",
                    version,
                    ".rds")
    #filename <- paste0("../pacea-data/data/",objname, ".rds")
    assign(objname, t3_sf)

    do.call("save", list(as.name(objname), file = filename, compress = "xz"))

# summary(t3_sf)   # TODO has no 0's, but a few low looking values so look into

    end <- Sys.time()
    jtime <- end-start
    print(jtime)
    names(jtime) <- paste(depth_range_i, tj, sep="_")
    proctimes <- c(proctimes, jtime)

    # Manually add names to data-raw/data-key/hotssea_data_list.csv
    # remove files
    # TODO update this:
    #rm(dat, dat_sf, tdat_sf, roms_cave, roms_buff,
    #   output2, output6, t2_sf2, t2_sf6, t2_sf26,
    #   t2_sf26a, t2_sf26b, t3_sf26, nc_var, nc_varmat)
    # rm(list = objname)   # TODO add back in when have saved and reloaded .rds
    gc()
  }
# }
