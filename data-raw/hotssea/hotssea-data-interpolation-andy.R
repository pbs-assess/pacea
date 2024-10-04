# Andy starting again from Travis's roms-data-interpolation.R, incorporating Greig's additions as I go
# along (easier to follow what was commented out by Greig). Copy this over to
# hottsee-data-interpolation.R when finalised.

# HOTSSea data from Greig Oldford - Salinity and Temperature 1980-2018
# model depths vary by grid cell, usually only slightly at the surface
# fields are therefore processed accounting for varying depth level spans.

# Run this from the data-raw/hotssea/ directory

# BCCM was ROMs output from Angelina Pena - full bottom and surface variables that Andy requested

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


#####
# START - load data to environment

# transform bc_coast
tbc <- bc_coast

# convert to multilinestring
tbc.line <- st_cast(tbc, "MULTILINESTRING")

# Travis had this all commented out:
# #####
# # create polygons for cropping to roms data
# nc_dat <- nc_open(paste0("data-raw/roms/bcc42_era5glo12r4_mon1993to2019_botTSO.nc"))
#
# # load lon-lat and mask layers from netcdf
# nc_lon <- as.vector(ncvar_get(nc_dat, "lon_rho"))
# nc_lat <- as.vector(ncvar_get(nc_dat, "lat_rho"))
#
# nc_var <- ncvar_get(nc_dat, "temp")
# nc_varmat <- apply(nc_var, 3, c)
#
# # put sst into dataframe and sf object
# dat <- data.frame(x = nc_lon, y = nc_lat) %>% cbind(nc_varmat) %>%
#   st_as_sf(coords = c("x", "y"), crs = "EPSG:4326") %>%
#   st_transform(crs = "EPSG: 3005")
#
# # create polygon for cropping ROMS data
# roms_cave <- dat %>%
#   na.omit() %>%
#   concaveman::concaveman()
# roms_buff <- dat %>%
#   na.omit() %>%
#   st_geometry() %>%
#   st_buffer(dist = 5000) %>%
#   st_union()
#
# rm(nc_dat, nc_lon, nc_lat, nc_var, nc_varmat, dat)
# #####


#####
# PARAMETERS


# OPTION 1 FOR LOOPING THROUGH VARIABLES FOR EACH DEPTH
# loop variables
ifiles <- list.files(pattern = "tempsalin_avg.nc")
jvars <- c("votemper", "vosaline")   # French since NEMO

# index table
vars_fullname <- c("temperature",
                   "salinity")
vars_units <- c("Temperature (\u00B0C)",
  #  "Temperature (potential; \u00B0C)",  TODO Greig had this, need to tweak
  #   plot_pacea_st() though
                "Salinity (PSU)")
jvars_table <- cbind(jvars,
                     vars_fullname,
                     vars_units)

# OPTION 2 FOR LOOPING THROUGH ONLY SURFACE VARIABLES (PRIMARY PRODUCTION)
# loop variables - won't be needed for hotssea

# function argument
llnames <- c("x", "y")
nmax <- 4

# column names
cnames <- paste(rep(1980:2018, each=12),    # TODO double check model starts in
                                        # Jan; see end of file for Greig's suggestion
                1:12,
                sep="_")

# version of data update
version <- "01"

# processing times output
proctimes <- vector()

# surface mask layer
snc_dat <- nc_open("hotssea_1980to2018_monthly_0to4m_tempsalin_avg.nc")
snc_lon <- as.vector(ncvar_get(snc_dat, "nav_lon"))
snc_lat <- as.vector(ncvar_get(snc_dat, "nav_lat"))
svar <- as.vector(ncvar_get(snc_dat, "votemper", count = c(-1, -1, 1)))

sdat <- data.frame(x = snc_lon, y = snc_lat, value = svar) %>%
  st_as_sf(coords = c("x", "y"), crs = "EPSG:4326") %>%
  st_transform(crs = "EPSG:3005")

sroms_cave <- sdat %>%
  na.omit() %>%
  concaveman::concaveman()
sroms_buff <- sdat %>%
  na.omit() %>%
  st_geometry() %>%
  st_buffer(dist = 2000) %>%     # TODO make that 1500?
  st_union() %>%
  st_as_sf()

rm(snc_dat, snc_lon, snc_lat, svar, sdat)
# END parameters
#####


# for(i in ifiles) {
i <- ifiles[1]


  nc_dat <- nc_open(i)

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
  ti = "0to10m"

#  for(j in jvars) {
j <- "votemper"
    start <- Sys.time()

    nc_var <- ncvar_get(nc_dat, j)
    nc_varmat <- apply(nc_var, 3, c)

    # put sst into dataframe and sf object
    dat <- data.frame(x = nc_lon, y = nc_lat) %>% cbind(nc_varmat)
    dat_sf <- st_as_sf(dat, coords = c("x", "y"), crs = "EPSG:4326")    # Okay for Greig's
    tdat_sf <- st_transform(dat_sf, crs = "EPSG: 3005")                 # BC Albers

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
stopifnot(ncol(tdat_sf) == 469)    # if fails then change 468 below, note we
                                   # remove geometry column

tdat_sf_tib <- tibble::as_tibble(tdat_sf) %>%
  dplyr::select(-c("geometry")) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(min = min(c_across(`1`:`468`)),
                max = max(c_across(`1`:`468`)))  # Takes a few minutes

only_zeros_indices <- which(tdat_sf_tib$min == 0 & tdat_sf_tib$max == 0)  # length(only_zeros_indices) is 30086
tdat_sf_cropped <- tdat_sf[-only_zeros_indices, ]
tdat_sf_cropped
dim(tdat_sf_cropped)
plot(tdat_sf_cropped)   # works


# BCCM is eventually saved as a polygon. So think need some of this to create
# polygons for ssea. Can plot all these:

  # create polygon for cropping ROMS data.
    roms_cave <- tdat_sf %>%
      na.omit() %>%
      concaveman::concaveman()     # creates outline of a set of points, this is
                                   # basically a rectangle
#
    roms_cave_cropped <- tdat_sf_cropped %>%
      na.omit() %>%
      concaveman::concaveman()     # this is the concave outline of everything (since cropped)


    roms_buff <- tdat_sf_cropped %>%
      na.omit() %>%
      st_geometry() %>%
      st_buffer(dist = 1500) %>%
      st_union() %>%
      st_as_sf()                   # This is the kind of buffer outline, more detailed

    # interpolate data
    # 2 km res
#THIS MIGHT CRASH as inshore_poly shouldn't ovelap domain of hotssea HERE
#    output2 <- point2rast(data = tdat_sf, spatobj = inshore_poly, loc = llnames, cellsize = 2000, nnmax = nmax, as = "SpatRast")
output2 <- point2rast(data = tdat_sf_cropped,
                      spatobj = roms_buff,   # Travis had inshore_poly
                      loc = llnames,
                      cellsize = 1500,       # Want 1500 not 2000
                      nnmax = nmax,
                      as = "SpatRast")



# 6 km res
#    output6 <- point2rast(data = tdat_sf, spatobj = offshore_poly, loc = llnames, cellsize = 6000, nnmax = nmax, as = "SpatRast")

 crop out grid cells with polygon masks
    t2_sf2 <- output2 %>%
#      mask(bccm_eez_poly) %>%
#      mask(inshore_poly) %>%
      stars::st_as_stars() %>%  ## check here for converting to points (not raster)
      st_as_sf()
#    t2_sf6 <- output6 %>%
#      mask(bccm_eez_poly) %>%
#      mask(offshore_poly) %>%
#      stars::st_as_stars() %>%
#     st_as_sf()

hello

    # mask 2k grid with 6k grid, then combine grids
#    t2_sf26a <- t2_sf2[!st_intersects(st_union(t2_sf6), t2_sf2, sparse=FALSE, prepared=TRUE),] %>%
#      rbind(t2_sf2[st_intersects(st_union(t2_sf6), t2_sf2, sparse=FALSE, prepared=TRUE),]) %>%
#      rbind(t2_sf6)


    ##### BC MASK OPTION 1 - Using bc shapefile
    # index points that dont intersect with bc coast shapefile
    #  disjoint - do not share space
    # dis2 <- t2_sf26[st_disjoint(st_union(tbc), t2_sf26, sparse=FALSE, prepared=TRUE),]
    #
    # #  convert bc coast to sf linestring and finding coastline intersections separately - increased processing speed
    # #  using st_intersects is much faster than other predicate functions
    # sub.t2 <- t2_sf26[st_intersects(st_union(tbc), t2_sf26, sparse=FALSE, prepared=TRUE),]
    # inter.line <- sub.t2[st_intersects(tbc.line, sub.t2, sparse=FALSE, prepared=TRUE),]
    # t2_sf26 <- rbind(dis2, inter.line)

    ##### BC MASK OPTION 2 - Using roms outline
    # 1. use roms_cave
 #   t2_sf26b <- t2_sf26a[roms_cave,]

    # 2. use roms_buff to get haida gwaii outline and shore
#    t2_sf26b <- t2_sf26b[roms_buff,]

    # 3. use default surface roms_cave
#    t2_sf26b <- t2_sf26b[sroms_cave,]

    # 4. use default surface roms_buff
#    t2_sf26 <- t2_sf26b[sroms_buff,]

    # data should have 41,288 grid cells
    # if(nrow(t2_sf26) != 41288){
    #   out.msg <- paste0(as.symbol(t2_sf26), " nrows = ", nrow(get(objname)),
    #                     ". nrows not equal to 13,377,312...wrangle to long format (or somethinig else) failed.")
    #   stop(out.msg)
    # }

    # assign column names as year_month
    names(tdat_sf_cropped)[1:(ncol(tdat_sf_cropped) - 1)] <- cnames

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
#    t3_sf26 <- t2_sf26 %>%
#      st_drop_geometry() %>%
#      round(digits = 6) %>%
#      st_as_sf(geometry = st_geometry(t2_sf26))


tdat_sf_cropped_2 <- tdat_sf_cropped
skipping this for now:  TODO put back in
#    tdat_sf_cropped_2 <- tdat_sf_cropped %>%
#      st_drop_geometry() %>%
#      round(digits = 6) %>%
#      st_as_sf(geometry = st_geometry(tdat_sf_cropped))

    # assign pacea class

    # Travis had this, but I think safer to do line after since it isn't a tibble
    # class(tdat_sf_cropped_2) <- c("pacea_st", "sf", "tbl_df", "tbl", "data.frame")

# This works here while still just an sf object:
plot(tdat_sf_cropped_2, cex = 0.6, pch = 16)

class(tdat_sf_cropped_2) <- c("pacea_st",
                              class(tdat_sf_cropped_2))


    # assign units attribute
    attr(tdat_sf_cropped_2, "units") <- jvars_table[which(jvars_table[, 1] == j), 3]

# doesn't work though, using plot.pacea_st()
plot(tdat_sf_cropped_2, cex = 0.6, pch = 16)


    # name file and write data
    tj <- jvars_table[which(jvars_table[, 1] == j), 2]
    if(ti == "zInt"){
      objname <- paste("hotssea", tj, sep = "_")
    } else {
      objname <- paste("hotssea", ti, tj, sep = "_")    # TODO add in mean
    }
#    filename <- paste0("../pacea-data/data/",objname, "_", version, ".rds")
#    TODO, for now dumping in same folder
    filename <- paste0(objname, "_", version, ".rds")
    #filename <- paste0("../pacea-data/data/",objname, ".rds")
    assign(objname, tdat_sf_cropped_2)

    do.call("save", list(as.name(objname), file = filename, compress = "xz"))

    end <- Sys.time()
    jtime <- end-start
    print(jtime)
    names(jtime) <- paste(ti, tj, sep="_")
    proctimes <- c(proctimes, jtime)

    # remove files
    rm(dat, dat_sf, tdat_sf, roms_cave, roms_buff,
       output2, output6, t2_sf2, t2_sf6, t2_sf26,
       t2_sf26a, t2_sf26b, t3_sf26, nc_var, nc_varmat)
    rm(list = objname)
    gc()
  }
}

Greig: Might work to creat the times [or see cnames above]

                          time_counter <- ncvar_get(snc_dat, "time_counter")
time_dates <- as.POSIXct(time_counter, origin = "1900-01-01", tz = "UTC")
time_formatted <- format(time_dates, "%Y-%m")
