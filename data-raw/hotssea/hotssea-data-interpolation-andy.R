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
dir <- here::here()   # Will give pacea/

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
snc_dat <- nc_open(paste0(dir, "/data-raw/hotssea/hotssea_1980to2018_monthly_0to4m_tempsalin_avg.nc"))
snc_lon <- as.vector(ncvar_get(snc_dat, "nav_lon"))
snc_lat <- as.vector(ncvar_get(snc_dat, "nav_lat"))
svar <- as.vector(ncvar_get(snc_dat, "votemper", count = c(-1, -1, 1)))

sdat <- data.frame(x = snc_lon, y = snc_lat, value = svar) %>%
  st_as_sf(coords = c("x", "y"), crs = "EPSG:4326") %>%
  st_transform(crs = "EPSG:3005")

surf_hotssea_cave <- sdat %>%
  na.omit() %>%
  concaveman::concaveman()
  # plots as a rectangle-ish

## mask with coastline, I think this is kind of a fix as we've used surface info
surf_hotssea_buff <- sdat %>%
  na.omit() %>%
  st_geometry() %>%
  st_buffer(dist = 1500) %>%
  st_union() %>%
  st_as_sf()
# Kind of looks the same on the plot, actually that could just be a rescaling

rm(snc_dat, snc_lon, snc_lat, svar, sdat)
# END parameters
#####


for(i in ifiles[1]){   # ifiles) {  # TODO put back in for all of them
# i <- ifiles[1]   # for running line by line
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

  for(j in c("votemper")){   # jvars) {   # TODO put back in for loop
    # j <- "votemper"  # for running line-by-line
    start <- Sys.time()

    nc_var <- ncvar_get(nc_dat, j)
    nc_varmat <- apply(nc_var, 3, c)

    # put sst into dataframe and sf object
    dat <- data.frame(x = nc_lon,
                      y = nc_lat) %>%
      cbind(nc_varmat)
    dat_sf <- st_as_sf(dat,
                       coords = c("x", "y"),
                       crs = "EPSG:4326")    # Okay for Greig's
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

    output2 <- point2rast(data = tdat_sf,
                          spatobj = hotssea_poly,
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
    # Simple feature collection with 34515 features and 468 fields
    # Geometry type: POLYGON

    # BCCM:
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


    # assign pacea class

    # Travis had this, but I think safer to do line after since it isn't a tibble
    class(t3_sf) <- c("pacea_st", "sf", "tbl_df", "tbl", "data.frame")

# This works here while still just an sf object:
# plot(tdat_sf_cropped_2, cex = 0.6, pch = 16)

# class(tdat_sf_cropped_2) <- c("pacea_st",
#                              class(tdat_sf_cropped_2))


    # assign units attribute
    attr(t3_sf, "units") <- jvars_table[which(jvars_table[, 1] == j), 3]


# doesn't work though, using plot.pacea_st(), even after doing th cropping
# etc. above.
# AHA think because variable names are slightly different
# plot(tdat_sf_cropped_2, cex = 0.6, pch = 16)


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
    assign(objname, t3_sf)

    do.call("save", list(as.name(objname), file = filename, compress = "xz"))

    end <- Sys.time()
    jtime <- end-start
    print(jtime)
    names(jtime) <- paste(ti, tj, sep="_")
    proctimes <- c(proctimes, jtime)

    # remove files
    # TODO update this:
    rm(dat, dat_sf, tdat_sf, roms_cave, roms_buff,
       output2, output6, t2_sf2, t2_sf6, t2_sf26,
       t2_sf26a, t2_sf26b, t3_sf26, nc_var, nc_varmat)
    rm(list = objname)
    gc()
  }
}

HERE, need to get the bc_coast back in, but in a way that it doesn't expand the axes.
plot.pacea_st_hotssea(hotssea_0to10m_temperature, bc = TRUE, eez = FALSE,
                      months.plot = "June", years.plot = 2013) # :2018)


tTODO
Greig: Might work to creat the times [or see cnames above]

                          time_counter <- ncvar_get(snc_dat, "time_counter")
time_dates <- as.POSIXct(time_counter, origin = "1900-01-01", tz = "UTC")
time_formatted <- format(time_dates, "%Y-%m")


TODO:
This is for plotting:
Elise:
coord <- ggplot2::coord_sf(xlim = c(-125.5, -122.9), ylim = c(48.1, 49.44), crs = sf::st_crs(4326)) and this can be added to the end of a ggplot call to crop to your desired coordinates
extent() or create_bb(-123.4, -123.1, 48.32...

extent() or create_bb(-123.4, -123.1, 48.32, 48.5)
to get the range of a spatial object by Keppel, Elise (DFO/MPO)
--
Philina's:
 coord_sf(xlim = range(dset$longitude, na.rm = TRUE),
           ylim = range(dset$latitude, na.rm = TRUE), expand = FALSE) +

ex <- raster::extent(hotssea_0to10m_temperature)   # extent

--
My googling:
tt <- plot.pacea_st_hotssea(hotssea_0to10m_temperature, bc = FALSE, eez = FALSE, months.plot = "June", years.plot = 2013)

Then extract ranges,
ggplot_build(ttt)$layout$panel_scales_x[[1]]$range$range

Then add coast and force axis.
---

  Combining ideas:

  tt + geom_sf(data = bc_coast, fill = "darkgrey") +
 coord_sf(xlim = c(raster::extent(hotssea_poly)@xmin,
                    raster::extent(hotssea_poly)@xmax),
           ylim = c(raster::extent(hotssea_poly)@ymin,
                    raster::extent(hotssea_poly)@ymax), expand = FALSE) # Already a bit of a buffer I think
# That worked, just now want to avoid extent, this gives the same (tiny tiny bit
# different, but fine):
tt <- plot.pacea_st_hotssea(hotssea_0to10m_temperature, bc = FALSE, eez = FALSE,
                            months.plot = "June", years.plot = 2013)
  tt + geom_sf(data = bc_coast, fill = "darkgrey") +
    coord_sf(xlim = ggplot_build(ttt)$layout$panel_scales_x[[1]]$range$range,
             ylim = ggplot_build(ttt)$layout$panel_scales_y[[1]]$range$range,
             expand = FALSE)

Panel:

tt <- plot.pacea_st_hotssea(hotssea_0to10m_temperature, bc = FALSE, eez = FALSE,
                            months.plot = "June", years.plot = 2013:2018)
  tt + geom_sf(data = bc_coast, fill = "darkgrey") +
    coord_sf(xlim = ggplot_build(ttt)$layout$panel_scales_x[[1]]$range$range,
             ylim = ggplot_build(ttt)$layout$panel_scales_y[[1]]$range$range,
             expand = FALSE)
