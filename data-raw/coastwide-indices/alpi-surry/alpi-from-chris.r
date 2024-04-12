# This is a source file named ALPI.r
# Updated for R 3.1.2
# January 12, 2015
############################################################
# Functions to calculate the annual value for the Aleutian
# Low Pressure Index (ALPI).
#
# ALPI is used as in Beamish et al. (1997), modified from Beamish
# and Bouillon (1993): For each month from December to March,
# the average area of the ocean covered by the low less than
# 100.5 kPa is estimated. The average for the 4 months is
# subtracted from the longerm (1950-1997) average, and the
# resulting anomaly is used as the yearly index for the
# January year.
#
# Based on Compugrid method developed by Michael Folkes in
# 1998, which was then adapted for use in ESRI ArcMap by
# Rob Flemming and Karin Mathias in 2003.
# This version written by Maria Surry in October 2012, and
# updated in January 2015.
#
# Content was derived with the help of the documentation
# associated with each package, as well past contributions
# on the R-SIG-Geo mailing list at
# https://stat.ethz.ch/mailman/listinfo/R-SIG-Geo/.
# Correct citations for R and for packages used can be obtained
# using the function citation().
#
# These functions work on data you have already downloaded.
#
# The data for this study are from the Research Data Archive
# RDA) which is maintained by the Computational and Information
# Systems Laboratory (CISL) at the National Center for
# Atmospheric Research (NCAR). NCAR is sponsored by the National
# Science Foundation (NSF). The original data are available from
# the RDA (http://rda.ucar.edu) in dataset number ds010.1.
#
# Access is free, but you do have to register.
#
# From the RDA website, under the data access tab, select
# "Get a subset". In "Date Range" select December to March.
# For example, for 2011, select 201012 for Start Date (December)
# and 201103 for End Date (March). Select NetCDF for the output
# format.
############################################################
# remove all objects in the working directory
#rm(list=ls())
# attach necessary libraries
library(RNetCDF)
library(gstat)
library(raster)
library(rgdal)

# library(sp) # loaded automatically with 'raster"
############################################################
get.alpi.nc <- function(file) {
  # function to open a NetCDF file and parse it into dec to mar
  # sea level pressure data, subsetted to the correct extent
  library(RNetCDF)
  file.nc <- open.nc(file)
  #retrieve the relevant variables - all are arrays
  # *longitude is 0-355 degrees, in 5 degree increments (72 elements)
  # *latitude is 15-90 degrees, in 5 degree increments (16 elements)
  # *each 72 x 16 element is one month of data
  lon.original <- var.get.nc(file.nc,"lon") #longitude (72x1)
  lat.original <- var.get.nc(file.nc,"lat") #latitude (16x1)
  slp.original <- var.get.nc(file.nc,"slp") #sea level pressure 72x16x4)
  date.time <- var.get.nc(file.nc,"date_time") #month (4x1)
  close.nc(file.nc)
  # Subset to the extent from Beamish & Bouillon (1993)
  lon <- lon.original[25:49] # longitude 120-240° (120°E-120°W)
  lat <- lat.original[2:12] # latitude 20-70°N
  # divide into months
  dec.slp <- slp.original[25:49,2:12,1] #december (25x11)
  jan.slp <- slp.original[25:49,2:12,2] #january (25x11)
  feb.slp <- slp.original[25:49,2:12,3] #february (25x11)
  mar.slp <- slp.original[25:49,2:12,4] #march (25x11)
  # Glue the variables into one data frame for each month, with
  # columns for longitude, latitude, and sea level pressure.
  y <- rep(lat,length(lon))
  x <- rep(lon, length(lat))
  x <- as.vector(t(matrix(x,nrow=length(lon))))
  # Name each data frame by the corresponding month
  dec <- data.frame(x,y,z=as.vector(t(dec.slp)))
  dec.name <- paste("December",substr(date.time[1],1,4))
  jan <- data.frame(x,y,z=as.vector(t(jan.slp)))
  jan.name <- paste("January",substr(date.time[2],1,4))
  feb <- data.frame(x,y,z=as.vector(t(feb.slp)))
  feb.name <- paste("February",substr(date.time[3],1,4))
  mar <- data.frame(x,y,z=as.vector(t(mar.slp)))
  mar.name <- paste("March",substr(date.time[4],1,4))
  # Combine data frames into list
  data <- list(dec,jan,feb,mar)
  names(data) <- c(dec.name,jan.name,feb.name,mar.name)
  return(data)
}
############################################################
slp.idw <- function(month,cs=40){
  # Function to turn point observations of monthly sea level pressure
  # into a raster using inverse distance weighted (IDW) interpolation.
  # This function works on a single month of data - e.g. one element
  # of the list created by file.nc above.
  library(gstat)
  library(raster)
  library(rgdal)
  #turn ordinary data frame into a spatial object (package 'sp')
  data.new <- month[!is.na(month$z),] # get rid of NAs first
  coordinates(data.new) <- ~ x + y # turns into SpatialPointsDataFrame
  #
  # assign a coordinate system (CRS = Coordinate Reference System)
  26
  # set up CRS strings
  wgs84 <- c("+proj=longlat +datum=WGS84") #WGS84
  nad83 <- c("+proj=longlat +datum=NAD83") #NAD83
  wb <- c("+proj=cea +lat_ts=30 +lon_0=180") # World Berhmann
  aea <- c("+proj=aea +lat_1=30 +lat_2=60 +lat_0=52 +lon_0=-170 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #NPac Albers Equal Area
  # proj4string(data.new) <- CRS(wgs84)
  proj4string(data.new) <- CRS(nad83)
  #
  # Data requires an equal-area projection to measure area without
  # distortion
  #
  # World Berhmann projection with central meridian at 180 degrees
  # (used by Karin Mathias in ESRI method)
  # data.new <- spTransform(data.new, CRS(wb))
  #
  # North Pacific Albers Conic Equal Area projection with standard
  # parallels at 30 and 60 and central meridian is -170 degrees.
  # From www.stateofthesalmon.org, available at www.spatialreference.org
  # This looks similar to Beamish & Bouillon (1993) figures.
  data.new <- spTransform(data.new, CRS(aea))
  #
  # Make a regular grid that has the same extent as the dataset
  # (note: You can't just make the dataset gridded, because as soon as
  # you project the regular lat/lon grid into cartesian coordinates, it's
  # not regular any more.) Pick a cell size (resolution) that gives
  # results closest to original values.
  grd <- spsample(data.new,type="regular",offset = c(0.5,0.5),
                  cellsize=c(cs*1000,cs*1000))
  gridded(grd) <- TRUE
  # Interpolate the data into the new grid, using IDW (gstat)
  # nmax is the number of nearest observations to use, idp is the power
  # Output is spatial pixels dataframe which can be converted into
  # a raster.
  data.idw <- idw(z~1,data.new,grd,nmax=16,idp=1)
  # convert to raster -note: this is the interpolated raster for one
  # month of sea level pressure data
  data.ras <- raster(data.idw)
  return(data.ras)
}
############################################################
month.val <- function(month.ras) {
  # Function to calculate the area of the Aleutian Low (area that is less
  # than or equal to 1005 hPa) - i.e. the monthly value
  # Output is a single value for the area.
  library(raster)
  pred.slp <- values(month.ras) #get interpolated values
  r <- res(month.ras) # raster resolution (cell size)
  # Find the area with sea level pressure less than 1005 hPa:
  area <- length(pred.slp[pred.slp<=1005])*r[1]*r[2]
  return(area)
}
############################################################
alpa <- function(vals) {
  # Function to convert a list of monthly values for one ALPI year into
  # a vector, and to calculate the average Aleutian Low for that year
  colnames <- c("December","January","February","March","Average")
  27
  monthly <- c(vals,sum(vals)/4)
  names(monthly) <- colnames
  return(monthly)
}
############################################################
alpi.val <- function(lows,lt.mean) {
  # Function to compute the annual anomaly from the long-term mean.
  # 'lows' is a vector or dataframe with the Dec-Mar values for
  # the area of the Aleutian Low.
  # The long-term mean must be specified.
  # Output is the index value in km^2 x 10^6 for each year supplied.
  avg <- sum(lows)/4
  ind.val <- (avg-lt.mean)/1000000/1000000
  names(ind.val) <- "ALPI"
  return(ind.val)
}
############################################################
month.reclass <- function(month.ras) {
  # Function to reclassify the area less than 1005 hPa and return
  # the reclassified raster. Output is a raster with just
  # the area < 1005. This is the same as the output of the "reclassify"
  # function in ArcMap Spatial Analyst.
  pred.slp <- values(month.ras) #get interpolated values
  # Find the area with sea level pressure less than 1005 hPa:
  # reclassify the area < 1005 as "1"
  # reclassify all other values to NA
  m <- c(0, 1005, 1, 1005, max(pred.slp), NA)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  data.reclass <- reclassify(month.ras, rclmat)
  return(data.reclass)
}
############################################################
get.alpa.ctr <- function(month.ras) {
  # Function to find the coordinates of the centre of the Aleutian Low
  # Pressure Area (ALPA).
  # First reclassifies the raster using month.reclass, so that all
  # values in the raster > 100.5 kPa are NA, and all values <= 100.5 kPa
  # are converted to "1".
  # Then processes the output from month.reclass by finding "clumps" of area
  # that are <= 100.5 kPa: sometimes there is just one clump; other times
  # there are small additional clumps which we ignore.
  #
  # First find the clumps and figure out the area of each so that
  # you can pick the biggest one.
  library(raster)
  library(rgdal)
  #
  month.rcl <- month.reclass(month.ras)
  month.cl <- clump(month.rcl)
  cl.vals <- values(month.cl)
  if (all(is.na(cl.vals))) {
    # if there is no ALPA for this month, return with no coordinates
    coord.ctr <- c(NA,NA,NA,NA)
    return(coord.ctr)
  }
  r <- res(month.cl) # raster resolution (cell size)
  counts <- data.frame(freq(month.cl,useNA='no')) # no. cells per clump
  28
  alpa.id <- counts$value[which.max(counts$count)] # biggest clump is ALPA
  #
  # eliminate ALPAs where the clumps are too similar in size to
  # unambiguously pick the biggest one
  clump.max <- counts$count[alpa.id]
  clump.other <- sum(counts$count)-clump.max
  if (0.1*clump.max<clump.other){
    coord.ctr <- c(NA,NA,NA,NA)
    return(coord.ctr)
  }
  #
  # Now find the coordinates associated with the ALPA. Put them in a
  # dataframe so that you can find the min and max of the cartesian
  # coordinates. The mean of these will be the centre point of the
  # ALPA.
  #
  xy <- xyFromCell(month.cl,1:ncell(month.cl)) # get coordinates
  df <- data.frame(xy, cl.vals, is.alpa = month.cl[] %in% alpa.id)
  df <- df[df$is.alpa == T, ] # only want coordinates for ALPA
  x <- (min(df$x)+max(df$x))/2
  y <- (min(df$y)+max(df$y))/2
  #
  #Now convert the x,y (cartesian) into lat,long (geographic). First set
  # up a Coordinate Refernce System (CRS), turn the x,y into a spatial
  # object with CRS = NPac Albers, and then convert to NAD83.
  nad83 <- c("+proj=longlat +datum=NAD83") #NAD83
  aea <- c("+proj=aea +lat_1=30 +lat_2=60 +lat_0=52 +lon_0=-170 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #NPac Albers
  #
  alpa.ctr <- SpatialPoints(matrix(c(x,y), nrow=1), proj4string=CRS(aea))
  alpa.ctr.geo <- spTransform(alpa.ctr, CRS=CRS(nad83))
  coord.ctr <- cbind(coordinates(alpa.ctr.geo),coordinates(alpa.ctr))
  colnames(coord.ctr) <- c("lon","lat","x","y")
  return(coord.ctr) # return the latitude, longitude, x, and y coords
}
############################################################
seasonal.ctr <- function(data.rcl.list) {
  # Function to find centers for the monthly and seasonal Aleutian Lows
  # Accepts a list of rasters for each year (Dec, Jan, Feb, Mar)
  monthly.coords <- sapply(data.rcl.list,get.alpa.ctr) # monthly centers
  #
  # Use the cartesian coordinates to do the averaging. First set up a
  # Coordinate Refernce System (CRS), turn the xy's into a spatial object
  # with CRS = NPac Albers, and then convert to NAD83.
  nad83 <- c("+proj=longlat +datum=NAD83") #NAD83
  aea <- c("+proj=aea +lat_1=30 +lat_2=60 +lat_0=52 +lon_0=-170 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #NPac Albers
  #
  x <- mean(monthly.coords[3,],na.rm=T)
  y <- mean(monthly.coords[4,],na.rm=T)
  alpa.ctr <- SpatialPoints(matrix(c(x,y), nrow=1), proj4string=CRS(aea))
  alpa.ctr.geo <- spTransform(alpa.ctr, CRS=CRS(nad83))
  seasonal.ctr <- cbind(coordinates(alpa.ctr.geo),coordinates(alpa.ctr))
  coord.ctr <- cbind(monthly.coords,t(seasonal.ctr))
  rownames(coord.ctr) <- c("lon","lat","x","y")
  # get the January year and use to create a colname for the new coords
  year <- as.numeric(strsplit(dimnames(monthly.coords)[[2]]," ")[[2]][2])
  29
  colnames(coord.ctr)[5] <- paste("Seasonal",year)
  return(coord.ctr) # return the latitude, longitude, x, and y coords
}
