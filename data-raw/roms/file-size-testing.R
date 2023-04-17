# testing file size differences

library(terra)
library(ncdf4)
library(sf)
library(stars)
library(dplyr)

# read ncdf data with nc_open as array
temp_nc <- nc_open("C:/Users/TAIT/Documents/Research/Roms_bcc42_mon_2008to2011_sst.nc") # 38 mb

print(temp_nc)

######
# creating sf object of sst with lat lon
# SST
sst <- ncvar_get(temp_nc, varid = "sst")
sst1 <- sst[,,1]
sst.m <- apply(sst, MARGIN = c(3), FUN = c)
dim(sst.m)

# lon
lon <- ncvar_get(temp_nc, varid = "lon_rho")
lon.v <- c(lon)

# lat
lat <- ncvar_get(temp_nc, varid = "lat_rho")
lat.v <- c(lat)

sst.dat <- as.data.frame(sst.m) %>% 
  mutate(lon = lon.v,
         lat = lat.v) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = "EPSG: 4326")

usethis::use_data(sst.dat) # 12.5mb

#####

# checking rdata file size of dataframe/array
# usethis::use_data(sst)  # 11.5 mb (saves 300kb to store as one file)
# usethis::use_data(sst1) # 244 kb

# converting sst data to vector to eliminate NAs
sst_vec <- sst %>% 
  apply(3,c) # concatenate into single vector by 3rd (layer) dimension
sst_vecNA <- sst_vec %>%
  na.omit()

dim(sst_vec)
summary(sst)  # 1.03M NA cells
summary(sst_vec)
1.03/4.64  # approx proportion of NA cells = 22%

# checking file sizes between vectorized data and without NAs
# usethis::use_data(sst_vec) # 11.5 mb
# usethis::use_data(sst_vecNA) # 11.3 mb

# read ncdf data directly as a raster
nc_all <- terra::rast("C:/Users/TAIT/Documents/Research/Roms_bcc42_mon_2008to2011_sst.nc") 
crs(nc_all) <- "EPSG:4326"
nc_all


#-------------------------------------------------#
# converting array to raster

dim(sst)

sst_rast <- terra::rast(sst)
sst1_rast <- terra::rast(sst[,,1]) ## subset of 1 layer

# set crs
crs(sst_rast) <- "EPSG:4326"

# checking rdata file size of raster
# use_data() (i.e. save) doesn't seem to work when storing SpatRaster class objects
# load_all() (i.e. load) fails when loading in this as an .rda file
# data is lost when saving this way
# usethis::use_data(sst_rast) # 244 kb
# usethis::use_data(sst1_rast) # 244 kb

# saveRDS function preserves raster data and can be loaded with readRDS, but cannot
#  be loaded using loaded as part of package load_all()
terra::saveRDS(sst_rast, file = "./data/sst_rast_saveRDS.rda")


#-------------------------------------------------#
# converting raster to sf (via stars)

sst_starsA <- stars::st_as_stars(sst_rast)
sst_sfA <- sf::st_as_sf(sst_starsA, as_points = T)  # points is marginally smaller than polygons

# checking rdata file size of sf object
usethis::use_data(sst_sfA) # 11.3 mb

plot(sst_sfA[,c(1,49)])


#-------------------------------------------------#
# create example dataframe that will be stored to check file size

# parameters
set.seed(500)
ncell <- 150000   # number of cells
yrs <- 1950:2050  # 100 years of data/projections
mths <- 1:12      # monthly time series
NAsample <- sample(1:ncell,33000)  # portion of cells that are blank (e.g. land)

A <- matrix(rnorm(ncell * length(yrs) * length(mths)), nrow = length(yrs) * length(mths), ncol = ncell)
dat <- tibble(year = rep(yrs, each = length(mths)),
              month = rep(mths, times = length(yrs)))
dat <- dat %>% cbind(A) 
dat[,c(NAsample+2)] <- NA  # assign cells NA value
rm(A)

dim(dat)

# remove date columns and NA columns
Adummy <- dat
Adummy_nodate <- dat[,-c(1:2)]
Adummy_nodateNA <-dat[,-c(NAsample+2)] # approx 22% of cells are NAs (based on ROMs)

# checking file size of data stored as dataframes
# Not much file size savings in removing NA data. 
# usethis::use_data(Adummy)          # 1072 mb
# usethis::use_data(Adummy_nodate)   # 1072 mb
# usethis::use_data(Adummy_nodateNA) # 1071 mb


# converting to sf object - convert to array, raster, add geometries and crs
Adummy_array <- dat[,-c(1,2)] %>% 
  as.matrix() %>%
  t() %>%
  as.vector() %>%
  array(dim = c(300,500,1212))

Adummy_rast <- terra::rast(Adummy_array)
terra::crs(Adummy_rast) <- "EPSG:4326"

Adummy_sf <- stars::st_as_stars(Adummy_rast)
Adummy_sfpoly <- sf::st_as_sf(Adummy_sf)  # points is marginally smaller than polygons
Adummy_sfpoints <- sf::st_as_sf(Adummy_sf, as_points = T)  # points is marginally smaller than polygons
# NOTE: gridded cells (ie. rows when converted to sf) that have no data (NA) 
#  across all layers get omitted automatically

# checking file size
usethis::use_data(Adummy_array)          # 1071 mb
usethis::use_data(Adummy_rast)          # 303 mb - rasters don't work with save/load
usethis::use_data(Adummy_sfpoly)          # 1071 mb
usethis::use_data(Adummy_sfpoints)        # 1071 mb

# Conclusion: there are minimal differences in file size between data types 
#  (e.g. dataframe, array, sf). Data compression handles NAs and doesn't increase file size




#-------------------------------------------------#
#-------------------------------------------------#
##### Loading in data to see if it plotting works without loaded packages

library(devtools)
load_all()

class(oni)
class(sst_sfA)
crsclass(Adummy_sfpoints)


# ROMS sst - class sf
plot(sst_sfA[,c(1,49)])  # can be plotted without loading library(sf)

tdat<-sst_sfA
class(tdat) <- "data.frame"
plot(tdat[,c(1,49)])

# dummy data plotting sf objects 
plot(Adummy_sfpoints[,c(1212:1213)])
plot(Adummy_sfpoints[,c(1211:1213)])


# reading in raster written and stored as .tif
library(terra)
dat <- rast("./data/sst_rastC.tif")
plot(dat,1:2)




