# interpolate to a 2km and 6km grid
##  should it be interpolated for just the extent of inshore and offshore, respectively? Could save a bit of time and every bit counts


library(devtools)
library(terra)
library(gstat)
library(sf)
library(ncdf4)

sf_use_s2(FALSE)  # remove spherical geometry (s2) for sf operations

# load pacea
load_all()

# load functions from data-raw/roms folder
source("./data-raw/roms/point2rast_interpolation.R")


#####
# START - load data to environment

# pacea - rnaturalearth bc coast
data(bc_coast)

# pacea - PBSdata eez
data(bc_eez)

# pacea - romseez polygon
data(romseez_poly)

# open ncdf and load sst data
nc_dat <- nc_open("Roms_bcc42_mon_2008to2011_sst.nc")
nc_sstarray <- ncvar_get(nc_dat, "sst")
nc_sstmat <- apply(nc_sstarray, 3, c)

# load lon-lat and mask layers from netcdf
nc_lon <- as.vector(ncvar_get(nc_dat, "lon_rho"))
nc_lat <- as.vector(ncvar_get(nc_dat, "lat_rho"))
nc_mask <- as.vector(ncvar_get(nc_dat, "mask_rho"))

# put sst into dataframe and sf object
sst_dat <- data.frame(x = nc_lon, y = nc_lat) %>% cbind(nc_sstmat)
sst_sf <- st_as_sf(sst_dat, coords = c("x", "y"), crs = "EPSG:4326")
tsst_sf <- st_transform(sst_sf, crs = "EPSG: 3005")

# END load data
#####


#####
# PARAMETERS

dat <- tsst_sf[, c(1:4)]
sobj <- romseez_poly
llnames <- c("x", "y")
res <- 10000
nmax <- 4

# END parameters
#####


#####
# MAIN PROGRAM

output <- point2rast(data = dat, spatobj = sobj, loc = llnames, cellsize = res, nnmax = nmax,
                     as = "SpatVect")

## NEXT: write function to process data to our 2km and 6km grid







