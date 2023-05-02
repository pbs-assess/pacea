# interpolate to a 2km and 6km grid
##  should it be interpolated for just the extent of inshore and offshore, respectively? Could save a bit of time and every bit counts


library(devtools)
library(dplyr)
library(terra)
library(gstat)
library(sf)
library(stars)
library(ncdf4)
library(ggplot2)

sf_use_s2(FALSE)  # remove spherical geometry (s2) for sf operations

# load pacea
load_all()

# load functions from data-raw/roms folder
source("./data-raw/roms/point2rast_interpolation.R")


#####
# START - load data to environment

# pacea - PBSdata eez
data(bc_eez)

# pacea - romseez polygon
data(romseez_poly)

# pacea - rnaturalearth bc coast
data(bc_coast)
tbc <- st_transform(bc_coast, crs=crs(romseez_poly))

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
res <- 6000
nmax <- 4

# END parameters
#####


#####
# MAIN PROGRAM

# interpolate sst data 
output <- point2rast(data = dat, spatobj = sobj, loc = llnames, cellsize = res, nnmax = nmax,
                     as = "SpatRast")


#####
# TEST 1.1 - interpolate to spatvector then convert to sf before masking
#  processing time to run code below: 44.36 s

##
start <- Sys.time()
# 2 km res
res <- 2000
output2 <- point2rast(data = dat, spatobj = sobj, loc = llnames, cellsize = res, nnmax = nmax,
                      as = "SpatVect")

# 6 km res
res <- 6000
output6 <- point2rast(data = dat, spatobj = sobj, loc = llnames, cellsize = res, nnmax = nmax,
                     as = "SpatVect")

# crop out grid cells with polygon masks
sf_m2 <- st_as_sf(output2) %>% 
  st_filter(romseez_poly) %>% 
  st_filter(inshore_poly) %>% 
  st_as_sf()

sf_m6 <- st_as_sf(output6) %>% 
  st_filter(romseez_poly) %>%
  st_filter(offshore_poly)
sf_m6 <- sf_m6[!st_intersects(sf_m6, st_union(sf_m2), sparse=F),] %>% 
  st_as_sf()

# combine grids
sf_m26 <- sf_m2 %>% rbind(sf_m6)

# index points that dont intersect with bc coast shapefile
sf_m26 <- sf_m26[!st_intersects(sf_m26, tbc, sparse=F),]
# names(sf_m26)[1:4] <- c("A","B","C","D")
# sf_m26

end <- Sys.time()
end-start
##


# plot
ggplot() +
  geom_sf(data=sf_m26[,1], aes(col=A)) +
  geom_sf(data=tbc) + 
  geom_sf(data=bc_eez, fill=NA) 


#####
# TEST 2 - interpolate to raster then convert to stars.raster to create mosaic of different raster resolution
#  processing time to run code below: 32.76 sec

##
start <- Sys.time()
# 2 km res
res <- 2000
output2 <- point2rast(data = dat, spatobj = sobj, loc = llnames, cellsize = res, nnmax = nmax,
                      as = "SpatRast")

# 6 km res
res <- 6000
output6 <- point2rast(data = dat, spatobj = sobj, loc = llnames, cellsize = res, nnmax = nmax,
                      as = "SpatRast")

# crop out grid cells with polygon masks
r2 <- output2 %>% 
  mask(romseez_poly) %>% 
  mask(inshore_poly) %>%
  stars::st_as_stars()

r6 <- output6 %>%
  mask(romseez_poly) %>%
  mask(offshore_poly) %>%
  #mask(st_as_sf(st_union(st_as_sf(r2))), inverse=T) %>%
  stars::st_as_stars()

r26 <- st_mosaic(r2, r6)
end <- Sys.time()
end-start
##

# convert to sf for simpler data manipulation
#  variables name change; but this is for numeric. TEST!!
r26_sf <- st_as_sf(r26)
r26_sf


# plot
plot(r2[,,,1])
plot(r6[,,,1])
plot(r26[,,,1])


ggplot() +
  geom_stars(data=r26)








## NEXT: write function to process data to our 2km and 6km grid







