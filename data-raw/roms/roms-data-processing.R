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

dat <- tsst_sf[, c(1:10)]
dat <- tsst_sf
sobj <- romseez_poly
llnames <- c("x", "y")
res <- 6000
nmax <- 4

# END parameters
#####


#####
# MAIN PROGRAM

# interpolate sst data timing test - 48 months at 6km resolution
system.time(output <- point2rast(data = dat, spatobj = sobj, loc = llnames, cellsize = res, nnmax = nmax,  # 54.03 sec
                                 as = "SpatRast"))
system.time(output <- point2rast(data = dat, spatobj = sobj, loc = llnames, cellsize = res, nnmax = nmax,  # 53.14 sec
                                 as = "SpatVect"))


#####
# TEST 1 - interpolate to spatvector then convert to sf before masking
#  outputs sf points data
#  processing time to run code - 48 months of roms data: 
#  6.772 mins

##
start <- Sys.time()
# 2 km res
output2 <- point2rast(data = dat, spatobj = sobj, loc = llnames, cellsize = 2000, nnmax = nmax, as = "SpatVect")

# 6 km res
output6 <- point2rast(data = dat, spatobj = sobj, loc = llnames, cellsize = 6000, nnmax = nmax, as = "SpatVect")

# crop out grid cells with polygon masks
t1_sf2 <- st_as_sf(output2) %>% 
  st_filter(romseez_poly) %>% 
  st_filter(inshore_poly) %>% 
  st_as_sf()

# two options to filter out data
# 1
# ~5 sec
# t1_sf6 <- st_as_sf(output6) %>% 
#   st_filter(romseez_poly) %>%
#   st_filter(offshore_poly)
# t1_sf6 <- t1_sf6[!st_intersects(t1_sf6, st_union(t1_sf2), sparse=F),] %>%   
#   st_as_sf()

# 2
# 0.3 sec - much faster
t1_sf6 <- st_as_sf(output6) %>% 
  st_filter(romseez_poly) %>%
  st_filter(offshore_poly)
system.time(
t1_sf6 <- t1_sf6[!st_intersects(st_convex_hull(st_union(t1_sf2)), t1_sf6, sparse=F, prepared=T),] %>%   
  st_as_sf() 
)

# combine grids
t1_sf26 <- t1_sf2 %>% rbind(t1_sf6)

# index points that dont intersect with bc coast shapefile
t1_sf26 <- t1_sf26[!st_intersects(st_union(tbc), t1_sf26, sparse=F, prepared=T),]

end <- Sys.time()
t1time <- end-start
##

# data size of sf object
roms_sf_test1 <- t1_sf26
roms_sf_test1_nogeom <- t1_sf26 %>% st_drop_geometry()
usethis::use_data(roms_sf_test1)  # 7.687 mb
usethis::use_data(roms_sf_test1_nogeom)  # 7.671 mb

# END TEST 1
#####



#####
# TEST 2 - interpolate to raster then convert to stars.raster then sf polygons
#  outputs sf polygon data (represents raster cells)
#  processing time to run code - 48 months of roms data: 
#  6.669 mins

##
start <- Sys.time()
# 2 km res
output2 <- point2rast(data = dat, spatobj = sobj, loc = llnames, cellsize = 2000, nnmax = nmax,
                      as = "SpatRast")

# 6 km res
output6 <- point2rast(data = dat, spatobj = sobj, loc = llnames, cellsize = 6000, nnmax = nmax,
                      as = "SpatRast")

# crop out grid cells with polygon masks

# compare times with masking rasters vs filtering sf points
# 1 - 1.54 sec
system.time(
  t2_sf2 <- output2 %>% 
    mask(romseez_poly) %>% 
    mask(inshore_poly) %>%
    stars::st_as_stars() %>%  ## check here fro converting to points (not raster)
    st_as_sf()
)

# 2 - 4.28 sec
# system.time(
#   t2_sf2 <- output2 %>% 
#     stars::st_as_stars() %>%  ## check here fro converting to points (not raster)
#     st_as_sf() %>% 
#     st_filter(romseez_poly) %>% 
#     st_filter(inshore_poly) 
# )

t2_sf6 <- output6 %>%
  mask(romseez_poly) %>%
  mask(offshore_poly) %>%
  stars::st_as_stars() %>%
  st_as_sf()

st_difference(st_union(t2_sf2))  # can't do convex hull as it cuts off some areas along border of geometry cells

# mask 2k grid with 6k grid, then combine grids
t2_sf2 <- t2_sf2[!st_intersects(st_union(t2_sf6), t2_sf2, sparse=F, prepared=T),] %>%
  rbind(t2_sf2[st_intersects(st_union(t2_sf6), t2_sf2, sparse=F, prepared=T),])
t2_sf26 <- t2_sf2 %>% rbind(t2_sf6)

# index points that dont intersect with bc coast shapefile
#  disjoint - do not share space
dis2 <- t2_sf26[st_disjoint(st_union(tbc), t2_sf26, sparse=F, prepared=T),]

#  convert bc coast to sf linestring and finding coastline intserections separately - increased processing speed
#  using st_intersects is much faster than other predicate functionss
tbc.line <- st_cast(tbc, "MULTILINESTRING")
sub.t2 <- t2_sf26[st_intersects(st_union(tbc), t2_sf26, sparse=F, prepared=T),]
inter.line <- sub.t2[st_intersects(tbc.line, sub.t2, sparse=F, prepared=T),]
t2_sf26 <- rbind(dis2, inter.line)

end <- Sys.time()
t2time <- end-start
##

plot(t2_sf26[,1])


# data size of sf object
roms_sf_test2 <- t2_sf26
roms_sf_test2_nogeom <- t2_sf26 %>% st_drop_geometry()
usethis::use_data(roms_sf_test2)  # 8.362 mb
usethis::use_data(roms_sf_test2_nogeom)  # 8.252 mb

# END TEST 2
#####


#####
# TEST 3 - interpolate to raster then convert to stars.raster then sf points
#  outputs sf points data 
#  processing time to run code - 48 months of roms data: 
#  6.723 mins

##
start <- Sys.time()
# 2 km res
output2 <- point2rast(data = dat, spatobj = sobj, loc = llnames, cellsize = 2000, nnmax = nmax,
                      as = "SpatRast")

# 6 km res
output6 <- point2rast(data = dat, spatobj = sobj, loc = llnames, cellsize = 6000, nnmax = nmax,
                      as = "SpatRast")

# crop out grid cells with polygon masks
t3_sf2 <- output2 %>% 
  mask(romseez_poly) %>% 
  mask(inshore_poly) %>%
  stars::st_as_stars() %>%  
  st_as_sf(as_points=T)

t3_sf6 <- output6 %>%
  mask(romseez_poly) %>%
  mask(offshore_poly) %>%
  stars::st_as_stars() %>%
  st_as_sf(as_points=T)
t3_sf6 <- t3_sf6[!st_intersects(st_convex_hull(st_union(t3_sf2)), t3_sf6, sparse=F, prepared=T),] %>%   
  st_as_sf() 

# combine grids
t3_sf26 <- rbind(t3_sf2, t3_sf6)

# index points that dont intersect with bc coast shapefile
t3_sf26 <- t3_sf26[!st_intersects(t3_sf26, st_union(tbc), sparse=F),]

end <- Sys.time()
t3time <- end-start
##

plot(t3_sf26[,2])

# data size of sf object
roms_sf_test3 <- t3_sf26
roms_sf_test3_nogeom <- t3_sf26 %>% st_drop_geometry()
usethis::use_data(roms_sf_test3)  # 7.821 mb
usethis::use_data(roms_sf_test3_nogeom)  # 7.754 mb

# END TEST 3
#####

dim(t1_sf26)  # 48,247 cells
dim(t2_sf26)  # 52,861 cells
dim(t3_sf26)  # 48,874 cells

plot(t1_sf26[,1], pch=15)
plot(t2_sf26[,1])
plot(t3_sf26[,1], pch=15)

ggplot(t1_sf26) +
  geom_sf(aes(col=`1`), shape=15) +
  geom_sf(data=tbc) + 
  scale_colour_gradient2(low = "blue", mid = "orange", high = "red", midpoint = median(as.vector(t1_sf26$`1`)))
ggplot(t2_sf26) +
  geom_sf(aes(fill=`1`), col=NA) +
  geom_sf(data=tbc) + 
  scale_fill_gradient2(low = "blue", mid = "orange", high = "red", midpoint = median(as.vector(t1_sf26$`1`)))
ggplot(t3_sf26) +
  geom_sf(aes(col=`1`), shape=15) +
  geom_sf(data=tbc) + 
  scale_colour_gradient2(low = "blue", mid = "orange", high = "red", midpoint = median(as.vector(t1_sf26$`1`)))






