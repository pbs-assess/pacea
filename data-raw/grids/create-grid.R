# create empty sf grid

library(terra)
library(sf)
library(dplyr)
library(ncdf4)

load_all()

sf_use_s2(FALSE)

#####
# START - load data to environment

# transform bc_coast
tbc <- st_transform(bc_coast, crs=crs(romseez_poly))

# convert to multilinestring
tbc.line <- st_cast(tbc, "MULTILINESTRING")

#####
# create polygons for cropping to roms data
nc_dat <- nc_open(paste0("data-raw/roms/bcc42_era5glo12r4_mon1993to2019_botTSO.nc"))

# load lon-lat and mask layers from netcdf
nc_lon <- as.vector(ncvar_get(nc_dat, "lon_rho"))
nc_lat <- as.vector(ncvar_get(nc_dat, "lat_rho"))

nc_var <- ncvar_get(nc_dat, "temp")
nc_varmat <- apply(nc_var, 3, c)

# put sst into dataframe and sf object
dat <- data.frame(x = nc_lon, y = nc_lat) %>% cbind(nc_varmat) %>%
  st_as_sf(coords = c("x", "y"), crs = "EPSG:4326") %>%
  st_transform(crs = "EPSG: 3005")

# create polygon for cropping ROMS data
roms_cave <- dat %>% 
  na.omit() %>%
  concaveman::concaveman()
roms_buff <- dat %>% 
  na.omit() %>%
  st_geometry() %>% 
  st_buffer(dist = 5000) %>%
  st_union()

rm(nc_dat, nc_lon, nc_lat, nc_var, nc_varmat, dat)
#####

#####
# make sf grid 
grid2 <- st_make_grid(inshore_poly, cellsize = 2000) %>%
  st_as_sf()
grid2 <- grid2[inshore_poly,]

grid6 <- st_make_grid(offshore_poly, cellsize = 6000) %>%
  st_as_sf()
grid6 <- grid6[offshore_poly,]

# mask 2k grid with 6k grid, then combine grids
tgrid2 <- grid2[!st_intersects(st_union(grid6), grid2, sparse=F, prepared=T),] %>%
  rbind(grid2[st_intersects(st_union(grid6), grid2, sparse=F, prepared=T),])
tgrid26 <- tgrid2 %>% rbind(grid6)


##### BC MASK OPTION 1 - Using bc shapefile
# # index points that dont intersect with bc coast shapefile
# #  disjoint - do not share space
# dis2 <- tgrid26[st_disjoint(st_union(tbc), tgrid26, sparse=F, prepared=T),]
# 
# #  convert bc coast to sf linestring and finding coastline intserections separately - increased processing speed
# #  using st_intersects is much faster than other predicate functionss
# sub.t2 <- tgrid26[st_intersects(st_union(tbc), tgrid26, sparse=F, prepared=T),]
# inter.line <- sub.t2[st_intersects(tbc.line, sub.t2, sparse=F, prepared=T),]
# grid26 <- rbind(dis2, inter.line)


##### BC MASK OPTION 2 - Using roms outline 
# use roms_cave
tgrid26 <- tgrid26[roms_cave,]

# use roms_buff to get haida gwaii outline
grid26 <- tgrid26[roms_buff,]

usethis::use_data(grid26, overwrite = F)
