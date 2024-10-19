# Create the empty sf grid for hotssea, based on create-grid.R for ROMS
# outputs. Making this one 1500m to match the hotssea output (though it won't
# exactly match the curvilinear rotated grid).

TODO do we need this? Don't think so now.
library(terra)
library(sf)
library(dplyr)
library(ncdf4)

load_all()
dir <- here::here()   # Will give pacea/

sf_use_s2(FALSE)

#####
# START - load data to environment

# transform bc_coast
#tbc <- st_transform(bc_coast, crs=crs(bccm_eez_poly))

# convert to multilinestring
#tbc.line <- st_cast(tbc, "MULTILINESTRING")

#####
# create polygons for cropping to roms data
nc_dat <- nc_open(paste0(dir, "/data-raw/hotssea/hotssea_1980to2018_monthly_0to4m_tempsalin_avg.nc"))

# data-raw/roms/bcc42_era5glo12r4_mon1993to2019_botTSO.nc")) # Travis had a
                                        # different .nc file to mask-layer, but
                                        # grid is presumably the same (though
                                        # this is bottom).

# load lon-lat (no mask layers) from netcdf
nc_lon <- as.vector(ncvar_get(nc_dat, "nav_lon"))   # (French since NEMO)
nc_lat <- as.vector(ncvar_get(nc_dat, "nav_lat"))

nc_var <- ncvar_get(nc_dat, "votemper")
nc_varmat <- apply(nc_var, 3, c)

# put sst into dataframe and sf object
dat <- data.frame(x = nc_lon, y = nc_lat) %>%
  cbind(nc_varmat) %>%
  st_as_sf(coords = c("x", "y"), crs = "EPSG:4326") %>%
  st_transform(crs = "EPSG: 3005")

# create polygon for cropping hotssea data
hotssea_cave <- dat %>%
  na.omit() %>%
  concaveman::concaveman()
# Skipping buffer:
hotssea_buff <- dat %>%
  na.omit() %>%
  st_geometry() %>%
  st_buffer(dist = 5000) %>%    # TODO don't think we want this, as will end up
                                # on land
  st_union()

rm(nc_dat, nc_lon, nc_lat, nc_var, nc_varmat, dat)
#####

#####
# make sf grid
#grid2 <- st_make_grid(inshore_poly, cellsize = 2000) %>%
#  st_as_sf()
# grid2 <- grid2[inshore_poly,]  - don't need this for hottsea

grid_hotssea <- st_make_grid(hotssea_cave,
                             cellsize = 1500) %>%
  st_as_sf()

# mask 2k grid with 6k grid, then combine grids
#tgrid2 <- grid2[!st_intersects(st_union(grid6), grid2, sparse=F, prepared=T),] %>%
#  rbind(grid2[st_intersects(st_union(grid6), grid2, sparse=F, prepared=T),])
#tgrid26 <- tgrid2 %>% rbind(grid6)

##### BC MASK OPTION 2 - Using roms outline
# use roms_cave
# tgrid26 <- tgrid26[roms_cave,]  # TODO don't think need for hotssea

# use roms_buff to get haida gwaii outline
# grid26 <- tgrid26[roms_buff,]    # TODO don't think need for hotssea

usethis::use_data(grid_hotssea,
                  overwrite = TRUE)
