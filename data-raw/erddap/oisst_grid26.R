
## OISST NOAA erddap data interpolate to pacea grid 26
#

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

# set working directory from current path script
setwd(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/../../"))

# load pacea
load_all()

# load roms_cave and roms_buff

load("data/ignore/roms_buff.rda")
load("data/ignore/roms_cave.rda")



# --- Step 1: transform oisst to match crs of grid ---


# 
dat_wide <- oisst_month %>%
  tidyr::pivot_wider(names_from = c("year", "month"),
                     values_from = "sst")

# crs of grid 26
grid_crs <- st_crs(grid26)


# oisst data
dat_trans <- st_transform(dat_wide, crs = grid_crs)




# --- Step 2: interpolate data ---
# 2 km res
sub_2 <- point2rast(data = dat_trans,
                    spatobj = inshore_poly,
                    loc = c("x", "y"),
                    cellsize = 2000,
                    as = "SpatRast")
# 6 km res
sub_6 <- point2rast(data = dat_trans,
                    spatobj = offshore_poly,
                    loc = c("x", "y"),
                    cellsize = 6000,
                    as = "SpatRast")


# --- Step 3: convert to sf ---

sub_sf2 <- sub_2 %>%
  terra::mask(bccm_eez_poly) %>%
  terra::mask(inshore_poly) %>%
  stars::st_as_stars() %>%  ## check here for converting to points (not raster)
  st_as_sf()
sub_sf6 <- sub_6 %>%
  terra::mask(bccm_eez_poly) %>%
  terra::mask(offshore_poly) %>%
  stars::st_as_stars() %>%
  st_as_sf()

# crop out grid cells with polygon masks
sub_sf2 <- sub_2 %>%
  terra::mask(bccm_eez_poly) %>%
  terra::mask(inshore_poly) %>%
  stars::st_as_stars() %>%  ## check here for converting to points (not raster)
  st_as_sf()
sub_sf6 <- sub_6 %>%
  terra::mask(bccm_eez_poly) %>%
  terra::mask(offshore_poly) %>%
  stars::st_as_stars() %>%
  st_as_sf()

# mask 2k grid with 6k grid, then combine grids
sub_sf26a <- sub_sf2[!st_intersects(st_union(sub_sf6),
                                    sub_sf2,
                                    sparse=FALSE,
                                    prepared=TRUE),] %>%
  rbind(sub_sf2[st_intersects(st_union(sub_sf6),
                              sub_sf2,
                              sparse=FALSE,
                              prepared=TRUE),]) %>%
  rbind(sub_sf6)


# Ideally want to make it bigger? For now Philina just wants same as BCCM.
# Need to use same outline as for bccm values, i.e. roms_cave:

## snc_lat <- as.vector(ncvar_get(snc_dat, "lat_rho"))
## svar <- as.vector(ncvar_get(snc_dat, "temp", count = c(-1, -1, 1)))

## sdat <- data.frame(x = snc_lon, y = snc_lat, value = svar) %>%
##   st_as_sf(coords = c("x", "y"), crs = "EPSG:4326") %>%
##   st_transform(crs = "EPSG:3005")

## sroms_cave <- sdat %>%
##   na.omit() %>%
##   concaveman::concaveman()
## sroms_buff <- sdat %>%
##   na.omit() %>%
##   st_geometry() %>%
##   st_buffer(dist = 2000) %>%
##   st_union() %>%
##   st_as_sf()



# Think can ignore roms_buff and roms_cave, which were data-specific
# (e.g. bottom temperature), and we're already using the surface values
sub_sf26b <- sub_sf26a[roms_cave,]

# 2. use roms_buff to get haida gwaii outline and shore
sub_sf26b <- sub_sf26b[roms_buff,]

# TODO may need these if domains don't match
# 3. use default surface roms_cave
#    sub_sf26b <- sub_sf26b[sroms_cave,]

# 4. use default surface roms_buff
#    sub_sf26 <- sub_sf26b[sroms_buff,]

expect_equal(nrow(sub_sf26b), 41288)   # Passes!
# data should have 41,288 grid cells

# assign column names as year_month. TODO seems to already be there, might
# be formatted slightly differently?
#    names(sub_sf26)[1:(ncol(sub_sf26) - 1)] <- cnames

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
oisst_month_grid26 <- sub_sf26b %>%
  st_drop_geometry() %>%
  round(digits = 6) %>%
  st_as_sf(geometry = st_geometry(sub_sf26b))

# assign pacea class
class(oisst_month_grid26) <- c("pacea_st", "sf", "tbl_df", "tbl", "data.frame")

# assign units attribute
attr(oisst_month_grid26, "units") <- attr(oisst_month, "units")

save(oisst_month_grid26, file = "oisst_month_grid26.rds", compress = "xz")




















