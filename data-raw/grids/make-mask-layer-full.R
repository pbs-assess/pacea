# create mask layers for full domain that encompasses bccm and hotssea
#  output. Still want 2x2 inshore and 6x6 offshore polygons.
# Adapting from  make-mask-layer.R, but don't need everything in that to be redone.

# TODO remove lots of the commented out stuff once it works

library(devtools)
library(sf)
library(ncdf4)

pacea_dir <- here::here()
load_all()

sf_use_s2(FALSE)  # remove spherical geometry (s2) for sf operations

#####
# START - load data to environment

## --- skip from here
## # GIS hub Pacific Marine Habitat Classes
## #  obtained from: https://www.gis-hub.ca/dataset/marine-habitat-classes
## #  requires access to gis-hub
## pmhc_dir <- "./data-raw/pacific_marine_habitat_classes/Pacific_Marine_Habitat_Classes.gdb"
## (pmhc_layers <- st_layers(pmhc_dir))

## # use benthic habitat (bh) layer
## pmhc <- st_read(pmhc_dir, layer = pmhc_layers$name[3])
## str(pmhc)
## unique(pmhc$Habitat)

## # get crs from pmhc layer to use
## pmhc_crs <-st_crs(pmhc)$wkt


## # pacea - rnaturalearth bc coast
## data(bc_coast)

## # pacea - PBSdata eez
## data(bc_eez)

## ---to here

# open ncdf and load sst data
nc_dat <- nc_open(paste0(pacea_dir, "/data-raw/roms/Roms_bcc42_mon_2008to2011_sst.nc"))
nc_sstarray <- ncvar_get(nc_dat, "sst")

# load lon-lat and mask layers from netcdf, and create an sf object sst_sf1
nc_lon <- as.vector(ncvar_get(nc_dat, "lon_rho"))
nc_lat <- as.vector(ncvar_get(nc_dat, "lat_rho"))
nc_mask <- as.vector(ncvar_get(nc_dat, "mask_rho"))

sst_dat1 <- data.frame(x = nc_lon, y = nc_lat, sst = as.vector(nc_sstarray[,,1]))
# create sf object from non-geospatial data frame:
sst_sf1 <- sf::st_as_sf(sst_dat1,
                        coords = c("x", "y"),
                        crs = "EPSG:4326")

# END load data
#####

#####
# Creating masking and extent sf polygon layers
##  using ROMs grid, eez, bc coast, and
##  1. 10km buffer region around eez
##  2. inshore region for high resolution
##  3. 10km buffer region and ROMs data extent

pmhc_crs = "EPSG: 3005"   # Andy assuming this is correct, do not have access to
                          # gis-hub (see above), and this is what is used in
                          # roms-data-interpolation.R in st_transform commands

# transform to different CRS to perform sf calculation operations
tbc <- st_transform(bc_coast, crs = pmhc_crs)
teez <- st_transform(bc_eez, crs = pmhc_crs)
tsst_sf1 <- st_transform(sst_sf1, crs = pmhc_crs)

# create buffer around eez (in metres)
# tbuff <- st_buffer(teez, dist = 10000)

# create polygon around ROMs data layer
tsst_poly1 <- tsst_sf1 %>%
  summarise(geometry = st_union(geometry)) %>%
  st_convex_hull()

plot(tsst_poly1)
checking <- summarise(tsst_sf1, geometry = st_union(geometry))
plot(checking, add = TRUE, col = "red")

# save(tsst_poly1, file = "tsst_poly1.rda")


# Try that for hotssea, since tsst_sf1 and surf_dat have same structure
# create polygon around hotssea data layer
hotssea_poly1 <- surf_dat %>%
  summarise(geometry = st_union(geometry)) %>%
  st_convex_hull()

# Now try union:   this seems to be working HERE HERE HERE
bccm_hotssea_poly <- st_union(tsst_poly1,
                         hotssea_poly1)

ggplot() +
  geom_sf(data = bccm_hotssea_poly, col = NA, fill = "pink") +
  geom_sf(data = inshore_poly, col = NA, fill = "green") +
  geom_sf(data = offshore_poly, col = NA, fill = "blue") +
  geom_sf(data = bc_coast)       # Generally best to plot this last, as crs is
                                 # different, so order matters (as well as overlaying)

#Next need to figure out what to do about the 2x2 and 6x6 boundary.
#Maybe do a test file of
#Angelica's for the 2x2 everywhere. Realising that Travis already did that sort
#of thing. Calculations below suggest that might be fine at 2x2 level, only 2.2
#times as big as current .rds files.

#Commenting next stuff to get to the polys

# bc coast bounding box
#(bc_bbox <- st_bbox(tbc))

# bc eez bounding box
#(eez_bbox <- st_bbox(teez))

# buffer bounding box
#(buff_bbox <- st_bbox(tbuff))

# isolating deep habitat types inshore and offshore gridded areas and getting coordinates
# thabs <- c("Deep", "Seamount", "Hill", "Knoll", "Ridge", "Canyon")
#thabs <- c("Undefined Deep")
#tpmhc <- pmhc[grep(paste(thabs, collapse = "|"), pmhc$Habitat),]
#tpmhc_points <- (tpmhc %>% st_coordinates())[,c(1:2)]

# bounding box of deep habitat
#(tpmhc_bbox <- st_bbox(tpmhc))

# extract points from polygon
##  most northern point of deep habitat sf multipolygon
## pt1 <- tpmhc_points %>%
##   as.data.frame() %>%
##   dplyr::filter(Y == tpmhc_bbox$ymax) %>%
##   dplyr::filter(X == min(X)) %>%
##   mutate(X = X - 50000,
##          Y = Y + 5000)  # move point 50km west and 5k north

##  most eastern point of deep habitat sf multipolygon
## pt2 <- tpmhc_points %>%
##   as.data.frame() %>%
##   dplyr::filter(X == tpmhc_bbox$xmax) %>%
##   dplyr::filter(Y == min(Y)) %>%
##   mutate(X = X,
##          Y = Y - 30000)  # mover point 30km south

# points surrouding eez to cover inshore areas, and use to intersect with tbuff area
## pt3 <- data.frame(X = c(pt1$X, eez_bbox$xmax * 1.1,
##                         eez_bbox$xmax * 1.1, pt2$X),
##                   Y = c(eez_bbox$ymax * 1.1, eez_bbox$ymax * 1.1,
##                         eez_bbox$ymin * 0.9, eez_bbox$ymin * 0.9))

# create mask polygon with points and crop to buffer eez area
## inshore_poly <- pt1 %>%
##   rbind(pt3, pt2, pt1) %>%
##   st_as_sf(coords = c("X", "Y"),
## crs = st_crs(teez)) %>%
##   summarise(geometry = st_combine(geometry)) %>%
##   st_cast("POLYGON") %>%
##   st_intersection(bccm_eez_poly)

## offshore_poly <- bccm_eez_poly %>%
##   st_difference(inshore_poly)

# Structure is the same:
inshore_poly
offshore_poly
bccm_hotssea_poly

inshore_area <- st_area(inshore_poly) * 1e-06
offshore_area <- st_area(offshore_poly) * 1e-06
bh_area <- st_area(bccm_hotssea_poly) * 1e-06

inshore_area
offshore_area
bh_area
bh_area / (inshore_area + offshore_area)  # only 1.5x larger. Ignores land effects

# Roughly, can work out how much bigger .rds file should be for bh than for inshore + offshore. How many 2x2 km squares

curr_num_cells <- inshore_area / 4 + offshore_area / 9
bh_est_num_cells <- bh_area / 4

bh_est_size_ratio <- bh_est_num_cells/curr_num_cells
bh_est_size_ratio

# Only 2.17 times larger. So maybe don't worry about all the 2x2 and 6x6 and
# just do 2x2 throughout.

# save polygons as sf object to package
usethis::use_data(bccm_hotssea_poly, overwrite = TRUE)
#usethis::use_data(inshore_poly, overwrite = TRUE)
#usethis::use_data(offshore_poly, overwrite = TRUE)
#usethis::use_data(bccm_eez_poly, overwrite = TRUE)
#####
