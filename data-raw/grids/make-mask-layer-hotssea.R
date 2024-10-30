# create mask layer for hotssea, based on make-mask-layer.R for roms bccm, eez, and bc coast data
library(devtools)
library(sf)
library(ncdf4)
library(terra)

# Run code in data-raw/grids.
load_all()

dir <- here::here()   # Will give pacea/
sf_use_s2(FALSE)  # remove spherical geometry (s2) for sf operations


#####
# START - load data to environment

# For hotssea - I don't have phmc stuff locally, but presumably can just use
# crs(bccm_eez_poly) which is BC Albers and comes from pmhc_crs anyway, looking
# at the original make-mask-layer.R.
# So commenting phmc stuff out as don't think is needed, and can then delete. TODO

# GIS hub Pacific Marine Habitat Classes
#  obtained from: https://www.gis-hub.ca/dataset/marine-habitat-classes
#  requires access to gis-hub
#pmhc_dir <- "./data-raw/pacific_marine_habitat_classes/Pacific_Marine_Habitat_Classes.gdb"
#(pmhc_layers <- st_layers(pmhc_dir))

# use benthic habitat (bh) layer
#pmhc <- st_read(pmhc_dir, layer = pmhc_layers$name[3])
#str(pmhc)
#unique(pmhc$Habitat)

# get crs from pmhc layer to use
#pmhc_crs <-st_crs(pmhc)$wkt

# pacea - rnaturalearth bc coast
#data(bc_coast)

# pacea - PBSdata eez
#data(bc_eez)

# open ncdf and load sst data
nc_dat <- nc_open(paste0(dir, "/data-raw/hotssea/hotssea_1980to2018_monthly_0to4m_tempsalin_avg.nc"))
nc_sstarray <- ncvar_get(nc_dat, "votemper")

# load lon-lat and mask layers from netcdf
nc_lon <- as.vector(ncvar_get(nc_dat, "nav_lon"))   # (French since NEMO)
nc_lat <- as.vector(ncvar_get(nc_dat, "nav_lat"))
# nc_mask <- as.vector(ncvar_get(nc_dat, "mask_rho"))

sst_dat1 <- data.frame(x = nc_lon,
                       y = nc_lat,
                       sst = as.vector(nc_sstarray[,,1]))
sst_sf1 <- sf::st_as_sf(sst_dat1,
                        coords = c("x", "y"),
                        crs = "EPSG:4326")
#####
# Creating masking and extent sf polygon layers
##  using ROMs grid, eez, bc coast, and
##  1. 10km buffer region around eez
##  2. inshore region for high resolution
##  3. 10km buffer region and ROMs data extent

# transform to different CRS to perform sf calculation operations
# tbc <- st_transform(bc_coast, crs = pmhc_crs)
# teez <- st_transform(bc_eez, crs = pmhc_crs)
tsst_sf1 <- st_transform(sst_sf1,
                         crs = terra::crs(bccm_eez_poly))   # NAD83 BC Albers,  pmhc_crs)

# create buffer around eez (in metres)
# tbuff <- st_buffer(teez, dist = 10000)

# create polygon around hotssea data layer
tsst_poly1 <- tsst_sf1 %>%
  summarise(geometry = st_union(geometry)) %>%
  st_convex_hull()             # the rectangle
# I think that's all we need.

# NOTE does not actually encompass all the squares completely. See
# test-hotssea-bccm-full-overlap.R. Looks like it coveres the centers. Maybe
# don't do the convex hull, and stick with outline of all the boxes. See where
# else I've used it hotssea_poly.

# create a combined eez buffer and ROMs mask layer
# Don't think we need TODO check
# bccm_eez_poly <- tbuff %>% st_intersection(tsst_poly1)
hotssea_poly <- tsst_poly1


# bc coast bounding box
# (bc_bbox <- st_bbox(tbc))

# bc eez bounding box
# (eez_bbox <- st_bbox(teez))

# buffer bounding box
# (buff_bbox <- st_bbox(tbuff))

# isolating deep habitat types inshore and offshore gridded areas and getting coordinates
# thabs <- c("Deep", "Seamount", "Hill", "Knoll", "Ridge", "Canyon")
# thabs <- c("Undefined Deep")
# tpmhc <- pmhc[grep(paste(thabs, collapse = "|"), pmhc$Habitat),]
# tpmhc_points <- (tpmhc %>% st_coordinates())[,c(1:2)]

# bounding box of deep habitat
# (tpmhc_bbox <- st_bbox(tpmhc))

# extract points from polygon
##  most northern point of deep habitat sf multipolygon
#pt1 <- tpmhc_points %>%
#  as.data.frame() %>%
#  dplyr::filter(Y == tpmhc_bbox$ymax) %>%
#  dplyr::filter(X == min(X)) %>%
#  mutate(X = X - 50000,
#         Y = Y + 5000)  # move point 50km west and 5k north

##  most eastern point of deep habitat sf multipolygon
#pt2 <- tpmhc_points %>%
#  as.data.frame() %>%
#  dplyr::filter(X == tpmhc_bbox$xmax) %>%
#  dplyr::filter(Y == min(Y)) %>%
#  mutate(X = X,
#         Y = Y - 30000)  # mover point 30km south

# points surrouding eez to cover inshore areas, and use to intersect with tbuff area
#pt3 <- data.frame(X = c(pt1$X, eez_bbox$xmax * 1.1,
#                        eez_bbox$xmax * 1.1, pt2$X),
#                  Y = c(eez_bbox$ymax * 1.1, eez_bbox$ymax * 1.1,
#                        eez_bbox$ymin * 0.9, eez_bbox$ymin * 0.9))

# create mask polygon with points and crop to buffer eez area
# inshore_poly <- pt1 %>%
#  rbind(pt3, pt2, pt1) %>%
#  st_as_sf(coords = c("X", "Y"), crs = st_crs(teez)) %>%
#  summarise(geometry = st_combine(geometry)) %>%
#  st_cast("POLYGON") %>%
#  st_intersection(bccm_eez_poly)

#offshore_poly <- bccm_eez_poly %>%
#  st_difference(inshore_poly)

# save polygons as sf object to package
#usethis::use_data(inshore_poly, overwrite = TRUE)
#usethis::use_data(offshore_poly, overwrite = TRUE)
usethis::use_data(hotssea_poly, overwrite = TRUE)

#####
