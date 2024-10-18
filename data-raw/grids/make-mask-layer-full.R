# create mask layers for full domain that encompasses bccm and hotssea
#  output. Still want 2x2 inshore and 6x6 offshore polygons.
# Adapting from  make-mask-layer.R, but don't need everything in that to be redone.
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
tsst_poly1 <- tsst_sf1 %>% summarise(geometry = st_union(geometry)) %>% st_convex_hull()

save(tsst_poly1, file = "tsst_poly1.rda")

# Try that for hotssea:
# create polygon around hotsseaROMs data layer
hotssea_poly1 <- surf_dat %>%
  summarise(geometry = st_union(geometry)) %>%
  st_convex_hull()

ggplot() +
  geom_sf(data = surf_dat) +
  geom_sf(data = hotssea_poly1, col = NA, fill = "pink") +
  geom_sf(data = surf_dat)
plot(surf_dat)



# What we currently have in pacea:
ggplot() +
  geom_sf(data = inshore_poly, col = NA, fill = "green") +
  geom_sf(data = offshore_poly, col = NA, fill = "blue") +
  geom_sf(data = bc_coast)       # Generally best to plot this last

# crs are different, so the order matters here. Full domain of BCCM:
ggplot() +
  geom_sf(data = tsst_poly1, col = NA, fill = "red") +
  geom_sf(data = bc_coast)         # Generally best to plot this last

# plot coast first to show outside-of-full-domain more clearly.
ggplot() +
  geom_sf(data = bc_coast) +
  geom_sf(data = tsst_poly1, col = NA, fill = "red")

# Some inlets there might be in hotssea domain, so we should check that:
ggplot() +
  geom_sf(data = bc_coast) +
  geom_sf(data = tsst_poly1, col = NA, fill = "red") +
  geom_sf(data = surf_hotssea_cave, col = NA, fill = "green")
# Yes, a few inlests from hotssea are not within the bccm domain. Switch the
# order also to see differently.

# create a combined eez buffer and ROMs mask layer
# bccm_eez_poly <- tbuff %>% st_intersection(tsst_poly1)

# create a combined bbcm and hotssea buffer
bccm_hotssea <- st_union(tsst_poly1,
                         surf_hotssea_cave)
bccm_hotssea_union_union <- st_union(st_union(tsst_poly1,
                                              surf_hotssea_cave))


ggplot() +
  geom_sf(data = bc_coast) +
  geom_sf(data = tsst_poly1, col = NA, fill = "red") +
  geom_sf(data = surf_hotssea_cave, col = NA, fill = "green") +
  geom_sf(data = bccm_hotssea, col = "black", fill = "orange")

plot(bccm_hotssea)

bccm_hotssea_2 <- st_union(surf_hotssea_cave,
                           st_buffer(tsst_poly1, dist = 0))

ggplot() +
  geom_sf(data = bc_coast) +
  geom_sf(data = tsst_poly1, col = NA, fill = "red") +
  geom_sf(data = surf_hotssea_cave, col = NA, fill = "green") +
  geom_sf(data = bccm_hotssea_2, col = "black", fill = "orange")

#interwb says try:
single_sf <- dplyr::bind_rows(tsst_poly1, surf_hotssea_cave)
dissolve_sf <- st_union(single_sf) %>% st_union()

bccm_hotssea_3 <- st_combine(surf_hotssea_cave,
                             st_buffer(tsst_poly1, dist = 0))
# Nope

bccm_hotssea_4 <- rbind(surf_hotssea_cave,
                        tsst_poly1)

# Now trying:
plot(surf_hotssea_buff)
names(surf_hotssea_buff)   # colname is x though

bccm_hotssea_5 <- st_union(tsst_poly1,
                           surf_hotssea_buff)

ggplot() +
  geom_sf(data = bc_coast) +
  geom_sf(data = tsst_poly1, col = NA, fill = "red") +
  geom_sf(data = surf_hotssea_buff, col = NA, fill = "green") +
  geom_sf(data = bccm_hotssea_5, col = "black", fill = "orange")


# Now trying surf_dat, no luck as it contains points.




HERE


# bc coast bounding box
(bc_bbox <- st_bbox(tbc))

# bc eez bounding box
(eez_bbox <- st_bbox(teez))

# buffer bounding box
(buff_bbox <- st_bbox(tbuff))

# isolating deep habitat types inshore and offshore gridded areas and getting coordinates
# thabs <- c("Deep", "Seamount", "Hill", "Knoll", "Ridge", "Canyon")
thabs <- c("Undefined Deep")
tpmhc <- pmhc[grep(paste(thabs, collapse = "|"), pmhc$Habitat),]
tpmhc_points <- (tpmhc %>% st_coordinates())[,c(1:2)]

# bounding box of deep habitat
(tpmhc_bbox <- st_bbox(tpmhc))

# extract points from polygon
##  most northern point of deep habitat sf multipolygon
pt1 <- tpmhc_points %>%
  as.data.frame() %>%
  dplyr::filter(Y == tpmhc_bbox$ymax) %>%
  dplyr::filter(X == min(X)) %>%
  mutate(X = X - 50000,
         Y = Y + 5000)  # move point 50km west and 5k north

##  most eastern point of deep habitat sf multipolygon
pt2 <- tpmhc_points %>%
  as.data.frame() %>%
  dplyr::filter(X == tpmhc_bbox$xmax) %>%
  dplyr::filter(Y == min(Y)) %>%
  mutate(X = X,
         Y = Y - 30000)  # mover point 30km south

# points surrouding eez to cover inshore areas, and use to intersect with tbuff area
pt3 <- data.frame(X = c(pt1$X, eez_bbox$xmax * 1.1,
                        eez_bbox$xmax * 1.1, pt2$X),
                  Y = c(eez_bbox$ymax * 1.1, eez_bbox$ymax * 1.1,
                        eez_bbox$ymin * 0.9, eez_bbox$ymin * 0.9))

# create mask polygon with points and crop to buffer eez area
inshore_poly <- pt1 %>%
  rbind(pt3, pt2, pt1) %>%
  st_as_sf(coords = c("X", "Y"), crs = st_crs(teez)) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") %>%
  st_intersection(bccm_eez_poly)

offshore_poly <- bccm_eez_poly %>%
  st_difference(inshore_poly)

# save polygons as sf object to package
#usethis::use_data(inshore_poly, overwrite = TRUE)
#usethis::use_data(offshore_poly, overwrite = TRUE)
#usethis::use_data(bccm_eez_poly, overwrite = TRUE)
#####
