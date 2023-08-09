# BC coastline sf data object from 'rnaturalearth'
# Canada Pacific EEZ shapefile from 'PBSdata'
#  NOTE: PBSdata shapefiles are of class PolySet - requires 'maptools' to convert 
#        from PolySet to sf object, which is being retired in 2023

library(rnaturalearth)
library(PBSdata)
library(maptools)
library(sf)
library(ggplot2)
sf_use_s2(FALSE) # turn off spherical geometry for setting bounding boxes (eg crop)

# download Canada and US shapes
canus <- rnaturalearth::ne_countries(scale = "large", 
                                     type = "countries",
                                     country = c("Canada","United States of America"),
                                     #continent = "North America",
                                     returnclass = "sf")

# crop to bc marine area and plot
st_bbox(canus) # bounding box to create crop extent
box <- c(xmin = -142, ymin = 46, xmax = -120, ymax = 56)
tbc_coast <- st_crop(canus, box)

# keep only remove all extra data columns
tbc_coast <- tbc_coast[,c("admin","geometry")]

#####
# run code below to remove data columns / attributes (i.e. USA and Canada)

# combine data rows of bc_coast into one multipolygon
tbc_coast <- st_combine(tbc_coast) # converts to sfc_MULTIPOLYGON object
tbc_coast <- st_as_sf(tbc_coast)
st_geometry(tbc_coast) <- "geometry"
#####

bc_coast <- tbc_coast

# BC EEZ from PBSdata
data(eez.bc)

# convert to sf
bc_eez <- maptools::PolySet2SpatialPolygons(eez.bc)
bc_eez <- sf::st_as_sf(bc_eez)

# reproject to wgs84
bc_coast <- st_transform(bc_coast, crs = "EPSG:4326")
bc_eez <- st_transform(bc_eez, crs = "EPSG:4326")

# OR reproject to bcalbers
# bc_coast <- st_transform(bc_coast, crs = "EPSG:3005")
# bc_eez <- st_transform(bc_eez, crs = "EPSG:3005")

# test plot
ggplot() +
  geom_sf(data = bc_eez) + 
  geom_sf(data = bc_coast) 

# add sf object data to pacakge data folder
usethis::use_data(bc_coast, overwrite = T)
usethis::use_data(bc_eez, overwrite = T)

