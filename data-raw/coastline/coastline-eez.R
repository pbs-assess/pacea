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
st_bbox(canus)
box <- c(xmin = -142, ymin = 46, xmax = -120, ymax = 56)
bc_coast <- st_crop(canus, box)


# BC EEZ from PBSdata
data(eez.bc)

# convert to sf
bc_eez <- maptools::PolySet2SpatialPolygons(eez.bc)
bc_eez <- sf::st_as_sf(bc_eez)

# test plot
ggplot() +
  geom_sf(data = bceez) + 
  geom_sf(data = bccoast) 

# add sf object data to pacakge data folder
usethis::use_data(bc_coast, overwrite = T)
usethis::use_data(bc_eez, overwrite = T)

