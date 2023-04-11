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
bccoast <- st_crop(canus, box)


# BC EEZ from PBSdata
data(eez.bc)

# convert to sf
bceez <- maptools::PolySet2SpatialPolygons(eez.bc)
bceez <- sf::st_as_sf(bceez)

# test plot
ggplot() +
  geom_sf(data = bceez) + 
  geom_sf(data = bccoast) 

# add sf object data to pacakge data folder
usethis::use_data(bccoast)
usethis::use_data(bceez)

