# BC coastline map
# Exploring different options for default mapping of BC coastline

library(ggplot2)
library(sf)
sf_use_s2(FALSE) # turn off spherical geometry for setting bounding boxes (eg crop)

#-------------------------------------------------#
# bcmaps 
# Package from BC government using maps from BC Data Catalogue (https://catalogue.data.gov.bc.ca/)
# https://github.com/bcgov/bcmaps
# install.packages("bcmaps")
library(bcmaps)
layers <- bcmaps::available_layers()

# download map data to environment
bc <- bc_bound()
bc_h <- bc_bound_hres()
neighb <- bc_neighbours()

# visualizing different maps
plot(bc)      # only BC shape
plot(neighb)  # extent may be too small to include BC marine waters
ggplot() +
  geom_sf(data = bc)
ggplot() +
  geom_sf(data = neighb)

object.size(bc)      # 196 kb
object.size(bc_h)    # 52.4 mb - quite large
object.size(neighb)  # 421 kb

# cropping extent of bc_neighbours()
st_bbox(neighb)
box <- c(xmin = 100000, ymin = 290000, xmax = 1450000, ymax = 1400000)
neighb_crop <- st_crop(neighb, box)
ggplot() + 
  geom_sf(data = neighb_crop)  # extent may be too small to include BC marine waters

#-------------------------------------------------#
# PBSdata
# PBSdata coastline
#devtools::install_github("pbs-software/pbs-data/PBSdata")
#devtools::install_github("pbs-software/pbs-mapping/PBSmapping")
library(PBSdata)
library(PBSmapping)

?PBSdata # description of available data files
?PBSmapping

#####
# northeast pacific data
data(nepacLL)
str(nepacLL)

# assign new zone and convert to UTM
attr(nepacLL, "zone") <- 10
nepacUTM <- convUL(nepacLL)

# convert to sf
nepacLL_sf <- maptools::PolySet2SpatialPolygons(nepacLL)
nepacLL_sf <- sf::st_as_sf(nepacLL_sf)

# crop to BC marine area and plot
st_bbox(nepacLL_sf)
box <- c(xmin = -140, ymin = 45, xmax = -120, ymax = 57)
nepacLL_sf_crop <- st_crop(nepacLL_sf, box)

ggplot() +
  geom_sf(data = nepacLL_sf_crop)


#####
# northeast pacific high res
data(nepacLLhigh)
str(nepacLLhigh)

# assign new zone and convert to UTM
attr(nepacLLhigh, "zone") <- 10
nepacUTMhigh <- convUL(nepacLLhigh)

# convert to sf
nepacLLhigh_sf <- maptools::PolySet2SpatialPolygons(nepacLLhigh)
nepacLLhigh_sf <- sf::st_as_sf(nepacLLhigh_sf)

ggplot() +
  geom_sf(data = nepacLLhigh_sf)

object.size(nepacLL)         # 1.81 mb
object.size(nepacLL_sf)      # 1.53 mb
object.size(nepacLLhigh)     # 4.63 mb
object.size(nepacLLhigh_sf)  # 3.28 mb

#####
# bctopo - topography of bottom and some land areas, not all of bc extent
data(bctopo)

# plot - sea bottom
ggplot() +
  geom_contour(data = bctopo, aes(x = x, y = y, z = z))

#####
# eez.bc
data(eez.bc)
str(eez.bc)

# convert to sf
eez.bc_sf <- maptools::PolySet2SpatialPolygons(eez.bc)
eez.bc_sf <- sf::st_as_sf(eez.bc_sf)

ggplot() +
  # geom_polygon(data = eez.bc, aes(x = X, y = Y, group = PID),
  #              col = "black", fill = NA, lwd = 0.01) +
  geom_sf(data = eez.bc_sf) + 
  geom_sf(data = nepacLL_sf_crop)


#-------------------------------------------------#
# rnaturalearth - subset data for Canada & US

library(rnaturalearth)

# download Canada and US shapes
canus <- rnaturalearth::ne_countries(scale = "large", 
                                     type = "countries",
                                     country = c("Canada","United States of America"),
                                     #continent = "North America",
                                     returnclass = "sf")

# plot Canada and US
ggplot() +
  geom_sf(data = canus)

# setting scale to zoom into bc marine region
ggplot() +
  geom_sf(data = canus) +
  scale_x_continuous(limits = c(-142,-110)) +
  scale_y_continuous(limits = c(45,60))
ggplot() +
  geom_sf(data = canus) +
  coord_sf(xlim = c(-142,-110), ylim = c(45,60))

# crop to bc marine area and plot
st_bbox(canus)
box <- c(xmin = -142, ymin = 45, xmax = -110, ymax = 60)
canus_crop <- st_crop(canus, box)

ggplot() +
  geom_sf(data = eez.bc_sf) + 
  geom_sf(data = canus_crop) 

