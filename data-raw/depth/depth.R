# Adding depth values to grid26. Original code written by Kelsey Flynn in
#  depth-to-grid.R. Andy tweaking it here for saving as a data object in pacea.
#Date: 2024-05-10
#Author: Kelsey Flynn

#Required packages
# library(pacea)
load_all()
library(sf)
library(terra)
library(tidyterra)
library(exactextractr)
library(tidyverse)
library(here)
library(ggplot2)

#*bathymetry data downloaded from https://download.gebco.net/, using bounding box
#*-146,-117,42,58, grid version GEBCO 2023.

#pacea grid
#2km inshore, 6 km offshore resolution
#polygon format, sf object
grid26

#grab crs from grid
crs_new <- paste0(terra::crs(grid26, describe=TRUE)[,2],
                  ":",
                  terra::crs(grid26, describe=TRUE)[,3])

#read in gebco ncdf as terra spatraster
bathy <- terra::rast(here::here("data-raw/depth/",
                              "gebco_2023_n58.0_s42.0_w-146.0_e-117.0.nc"))


#template raster to transform bathy data to match pacea grid
ext2 <- st_bbox(grid26)

temp <- terra::rast(resolution=c(500,500), #have to resample to lower res than input data
                    xmin=ext2[1]-500, #match extent + buffer of pacea grid
                    ymin=ext2[2]-500,
                    xmax=ext2[3]+500,
                    ymax=ext2[4]+500,
                    crs=crs_new #crs of pacea grid for zonal statistics
                    )


#reproject to new crs, using bilinear for continuous data
bathy_proj <- terra::project(bathy, temp, method="bilinear")
bathy_proj


#do they match?

#unprojected bathymetry data
#grid26 %>%
#  st_transform(., crs=4326) %>% #project on fly so they match for plotting
#  ggplot(.) +
#  geom_spatraster(data=bathy)+
#  geom_sf()

#projected bathymetry data
grid26 %>%
  ggplot(.) +
  geom_spatraster(data=bathy_proj)+
  geom_sf()

#Zonal statistics
grid26$mean_depth <- exactextractr::exact_extract(bathy_proj, grid26, "mean")
grid26$max_depth <- exactextractr::exact_extract(bathy_proj, grid26, "max")
grid26$min_depth <- exactextractr::exact_extract(bathy_proj, grid26, "min")

#lookieloo
grid26 %>%
  ggplot(.) +
  geom_sf(aes(fill=mean_depth),colour=NA)

#Any average depths above 0 meter elevation?
grid26 %>%
  mutate(above0=ifelse(mean_depth>0, "Y", "N")) %>%
  ggplot(.) +
  geom_sf(aes(fill=above0),colour=NA)

#Any minimum depths above 0 meter elevation?
grid26 %>%
  mutate(above0_min=ifelse(min_depth>0, "Y", "N")) %>%
  ggplot(.) +
  geom_sf(aes(fill=above0_min),colour=NA)


plot(grid26["mean_depth"])     # shows the grid also

# This is nicer for the plot (from above)
grid26 %>%
  ggplot(.) +
  geom_sf(aes(fill=mean_depth),
          colour=NA)

# Actual values are:
grid26["mean_depth"]    # As sf object still, mean_depth and geometry
head(grid26$mean_depth) # Vector of the mean depths, so just showing first 6
length(grid26$mean_depth) # Vector of the mean depths, so just showing first 6

grid26_depth <- grid26   # As expect changing existing grid26 will mess up
                              # other code
usethis::use_data(grid26_depth,
                  overwrite = TRUE)
