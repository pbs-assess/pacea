# Adding depth values to grid26 into new object grid26_depth. Original code written by Kelsey Flynn in
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

# Now to save depths into new grid26_depth object
grid26_depth <- grid26   # Renaming as changing existing grid26 will likely mess
                         # up existing code

#Zonal statistics
grid26_depth$mean_depth <- exactextractr::exact_extract(bathy_proj, grid26_depth, "mean")
grid26_depth$max_depth <- exactextractr::exact_extract(bathy_proj, grid26_depth, "max")
grid26_depth$min_depth <- exactextractr::exact_extract(bathy_proj, grid26_depth, "min")

#lookieloo
grid26_depth %>%
  ggplot(.) +
  geom_sf(aes(fill=mean_depth),colour=NA)

#Any average depths above 0 meter elevation?
grid26_depth %>%
  mutate(above0=ifelse(mean_depth>0, "Y", "N")) %>%
  ggplot(.) +
  geom_sf(aes(fill=above0),colour=NA)

#Any minimum depths above 0 meter elevation?
grid26_depth %>%
  mutate(above0_min=ifelse(min_depth>0, "Y", "N")) %>%
  ggplot(.) +
  geom_sf(aes(fill=above0_min),colour=NA)

# Quick plot but it shows the grid also
plot(grid26_depth["mean_depth"])

# This is nicer for the plot (copied from above)
grid26_depth %>%
  ggplot(.) +
  geom_sf(aes(fill=mean_depth),
          colour=NA)

# Actual values are:
grid26_depth["mean_depth"]    # As sf object still, mean_depth and geometry
head(grid26_depth$mean_depth) # Vector of the mean depths, so just showing first 6
length(grid26_depth$mean_depth) # Vector of the mean depths, so just showing first 6

usethis::use_data(grid26_depth,
                  overwrite = TRUE)
