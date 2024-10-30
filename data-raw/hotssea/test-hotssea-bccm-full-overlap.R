# Testing the overlap of hotssea and bccm when using full grid. May not be
# perfect because grid is not predefined but recalculated each time, and
# slightly differently between the two. If they are different then talk to
# Kelsey and Andrea!

# TODO rerun from scratch once I've rerun hotssea with 2000 m cells.

load_all()

# Not sure if need all these.
library(dplyr)
library(terra)
library(gstat)
library(sf)
library(stars)
library(ggplot2)
library(concaveman)
library(stringr)

sf_use_s2(FALSE)  # remove spherical geometry (s2) for sf operations

pacea_dir <- here::here()   # Will give local pacea/
pacea_data_dir <- paste0(here::here(),
                         "/../pacea-data/data/")  # Where have saved example .rds files


# Note one is salinity and one is temperature, so they won't match up
#  numerically anyway. Just looking at the grid.

load(paste0(pacea_data_dir,
                           "hotssea/hotssea_avg0to30m_salinity_mean_01.rds"))
h <- hotssea_avg0to30m_salinity_mean   # Note the 01 is lost from the variable name

# hot_full <- readRDS("C:/andy18/github/pacea-data/data/hotssea/hotssea_avg0to30m_salinity_mean_01.rds")

# Takes a while to load (implying getting the full 120Mb) but
load(paste0(pacea_data_dir,
            "/bccm_avg0to40m_temperature_full_01.rds"))
b <- bccm_avg0to40m_temperature_full

h
b
# No:
overlap <- st_intersection(h, b) #tt, hotssea_poly)
plot(overlap)  # looks okay, not sure why circles are shown at some corners

# Yes:
overlap_b_hotssea_poly <- st_intersection(b, hotssea_poly)
plot(overlap_b_hotssea_poly)  # Looks as expected, squares in the overlapping area

# No:
overlap_h_grid26 <- st_intersection(h, grid26)
plot(overlap_h_grid26)  # has lots of lines, as expected, no reason to line up

# Yes:
overlap_h_overlap_b_hotssea_poly <- st_intersection(h,
                                                    overlap_b_hotssea_poly)
plot(overlap_h_overlap_b_hotssea_poly)  # Similar but not the same as the
                                        # earlier one

# Yes, kind of:
overlap_b_hotssea_poly_boundary <- st_boundary(overlap_b_hotssea_poly)
plot(overlap_b_hotssea_poly_boundary)
# Doesn't give a full simple polygon, still shows the squares. Aha, as still
# contains the data, not just the geometry. So has colour.

# Yes, kind of:
overlap_b_hotssea_poly_boundary <- st_boundary(overlap_b_hotssea_poly[, "geometry"])
plot(overlap_b_hotssea_poly_boundary)
# No colour, but still full grid.

#overlap_b_hotssea_poly_buffer <- st_buffer(overlap_b_hotssea_poly[, "geometry"],
#                                           dist = 0) %>%
#  st_union()
#plot(overlap_b_hotssea_poly_buffer)

# Yes:
overlap_b_hotssea_poly_union <- st_union(overlap_b_hotssea_poly) # [, "geometry"])
plot(overlap_b_hotssea_poly_union)
# Just a single polygon

# Yes!
overlap_h_overlap_b_hotssea_poly_union <- st_intersection(h,
                                                          overlap_b_hotssea_poly_union)
plot(overlap_h_overlap_b_hotssea_poly_union[, 1])
# Is what we want. Look like only shows squares that are more than half within
# the polygon

# Then compare with
windows()
plot(overlap_b_hotssea_poly[, 1])
# Shows parts of any square that is within the polygon

st_area(overlap_h_overlap_b_hotssea_poly_union) %>% sum()
st_area(overlap_b_hotssea_poly) %>% sum()
# Not exactly the same, second is larger which makes sense.

HERE - think about plots again and calcs. Looking good.

# st_equals(b, h)
# TEMP TODO delete:
# Checking intersections match, after having run just one loop above with j<-jvars[1]
tt <- bccm_avg0to40m_temperature_full
bb <- st_intersection(tt, hotssea_poly)
cc <- st_intersection(hotssea_poly, tt)
These match, comparing POLYGONs bb[1, ]$geometry[[1]] and cc[1, ]$geometry[[1]]
manually, though would expect them to as am using hotssea_poly not a grid.
