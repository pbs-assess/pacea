# Testing the overlap of hotssea and bccm when using full grid, on two initial
# sample files. May not be
# perfect because grid is not predefined but recalculated each time, and
# slightly differently between the two. If they are different then talk to
# Andrea or Kelsey. If they match then should be good.
# Run through this line-by-line. Went through with Andrea and looks good.

# To chat with Andrea:
# recap: hotssea is SoG, bccm is outer waters, there's a slight overlap. Want
# the grids to line up. Travis didn't predefine a grid (for the original bccm),
# it gets constructed each time as being within a big polygon. Done that here
# using common bccm_hotssea_poly . Didn't want to change the approach (i.e. make
# a specific grid once) because Travis spent a lot of time on it and I want to
# follow his steps.

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

plot(bccm_hotssea_poly) # To show the polygon, as previously chatted
                        # about. Turns out some squares (for hotssea at least)
                        # are not fully inside the polygon, but I think that's
                        # fine. And it all works.


# Note one is salinity and one is temperature, so they won't match up
#  numerically anyway. Just looking at the grid.

# hotssea rendered to 2x2 km
load(paste0(pacea_data_dir,
                           "hotssea/hotssea_avg0to30m_salinity_mean_01.rds"))
h <- hotssea_avg0to30m_salinity_mean$geometry   # Just the geometry; note the 01 is lost from the variable name
h_one_res <- hotssea_avg0to30m_salinity_mean[, 1]   # Just January 1980

# bccm rendered to 2x2 km grid, in a way that should align with the hotssea one
# Takes a while to load (implying getting the full 120Mb):
load(paste0(pacea_data_dir,
            "/bccm_avg0to40m_temperature_full_01.rds"))
b <- bccm_avg0to40m_temperature_full$geometry
b_one_res <- bccm_avg0to40m_temperature_full[, 1]   # Just January 1993

h   # all polygons, 6156 features
b   # all polygons (checked over half, there's lots), 161025 features

# TODO is there a way to see if all geometries are POLYGONS? Aha - if Geometry
# type is POLYGON is that the case? Think so (ask Andrea).

plot(h)
plot(hotssea_poly, add = TRUE)   # doesn't actually encompass all the
                                 # squares. Which explains why we have some
                                 # squares that appear partly out of it
plot(b)


# The Problem: want to confirm that the polygons in the small overlapping area
#  are equivalent for h and b.

overlap <- st_intersection(h, b)
overlap        # Geometry type: GEOMETRY, and has POINT and LINESTRING.
plot(overlap)  # looks okay, circles at corners must be
               # because st_intersection creates points and linestrings.
# Only shows full squares
# That's what we want but without the dots. Then just want to check the squares
# that intersect h match the squares that intersect b. Not done this yet, tried
# some other things. Though I think the squares are those that are more than
# half intersecting. There are 9 in second-right column.

# Help files and vignette don't seem to explain exactly what st_intersection does.

# Get same results the other way around:
#overlap_2 <- st_intersection(b, h)
# overlap_2   # type GEOMETRY and has POINT and LINESTRING.
# plot(overlap_2)

# Maybe don't want hotssea_poly, that's the rectangle? But that is the
# full grid, I think. All works though, and doesn't seem to exclude any hotssea.

plot(st_difference(h, hotssea_poly), col = "red", border = "red")   # A few small bits are outside
# plot(hotssea_poly, add = TRUE)


# b in hotssea_poly
overlap_b_hotssea_poly <- st_intersection(b, hotssea_poly)  # creates geometry
                                        # of shared portion
overlap_b_hotssea_poly
plot(overlap_b_hotssea_poly)  # Looks as expected, squares in the overlapping
                              # area, but also shows incomplete squares. As it's
                              # just looking at the polygons and then cutting
                              # them off, presumably.
# Has 9 squares (bottom is incomplete) in the 2nd column from the right. So
# previous one was showing some incomplete squares. But not the bottom two in
# the 3rd column from the right.



# Did some stuff further below, but now think this might be all that we need:
h_in_overlap <- st_within(h, overlap)   # h has 6165 features, overlap has 352,
h_in_overlap           # gets saved as a sparse geometry binary predicate list
                       # (class sgbp and list) of length 6165
dim(h_in_overlap)      # 6165 x 352, makes sense
h_in_overlap_ind <- which(lengths(h_in_overlap) > 0)
length(h_in_overlap_ind)   # 39
h_in_overlap_ind
plot(h[h_in_overlap_ind])   # Still has all 9 though
plot(hotssea_poly, add = TRUE, border = "red")

# Then do the same for b
b_in_overlap <- st_within(b, overlap)   # b has 161025 features, overlap has 352,
b_in_overlap           # gets saved as a sparse geometry binary predicate list
                       # (class sgbp and list) of length 161025
dim(b_in_overlap)      # 161025 x 352
b_in_overlap_ind <- which(lengths(b_in_overlap) > 0)
length(b_in_overlap_ind)   # 39 also! Slightly surprising it worked, given boxes
                           # tweak outside of hotssea_poly (which I now think I
                           # should define better maybe).
b_in_overlap_ind
windows()
plot(b[b_in_overlap_ind])
# Looks the same as h one, though no scale.
# Add in the h ones.
plot(h[h_in_overlap_ind], add = TRUE, col = "red")
# Think that is somewhat convincing, though no guarantee everything isn't just
# translated (shifted).

# Need to look at with the coast added. Not comapring the data (different), just
#  the location of the squares.
h_to_plot <- h_one_res[h_in_overlap_ind, ]
plot.pacea_st(h_to_plot, years = "1980", month = "Jan")

b_to_plot <- b_one_res[b_in_overlap_ind, ]
attr(b_to_plot, "restrict_plotting_range") <- TRUE   # Fudge until I fix #63
attr(b_to_plot, "units") <- attr(h_to_plot, "units")  # Fudge to get plotting
                                        # exactly the same; ignore the data (the
                                        # word "Temperature" is longer than
                                        # "Salinity" so it gets shifted, and I
                                        # currently only have a different one
                                        # rendered for each model).
windows()
plot.pacea_st(b_to_plot, years = "1993", month = "Jan")

# Woo-hoo, the squares' locations match up. As desired.

HERE - keep the rest below for reference and learning, but the above is what we
need. So stop here.

# Having done the stuff below, and then looking at an sf vignette, think this
# might be the way to go, but turns out not.
# inter <- st_intersects(h, b)


# No:
#overlap_h_grid26 <- st_intersection(h, grid26)
#plot(overlap_h_grid26)  # has lots of lines, as expected, no reason to line up

# h in the overlapping of b and hotssea_poly:
overlap_h_overlap_b_hotssea_poly <- st_intersection(h,
                                                    overlap_b_hotssea_poly)
overlap_h_overlap_b_hotssea_poly
plot(overlap_h_overlap_b_hotssea_poly)  # Similar but not the same as the
                                        # earlier one
# not sure why these aren't all complete squares. It's because
# overlap_b_hotssea_poly is parts of any b squares that lie within
# hotssea_poly. Aha - the circles are not there for incomplete squares. TODO
# understand why.
# Has 9 squares in second from right, including (clearly) the one with >0.5,
# next column ignores one that would be <0.5. Might not be 0.5, criteria might
# be three points within the overlap?

overlap_b_hotssea_poly_boundary <- st_boundary(overlap_b_hotssea_poly)
overlap_b_hotssea_poly_boundary
plot(overlap_b_hotssea_poly_boundary)
# Same as previous but no dots, and shows squares with only a tiny bit inside poly.

#overlap_b_hotssea_poly_buffer <- st_buffer(overlap_b_hotssea_poly[, "geometry"],
#                                           dist = 0) %>%
#  st_union()
#plot(overlap_b_hotssea_poly_buffer)

overlap_b_hotssea_poly_union <- st_union(overlap_b_hotssea_poly) # [, "geometry"])
overlap_b_hotssea_poly_union
plot(overlap_b_hotssea_poly_union)
# Just a single polygon, but includes incomplete squares

# Yes!
overlap_h_overlap_b_hotssea_poly_union <- st_intersection(h,
                                                          overlap_b_hotssea_poly_union)
overlap_h_overlap_b_hotssea_poly_union
plot(overlap_h_overlap_b_hotssea_poly_union)
# Is what we want. Look like only shows squares that are more than half within
# the polygon. Not sure why there are some dots.

# Then compare with
windows()
plot(overlap_b_hotssea_poly)
# Shows parts of any square that is within the polygon

# At some point I had a plot that was just full squares, but I think those are
# not necessarily fully within the overlap. We just want ones that are fully
# within I think, as then they'll be in h and b.


st_area(overlap_h_overlap_b_hotssea_poly_union) %>% sum()
st_area(overlap_b_hotssea_poly) %>% sum()
# Not exactly the same, second is larger which makes sense.

# To get geometry for full results I had to do b[1, ]$geometry[[1]]
