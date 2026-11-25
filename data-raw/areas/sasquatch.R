# Create an sf object in the shape of a sasquatch, just to test various
# functions.

# Mostly written by Claude Haiku 4.5 with the request from Andy, then lots of
#  iterations to get it working; was useful in learning how to better use AI.
# Original request, though then decided to do it in steps. And in hindsight I
#  should have just given a bounding box to fit it in; it did the original
#  scaling very well, just had trouble with trying to make concave polygons,
#  until I told it I don't think those were really necessary.
#
# I want to create a 'simple feature' (from the sf R package) polygon. It will
# be in the shape of a Sasquatch (the classic look of it walking as viewed from
# the side; can use /inst/elusive.png). In our R package we have the sf polygons `bc_eez` and
# `bc_coast`. The sasquatch should lie within `bc_eez` but not overlap
# `bc_coast`, and be fully south of 50 degrees north. Please make a plan, which
# can then go into the template file I created in areas/sasquatch.R. The final
# `sasquatch` sf object will become a data object in the package, as seen by the
# use_data command I have already inserted.

load_all()
library(magick)
library(imager)
library(sf)
library(concaveman)
library(ggplot2)

# Load the elusive.png image
img_path <- paste0(here::here(),
                   "/inst/elusive.png")
img <- image_read(img_path)

# Convert to grayscale
img_gray <- img %>%
  image_convert(colorspace = "gray")

# Display image info to verify
cat("Image dimensions:", image_info(img_gray)$width, "x", image_info(img_gray)$height, "\n")

# Convert to imager format for edge detection
img_imager <- magick2cimg(img_gray)

# Detect edges using Canny edge detection
edges <- cannyEdges(img_imager,
                    alpha = 1,
                    sigma = 1)

# Convert edges to matrix (no need for 4D indexing)
edges_matrix <- as.matrix(edges)
edge_coords <- which(edges_matrix > 0,
                     arr.ind = TRUE)

cat("Number of edge pixels detected:", nrow(edge_coords), "\n")

# Normalize coordinates to (0-1) range
img_height <- nrow(edges_matrix)
img_width <- ncol(edges_matrix)

x_norm <- (edge_coords[, 2] - 1) / (img_width - 1)
y_norm <- 1 - (edge_coords[, 1] - 1) / (img_height - 1)

coords <- cbind(x_norm, y_norm)

cat("Normalized coordinates range:\n")
cat("  X:", range(coords[, 1]), "\n")
cat("  Y:", range(coords[, 2]), "\n")

# Rotate 90 degrees clockwise
x_norm_rot <- y_norm
y_norm_rot <- 1 - x_norm

plot(x_norm_rot, y_norm_rot)

# Convert edge pixels to sf multipolygon

# Create coordinates matrix
coords_rot <- cbind(x_norm_rot, y_norm_rot)

# Scale to geographic coordinates (BC region; doing manually)
lon_min <- -132.0
lon_max <- -128.0
lat_min <- 47.0
lat_max <- 53.0

x_geo <- lon_min + coords_rot[, 1] * (lon_max - lon_min)
y_geo <- lat_min + coords_rot[, 2] * (lat_max - lat_min)

coords_geo <- cbind(x_geo, y_geo)

# Create concave hull using concaveman with matrix input

# concaveman expects a matrix with x, y columns
concave_hull <- concaveman(coords_geo)

# Scale to 80% of current height (did this already then I tweaked the
# coordinates above, so leave this in)
# Find centroid and scale y-coordinates
centroid_y <- mean(concave_hull[, 2])
concave_hull[, 2] <- centroid_y + 0.8 * (concave_hull[, 2] - centroid_y)

# Convert to sf polygon
sasquatch <- st_sf(
  geometry = st_sfc(st_polygon(list(concave_hull)),
                    crs = st_crs(oisst_month))
)

p <- plot(oisst_month)
p + geom_sf(data = sasquatch,
            fill = NA,
            colour = "black",
            linewidth = 1)

# Extract sst within the area
sasquatch_sst <- oisst_month[sasquatch, ]
class(sasquatch_sst) <- class(oisst_month)    # will make a function for that,
# or an option in the plotting

plot(sasquatch_sst)

pp <- plot(sasquatch_sst)
pp + geom_sf(data = sasquatch,
            fill = NA,
            colour = "black",
            linewidth = 1)
# Can see that, as Kelsey explained, it's the centres of the SST squares that
# determine whether the square is considered part of the area or not.

# Separate plot of just the sasquatch with axis labels
plot(sasquatch,
     main = "Sasquatch",
     axes = TRUE)

# Check the geometry
sasquatch

usethis::use_data(sasquatch,
                  overwrite = TRUE)

