# Create an sf object in the shape of a sasquatch, just to test various
# functions.

# Mostly written by Claude Haiku 4.5 with the request from Andy, then lots of
# iterations to get it working.
# I want to create a 'simple feature' (from the sf R package) polygon. It will
# be in the shape of a Sasquatch (the classic look of it walking as viewed from
# the side; can use /inst/elusive.png). In our R package we have the sf polygons `bc_eez` and
# `bc_coast`. The sasquatch should lie within `bc_eez` but not overlap
# `bc_coast`, and be fully south of 50 degrees north. Please make a plan, which
# can then go into the template file I created in areas/sasquatch.R. The final
# `sasquatch` sf object will become a data object in the package, as seen by the
# use_data command I have already inserted.

# Load pacea to access bc_eez and bc_coast for validation
load_all()
library(sf)
library(magick)
library(imager)
library(raster)

# Extract Sasquatch silhouette from inst/elusive.png using image processing

# Load the elusive.png image
img_path <- paste0(here::here(),
                   "/inst/elusive.png")
img <- image_read(img_path)

# Convert to grayscale
img_gray <- img %>%
  image_convert(colorspace = "gray")

# Get image dimensions
img_info <- image_info(img_gray)
img_width <- img_info$width
img_height <- img_info$height

# Extract pixel data and reshape to 2D matrix
img_data <- as.integer(image_data(img_gray, channels = "gray"))
img_matrix <- matrix(img_data, nrow = img_height, ncol = img_width, byrow = TRUE)

# Find pixels that are part of the foreground (Sasquatch - dark pixels)
threshold <- 128
foreground <- img_matrix < threshold  # Black/dark pixels are below threshold

# Get coordinates of foreground pixels
coords_idx <- which(foreground, arr.ind = TRUE)

# Extract x and y
y_coords <- coords_idx[, 1]
x_coords <- coords_idx[, 2]

# Normalize to 0-1 range
x_norm <- (x_coords - 1) / (img_width - 1)
y_norm <- (y_coords - 1) / (img_height - 1)

# Y needs to be inverted (image coordinates are top-down)
y_norm <- 1 - y_norm

coords_norm <- cbind(x_norm, y_norm)

dim(coords_norm)           # should be large, e.g. 69607 rows, 2 columns

# Trace the boundary using sf and stars packages
# Convert the boolean raster to a stars object, then to sf polygon

# Downsample the foreground to reduce complexity for rasterToPolygons
# Use aggregation to reduce the number of cells while preserving shape
downsample_factor <- 5  # Aggregate 5x5 cells into 1
foreground_small <- aggregate(raster(foreground * 1.0),
                              fact = downsample_factor,
                              fun = max)

# Convert to binary (threshold at 0.5)
foreground_small <- foreground_small > 0.5

# Create a raster from the downsampled foreground
r <- foreground_small
extent(r) <- c(0, 1, 0, 1)

# Convert raster to sf polygon (handles boundary tracing automatically)
poly <- st_as_sf(rasterToPolygons(r, na.rm = TRUE, dissolve = TRUE))

# Extract coordinates from the polygon
if(nrow(poly) > 0){
  sasquatch_coords <- st_coordinates(poly)[, 1:2]
} else {
  stop("Failed to convert raster to polygon")
}

# Scale to BC region
lon_min <- -126.0
lon_max <- -125.0
lat_min <- 48.0
lat_max <- 49.5

sasquatch_coords[, 1] <- lon_min + sasquatch_coords[, 1] * (lon_max - lon_min)
sasquatch_coords[, 2] <- lat_min + sasquatch_coords[, 2] * (lat_max - lat_min)

# Close the polygon by adding first point at the end
sasquatch_coords <- rbind(sasquatch_coords, sasquatch_coords[1, ])

# Create sf polygon
sasquatch_polygon <- sf::st_polygon(list(sasquatch_coords))
sasquatch <- sf::st_sf(
  geometry = sf::st_sfc(sasquatch_polygon, crs = sf::st_crs(bc_eez))
)

plot(sasquatch)

# Validate constraints
cat("Sasquatch bounding box:\n")
print(sf::st_bbox(sasquatch))

cat("\nSouth of 50°N:", all(sf::st_bbox(sasquatch)["ymax"] < 50), "\n")

cat("Entirely within bc_eez:", all(sf::st_within(sasquatch, bc_eez)), "\n")

cat("No overlap with bc_coast:", all(sf::st_disjoint(sasquatch, bc_coast)), "\n")



usethis::use_data(sasquatch,
                  overwrite = TRUE)

