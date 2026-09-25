library(magick)
library(sf)

# Load the elusive.png image
img_path <- "inst/elusive.png"
img <- image_read(img_path)

# Convert to grayscale and invert (so black becomes white/high values)
img_bw <- img %>%
  image_convert(colorspace = "gray") %>%
  image_negate()

# Extract pixel data as a matrix
img_array <- as.integer(image_data(img_bw, channels = "gray"))
img_matrix <- img_array[1, , ]  # Extract the matrix from the array

# Find pixels that are part of the foreground (Sasquatch)
# These will be high values (white) after negation
threshold <- 128
foreground <- img_matrix > threshold

# Get coordinates of foreground pixels
coords_list <- which(foreground, arr.ind = TRUE)
colnames(coords_list) <- c("y", "x")

cat("Total foreground pixels:", nrow(coords_list), "\n")
cat("Image dimensions:", dim(img_matrix), "\n")

# Normalize to 0-1 range
coords_norm <- coords_list
coords_norm[, "x"] <- (coords_list[, "x"] - 1) / (ncol(img_matrix) - 1)
coords_norm[, "y"] <- (coords_list[, "y"] - 1) / (nrow(img_matrix) - 1)

# Y needs to be inverted (image coordinates are top-down)
coords_norm[, "y"] <- 1 - coords_norm[, "y"]

# Find the convex hull to get the boundary
hull_indices <- chull(coords_norm[, "x"], coords_norm[, "y"])
hull_coords <- coords_norm[hull_indices, ]

cat("Convex hull points:", nrow(hull_coords), "\n")

# Order hull points counterclockwise
centroid_x <- mean(hull_coords[, "x"])
centroid_y <- mean(hull_coords[, "y"])
angles <- atan2(hull_coords[, "y"] - centroid_y, hull_coords[, "x"] - centroid_x)
hull_coords_ordered <- hull_coords[order(angles), ]

# Add first point at end to close polygon
hull_coords_closed <- rbind(hull_coords_ordered, hull_coords_ordered[1, ])

# Scale to BC region
lon_min <- -126.0
lon_max <- -125.0
lat_min <- 48.0
lat_max <- 49.5

sasquatch_coords <- hull_coords_closed
sasquatch_coords[, "x"] <- lon_min + hull_coords_closed[, "x"] * (lon_max - lon_min)
sasquatch_coords[, "y"] <- lat_min + hull_coords_closed[, "y"] * (lat_max - lat_min)

# Create polygon
sasquatch_polygon <- sf::st_polygon(list(sasquatch_coords))
sasquatch <- sf::st_sf(geometry = sf::st_sfc(sasquatch_polygon))

# Plot to verify
plot(sasquatch, main = "Sasquatch from elusive.png")

# Save the coordinates for inspection
print(head(sasquatch_coords, 20))
