# Create Area 126 sf polygon to test various functions (before making all areas
# in a consistent format)

load_all()

pfma_area_126_lat_lon_matrix <-
  matrix(c(-127.1506, -128.2331, -129.3492, -127.9167, -127.1847, -126.8200, -127.1506,
           49.85766, 49.00000, 48.99991, 50.11915, 50.40183, 50.24466, 49.85766),
         ncol = 2)

pfma_area_126_temp <- create_area_sf_polygon(pfma_area_126_lat_lon_matrix)

pfma_area_126_temp

plot(pfma_area_126_temp)    # just draws the polygon of the area

# Plot the latest OISST data plus draw the polygon of the area
p <- plot(oisst_month)

p + geom_sf(data = pfma_area_126_temp,
            fill = NA,
            colour = "black",
            linewidth = 1)

usethis::use_data(pfma_area_126_temp,
                  overwrite = TRUE)

