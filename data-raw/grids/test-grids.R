library(dplyr)
load_all()

# Shapefile of the entire BC Coastline for plotting
Coastline <- sf::st_as_sf(PACea::Coastline)
# Okay, should resave that as an sf so it's in the right format within the
# package.

# test_grid_2 = test   # not re-running, doing here in case something messes up
# Make different test grids based on cell size. test_grid_X has cells X by X km.
test_grid_2 <- make_grid(Coastline,
                         cell_size_x = 2,
                         cell_size_y = 2)

test_grid_10 <- make_grid(Coastline,
                          cell_size_x = 10,
                          cell_size_y = 10)

test_grid_20 <- make_grid(Coastline,
                          cell_size_x = 20,
                          cell_size_y = 20)

usethis::use_data(test_grid_2, overwrite = T)    # .rda file size is 1.6 Mb
usethis::use_data(test_grid_10, overwrite = T)   # .rda file size is  85 kb
usethis::use_data(test_grid_20, overwrite = T)   # .rda file size is 25 kb

# 2x2 km gives 46,198 cells   (max(test_grid_2$id)
# 5x5 km gives 8,249 cells
# 10x10 km gives 2,275 cells   max(test2$id)
# 20x20 km gives 637 cells

# so 10 years of monthly data at 2x2 km would be 5.5 million values. Need to test compression.

plot(Coastline)
plot(test_grid_10, add = TRUE)
plot(Coastline, add=TRUE)    # Coastline is offset if plot test first
