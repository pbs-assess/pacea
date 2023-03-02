make_grid <- function(coastline,
                      num_x = 10,
                      num_y = 10,
                      cell_size_x = 20,
                      cell_size_y = 20,
                      rotang = 318.5){
# DECIDE WHICH TO INCLUDE AS DEFAULT, num or cell size

# create large pixel grid over the simplified coastline
  # Rotate the grid to maximize the spatial overlap
#  rotang = 318.5

  center <- sf::st_centroid(sf::st_union(coastline))

  # This is the function call which actually creates the master grid
  # To increase the resolution (almost certainly desired), increase the
  # n argument (e.g. n=c(100,100)) or reduce the cellsize (which seems more
  # intuitive, it should be each direction in km's.

  Coastline_Grid <- sf::st_sf(
                        sf::st_make_grid(tran(sf::st_geometry(coastline),
                                              -rotang,
                                              center),
                                         cellsize = c(cell_size_x,
                                                      cell_size_y)))

# Have this option for numbers of cells, though am thinking cell_size is more intuitive
#  Coastline_Grid <- sf::st_sf(
#                          sf::st_make_grid(tran(sf::st_geometry(Coastline),
#                                                -rotang, center),
#                                           n=c(10,10)))

  Coastline_Grid <- tran(sf::st_geometry(Coastline_Grid),
                         rotang,
                         center)

  # Keep only the polygons that fall within or touch the original Coastline to save storage space
  Coastline_Grid <- sf::st_as_sf(sf::as_Spatial(Coastline_Grid))
  sf::st_crs(Coastline_Grid) <- sf::st_crs(coastline)

  # Name the individual polygons by row-column method
  # Note that sf works from bottom-left to top-right
  # The first grid cell is the bottom-left grid cell (1,1)
  # The final grid cell is the top-right grid cell (n,n)
  # NEED TO CHANGE THE VALUES 10 TO MATCH THE CHOICE OF n=c(,) above!!
# Only currently works for cell_size_x = cell_size_y = 20
  Coastline_Grid$Poly_Name <- paste0('Grid_Index_(',
                                     rep(1:17, each = 53), # repeat each value of 1:cols, nrows
                                     ',',
                                     rep(1:53, times = 17), # repeat sequence 1:nrows, ncols times
                                     ')')
# Don't get how that works given we've taken some out. BUT I don't think we want
# to do the fancy x,y labelling. That will all be behind the scenes, and it's
# just as simple to have a single number. Think someone suggested the x,y
# format, probably thinking the user would see that more (also before we thought
# about much higher resolution).

  Coastline_Grid  <- Coastline_Grid[Coastline, ]

  Coastline_Grid
}
