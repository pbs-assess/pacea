# create empty sf grid

library(terra)
library(sf)
library(dplyr)

load_all()

sf_use_s2(FALSE)

#####
# START - load data to environment

# transform bc_coast
tbc <- st_transform(bc_coast, crs=crs(romseez_poly))

# convert to multilinestring
tbc.line <- st_cast(tbc, "MULTILINESTRING")

#####
# make sf grid 
grid2 <- st_make_grid(inshore_poly, cellsize = 2000) %>%
  st_as_sf()
grid2 <- grid2[inshore_poly,]

grid6 <- st_make_grid(offshore_poly, cellsize = 6000) %>%
  st_as_sf()
grid6 <- grid6[offshore_poly,]


# mask 2k grid with 6k grid, then combine grids
tgrid2 <- grid2[!st_intersects(st_union(grid6), grid2, sparse=F, prepared=T),] %>%
  rbind(grid2[st_intersects(st_union(grid6), grid2, sparse=F, prepared=T),])
tgrid26 <- tgrid2 %>% rbind(grid6)

# index points that dont intersect with bc coast shapefile
#  disjoint - do not share space
dis2 <- tgrid26[st_disjoint(st_union(tbc), tgrid26, sparse=F, prepared=T),]

#  convert bc coast to sf linestring and finding coastline intserections separately - increased processing speed
#  using st_intersects is much faster than other predicate functionss
sub.t2 <- tgrid26[st_intersects(st_union(tbc), tgrid26, sparse=F, prepared=T),]
inter.line <- sub.t2[st_intersects(tbc.line, sub.t2, sparse=F, prepared=T),]
grid26 <- rbind(dis2, inter.line)

usethis::use_data(grid26)
