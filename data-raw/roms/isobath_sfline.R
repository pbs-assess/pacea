# create isobath line to sf object

setwd("C:/github/pacea")

library(devtools)
library(sf)
library(sfheaders)
load_all()

# Read in shelf boundary csv

crds <- read.csv("./data-raw/roms/shelf_bry.csv")
head(crds)

# create isobath sf poly line
isobath_200m <- crds %>% 
  dplyr::select("Longitude", "Latitude") %>%
  sf_multilinestring() %>%
  st_set_crs(value = "EPSG: 4326") %>%
  st_transform(crs = "EPSG: 3005")
isobath_200m <- isobath_200m[,-1]

usethis::use_data(isobath_200m, overwrite = TRUE)
