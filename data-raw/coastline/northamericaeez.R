require(mregions2)
require(sf)
require(dplyr)

id <- c(8493,8456,8438,8494)

north_america_eez <- gaz_search(id) |> 
  select(preferredGazetteerName) |> 
  mutate(geometry=gaz_geometry(id)) |> 
  st_as_sf()

usethis::use_data(north_america_eez, overwrite = TRUE)

sinew::makeOxygen(north_america_eez)

# ggplot(north_america_eez) +
#   geom_sf() +
#   coord_sf(xlim = c(-80, -30), ylim = c(35, 70), expand = FALSE)
