require(mregions2)
require(sf)
require(dplyr)

id <- c(8493,8456,8438,8494)

northamericaeez <- gaz_search(id) |> 
  select(preferredGazetteerName) |> 
  mutate(geometry=gaz_geometry(id)) |> 
  st_as_sf()

usethis::use_data(northamericaeez, overwrite = TRUE)

sinew::makeOxygen(northamericaeez)

# ggplot(northamericaeez) +
#   geom_sf() +
#   coord_sf(xlim = c(-80, -30), ylim = c(35, 70), expand = FALSE)
