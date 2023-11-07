# This is a copy of Andrea's andrea-code/CIOOS_SST_2023_download_plots.R to then
# edit down to just plot the map and save in pacea.


load_all()
# library(rerddap)
# library(dplyr)
library(ggplot2)
theme_set(theme_bw())
# library(rnaturalearthdata)
# library(rnaturalearthhires)
library(rnaturalearth)
# library(lubridate)
# library(stringr)

world <- ne_countries(scale = 10,
                      returnclass = "sf")#, country = c("Canada")

# Use pacea buoy_metadata
buoy_plot <- dplyr::select(buoy_metadata,
                           lat = latitude,
                           lon = longitude,
                           col_key,
                           name_key)

g <- ggplot() +
#  geom_sf(data = world,
#          colour = "grey70",
#          fill = "grey95") +
  geom_sf(data = bc_eez,
          col = "black",
          fill = NA,
          lty = 2) +
  geom_sf(data = bc_coast,
          colour = "grey70",
          fill = "grey95") +
  geom_point(data = buoy_plot,
             aes(x = lon,
                 y = lat,
                 fill = col_key),
             shape = 21,
             size = 3) +
  scale_fill_identity(labels = buoy_plot$name_key,
                      breaks = buoy_plot$col_key,
                      guide = "legend") +
  coord_sf(xlim = c(-140, -121.),
           ylim = c(46.7, 55.8),
           expand = FALSE) +
  xlab(NULL) + ylab(NULL) +
  scale_y_continuous(breaks = seq(48, 56, 2)) +
  labs(fill = NULL) +
  ggrepel::geom_text_repel(data = buoy_plot,
                           aes(x = lon,
                               y = lat,
                               label = name_key),
                           size = 2.5) + #, nudge_x = 1, nudge_y = -0.1
  theme(legend.position = "none")

ggsave("buoy_map_andy.png",
       units = "in",
       width = 4,
       height = 4,
       scale = 1.9)
TODO name of buoy, save this, include in talk and buoy vignette
