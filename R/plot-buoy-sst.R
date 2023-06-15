# From Andrea, this is plotting code, currently ignoring in R build.
# For climatology, which I don't really need.
sstclim <- data_buoy_full3 %>%
  filter(year(date) <= 2020, # Period 1991-2020 for climatology
         !(stn_id %in% c("C46303", "C46304","C46182","C46134"))) %>%
  group_by(name_key,
           jday = yday(date)) %>%
  summarise(SSTP_10perc_day = quantile(SSTP, probs = 0.1, na.rm=T),
         SSTP_90perc_day = quantile(SSTP, probs = 0.9, na.rm=T),
         SSTP_clim_mean_day = mean(SSTP, na.rm=T),
         SSTP_clim_n = sum(!is.na(SSTP)),
         col_key = unique(col_key),
         numobs_stn = sum(!is.na(SSTP))) %>%
  ungroup() %>% filter(numobs_stn > 0)
sstclim$fakedate = as.Date(paste(plot_yrs[1], sstclim$jday), format = "%Y %j")


# Buoy map ####
world <- ne_countries(scale = 10, returnclass = "sf")#, country = c("Canada")
buoyplot <- data_buoy_full3 %>% filter(year(date) >= plot_yrs[1]) %>%
  dplyr::select(name_key, lon, lat, col_key) %>% distinct()
buoyplot$test <- str_extract(string=buoyplot$name_key,
                             pattern = "C[0-9]{5}")

g <- ggplot() +
  geom_sf(data = world, colour = "grey70", fill = "grey95") +
  geom_point(data = buoyplot, aes(x = lon, y = lat, fill = col_key), shape = 21, size = 3) +
  scale_fill_identity(labels = buoyplot$name_key,
                      breaks = buoyplot$col_key, guide = "legend") +
  coord_sf(xlim = c(-140, -121.), ylim = c(46.7, 55.8), expand = F) +
  xlab(NULL) + ylab(NULL) +
  scale_y_continuous(breaks = seq(48,56,2)) +
  labs(fill = NULL) +
  ggrepel::geom_text_repel(data = buoyplot,
                           aes(x = lon, y = lat, label = name_key),
                           size = 2.5) + #, nudge_x = 1, nudge_y = -0.1
  theme(legend.position = "none") +
  labs(caption = paste0("DFO_MEDS_BUOYS data last updated: ", max(dfo_daily_mean$date,na.rm=T),
                        "\nECCC_MSC_BUOYS data last updated: ",
                        if_else(max(opp_daily_mean$date,na.rm=T) <= Sys.Date(), max(opp_daily_mean$date,na.rm=T), Sys.Date()),
                       "\nPlot last updated:", Sys.Date()))
# ggsave("Buoy_quickmap_colours.png", units = "in", width = 4, height = 4, scale = 1.9)

# Get the current year and last year's data
data_buoy_full3$fakedate = as.Date(paste(plot_yrs[1], yday(data_buoy_full3$date)), format = "%Y %j")

prevyr = data_buoy_full3 %>% filter(year(date) == plot_yrs[1])
currentyr = data_buoy_full3 %>% filter(year(date) == plot_yrs[2])

dayseq = seq.Date(as.Date(paste0(plot_yrs[1], "-01-01")),
                  as.Date(paste0(plot_yrs[1], "-12-31")), by = "day")
currentyr = currentyr %>%
  # filter(year(date) == plot_yrs[2]) %>%
  group_by(name_key) %>%
  tidyr::complete(fakedate = seq.Date(min(fakedate), max(fakedate), by = "day")) %>%
  ungroup() %>%
  arrange(name_key, fakedate)

prevyr = prevyr %>%
  # filter(year(date) == plot_yrs[1]) %>%
  group_by(name_key, col_key) %>%
  tidyr::complete(fakedate = dayseq) %>%
  ungroup() %>%
  arrange(name_key, fakedate)

# Time series plot ####
s <- prevyr %>%
  arrange(STN_ID, fakedate) %>%          # STN_ID %in% c("C46206","C46146")) %>%
  ggplot() +
  facet_wrap(~name_key, ncol = 3) +
  geom_ribbon(data = sstclim, aes(x = fakedate, ymin = SSTP_10perc_day, ymax = SSTP_90perc_day), fill = "grey70", colour = NA, alpha = 0.5) +
  geom_path(data = sstclim, aes(x = fakedate, y = SSTP_clim_mean_day), colour = "grey30", linewidth = 0.8) +
  # Heat dome - for 2021-2022 plots
  # geom_vline(xintercept = as.Date("2021-06-26"), linetype = "dashed") +
  geom_path(data = prevyr %>% filter((year(date) == plot_yrs[1]) | is.na(SSTP)), aes(x = fakedate, y = SSTP, colour = col_key), size = 0.6) +

  geom_path(data = currentyr %>% filter((year(date) == plot_yrs[2]) | is.na(SSTP)),aes(x = fakedate, y = SSTP), colour = "black", size = 1.4, lineend = "round") +
  geom_path(data = currentyr %>% filter(year(date) == plot_yrs[2] | is.na(SSTP)), aes(x = fakedate, y = SSTP), colour = "white", size = 0.5, lineend = "round") +
  scale_colour_manual(values = (glasbey_mod[1:(nrow(buoyplot))])) +
  scale_colour_identity(breaks = buoyplot$col_key) +
  ylab(expression("Mean Daily SST " ( degree*C))) +
  xlab(NULL) +
  scale_x_date(labels = scales::date_format("%b"), breaks = "2 months") +
  theme(legend.position = "none",
        strip.background = element_rect(colour = "grey70", fill = "grey95"))
 # s
# ggsave("Daily_mean_SSTP.png", width = 6, height = 4, scale = 1.6)

# Assemble and save ####

library(cowplot)

gg <- plot_grid(s, g,rel_heights = c(1,1), rel_widths = c(1.1,0.9))

ggsave(filename = paste0("Daily_mean_buoy_overview_",plot_yrs[2],".png"),
       plot = gg, width = 10, height = 7,
       units = "in", scale = 1.25)

# TODO See if any raw data are from Wallace, do up a figure. 45deg 48 min N, 63deg 28 min W.
