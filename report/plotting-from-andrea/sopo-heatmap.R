# Adapting andy_share_SOPO_heatmap_annual.R from Andrea to work locally. To then
# adapt to use for ecosystem summaries.

load_all()  # or library(pacea)
library(forcats)
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)
theme_set(theme_bw())

rename_stns <- function(stn_name) {
  stn_name = str_replace_all(string = stn_name, pattern = "_", replacement = "")
  stn_name[stn_name == "SKB"] = "SK-B"
  stn_name[stn_name == "AOI"] = "TḥT"
  stn_name = forcats::fct_relevel(stn_name, c("SK-B","TḥT",#offshore
                                              "GHO","GHW","GHS","GHE",
                                              "SRN","SRC","SRS",#sponge reefs
                                              "SI","BC6"))
  return(stn_name)
}

# Andy will have data already formatted so might not need this.
#Let's make a heatmap of SST from a few different regions
lf = list.files("./andy_share/", pattern = "CRW_5km_SST.csv", full.names = TRUE)
crw = lapply(1:length(lf), function(k) read.csv(lf[k]))
crw = do.call("rbind", crw)
crw$Date = as.Date(crw$Date)
crw$regname = rename_stns(crw$regname)
crw = crw %>% filter(yday(Date) < 366,
                     regname != "BC6",
                     regname != "EHV")

sub = crw

# Calculate climatology

clim = sub %>%
  filter(year(Date) >= 1991, year(Date) <= 2020) %>%
  group_by(regname) %>%
  summarize(climmean = mean(sstmean, na.rm=T)) %>% ungroup()

sub = full_join(sub, clim, by = "regname")



sub %>%
  filter(#year(Date) >= 1998,
    year(Date) < 2026,
    !(regname %in% c("SSB","NSB","EEZ","OPB","SOG"))) %>%
  mutate(regname = forcats::fct_relevel(regname, rev(c("SK-B","TḥT",#offshore
                                                       "GHO","GHW","GHS","GHE",
                                                       "SRN","SRC","SRS",#sponge reefs
                                                       "SI")))) %>%
  ggplot(aes(x = year(Date), y = regname)) +
  geom_tile(aes(fill = sstmean), colour = "black") +
  scale_fill_gradientn(colours = rev(pals::brewer.spectral(20)),
                       limits = c(8.3,12.9),
                       name = bquote("Annual mean SST ("*degree*C*")")) +
  scale_x_continuous(expand = c(0,0), name = NULL,
                     breaks = seq(1985,2025,1)) +
  scale_y_discrete(expand = c(0,0), name = NULL) +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 12),
        panel.spacing.y = unit(0.1,"lines")
  ) +
  geom_text(aes(label = round(sstmean, 1)), size = 4) +
  guides(fill = guide_colorbar(barwidth = 15, title.position = "top",
                               frame.colour = "black",
                               ticks.colour = "black")) +
  labs(caption = "Baseline period: 1991-2020\nDataset: CoralTemp 5km SST")

ggsave(filename = "Annual_heatmap_meanSST_SOPOregions_allyears.png", width = 9, height = 3., dpi = 300,
       scale = 1.4)


sub %>%
  filter(#year(Date) >= 1998,
    year(Date) < 2026,
    !(regname %in% c("SSB","NSB","EEZ","OPB","SOG"))) %>%
  mutate(regname = forcats::fct_relevel(regname, rev(c("SK-B","TḥT",#offshore
                                                       "GHO","GHW","GHS","GHE",
                                                       "SRN","SRC","SRS",#sponge reefs
                                                       "SI")))) %>%
  ggplot(aes(x = year(Date), y = regname)) +
  geom_tile(aes(fill = sstmean-climmean), colour = "black") +
  scale_fill_gradientn(colours = pals::ocean.balance(20)[3:18],
                       limits = c(-1.7, 1.7),
                       name = bquote("Annual SST anomaly ("*degree*C*")")) +
  scale_x_continuous(expand = c(0,0), name = NULL,
                     breaks = seq(1985,2025,1)) +
  scale_y_discrete(expand = c(0,0), name = NULL) +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 12),
        panel.spacing.y = unit(0.1,"lines")
  ) +
  geom_text(aes(label = round(sstmean-climmean, 1)), size = 3.5) +
  guides(fill = guide_colorbar(barwidth = 15, title.position = "top",
                               frame.colour = "black",
                               ticks.colour = "black")) +
  labs(caption = "Baseline period: 1991-2020\nDataset: CoralTemp 5km SST")

ggsave(filename = "Annual_heatmap_SST_anomaly_SOPOregions_allyears.png", width = 9, height = 3., dpi = 300,
       scale = 1.4)
