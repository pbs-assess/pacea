# Make oni plot with bars and no axes. Fiddly to remove bottom axis so just
# blank out in ppt.

png(paste0(here::here(),
           "/talks/oni-bars-only.png"))

plot(oni,
     smooth_over_year = TRUE,
     lwd = 6,
     axes = FALSE,
     xlab = "",
     ylab = "",
     tickmarks = NULL,
     y_tick_by = 100)


dev.off()
