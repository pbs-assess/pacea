# Quickly tried the code at the top but didn't get very far. See further down
# Think this is adapted from the hexSticker GitHub site.
library(hexSticker)
s <- sticker(~plot(cars, cex=.5, cex.axis=.5, mgp=c(0,.3,0), xlab="", ylab=""),
         package="hexSticker", p_size=20, s_x=.8, s_y=.6, s_width=1.4, s_height=1.2,
         filename="inst/figures/baseplot.png")

print(s)

p <- sticker(~plot(cars,
                   cex=.5,
                   cex.axis=.5,
                   mgp=c(0,.3,0),
                   xlab="",
                   ylab=""),
             package="hexSticker",
             p_size=20,
             s_x=.8,
             s_y=.6,
             s_width=1.4,
             s_height=1.2,
             filename="inst/figures/baseplot.png")


# Using this to send to Carley, for some ideas for the eventual logo.

load_all()

pdata <- bccm_surface_temperature()
png("temperature-map.png")
plot(pdata, months = "September", years = 2010)
dev.off()

png("el-nino.png")
plot(oni,
     smooth_over_year = TRUE,
     lwd = 7)     # You may have to tweak the line thickness to suit your figure
dev.off()


For Carley:

There's a website of silhouettes of organims, and any of these would be appropriate to use if you like:

Seals:
https://www.phylopic.org/images/41d9f21e-7e0b-42d2-a088-e154bfa63984/phoca-largha
or
https://www.phylopic.org/images/5e359baf-a5f7-4101-8f61-6d42beb52756/pagophilus-groenlandicus

Hake:
https://www.phylopic.org/images/a1e87ee1-590c-4e06-80b5-04ceeab1b0f0/merluccius-productus

Herring:
https://www.phylopic.org/images/0b89df58-7eae-40c5-9676-d35e3449afb2/clupea-pallasii

Zooplankton:
https://www.phylopic.org/images/c1ee28a8-284f-4c4b-b8e0-82f981cac04a/acartia-tonsa

If they were to be ordered top to bottom, then the above order would be good (as it's kind of how they would be in a food web), but spread out is fine also.

# Carley's final awesome version is below is in inst/ (also a .jpg version for
# printing), and this makes (copies?) one in man/figures/logo.png and gives code
# to add to README.Rmd:

ifelse(lubridate::today() %>% lubridate::month() == 12,
       logo_file <- paste0(here::here(), "/inst/pacea-logo-from-carley-3.png"),
       logo_file <- paste0(here::here(), "/inst/pacea-logo-from-carley.png"))

use_logo(logo_file)
