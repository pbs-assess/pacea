# Quickly tried this but didn't get very far. Waiting until we get some nice
#  figures going.
# Thing this is adapted from the hexSticker GitHub site.
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


# Using this to send to Carley

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


That website does not have Sasquatch. I searched Sasquatch silhouttes and this one looks good (the turning of the head is nice), but there are many many others (not all free though, unlike the real animals!):
 https://www.pinterest.ca/pin/536280268131153798/
If you want to sneak one into northern Van Island that'd be fun, but no big deal.
