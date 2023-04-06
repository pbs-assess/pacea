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
