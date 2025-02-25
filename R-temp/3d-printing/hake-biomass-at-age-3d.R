# Create 3d printing file (stl) of n-at-age.

# This is based on template, saved as example-code.R, from
# https://github.com/paulnorthrop/r2stl/blob/main/demo/r2stl.R

# Run this after building doc so have the base model loaded in.

# OR move into utils-pacea-save.R which is probably better, and save n-at-age in pacea.
library(r2stl)

hake_total_biomass_at_age

h <- hake_total_biomass_at_age  # rows are years, columns are ages

x <- h$year                   # years
y <- as.numeric(names(h)[-1]) # ages

# z: A numeric 'length(x)' by 'length(y)' matrix with the
#      z-coordinates to plot.

z <- dplyr::select(h, -"year") %>%
  as.matrix() %>%
  unname()


length(x)
length(y)
dim(z)

r2stl(x,
      y,
      z,
      filename="hake-total-biomass-at-age-2025-default.stl",
      show.persp=TRUE)

# Creates a pointy surface not bars. So need to manually add in intermediate
# values to fudge it to make bars.

# so want age-0 to take values 0.0, 0.1, 0.2, 0.3, ..., 0.9, then age-1 to be
# 1.0, 1.1, 1.2, ... 1.9. Similar for years.

# Temporary to get code working
# x <- x[1:5]
# y <- y[1:3]
# z <- z[1:5, 1:3]

inc <- 0.2    # increment width to use
num_incs <- length(seq(0,
                       1 - inc,
                       by = inc)) # number of increments, essentially 1/inc

# Assume years and ages are both consecutive, just check:
expect_equal(max(diff(x)), 1)
expect_equal(max(diff(y)), 1)
# so just recreate with the required increment

year_inc <- seq(min(x),
                max(x) + 1 - inc,
                by = inc)

age_inc <- seq(min(y),
               max(y) + 1 - inc,
               by = inc)

# Easier to understand by doing a loop:
z_inc <- matrix(data = NA,
                nrow = length(year_inc),
                ncol = length(age_inc))

for(year_index in 1:length(x)){       # true years
  for(year_repeat in 0:(num_incs - 1)){  # to repeat rows for a single year
    z_inc[(year_index - 1) * num_incs + 1 + year_repeat, ] <- rep(z[year_index, ],
                                                                  each = num_incs)
  }
}

r2stl(x = year_inc,
      y = age_inc,
      z = z_inc,
      filename="hake-total-biomass-at-age-2025.stl",
      show.persp=TRUE)




stop()
# Let's do the classic persp() demo plot

x <- seq(-10, 10, length= 100)

y <- x

f <- function(x,y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }

z <- outer(x, y, f)

z[is.na(z)] <- 1

r2stl(x, y, z, filename="lovelyfunction.stl", show.persp=TRUE)
