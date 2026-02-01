# Create 3d printing file (stl) of n-at-age.

# This is based on template, saved as example-code.R, from
# https://github.com/paulnorthrop/r2stl/blob/main/demo/r2stl.R

# After building hake doc update assess_yr in hake dir:
#  hake::pacea_save() - will create .rds and place them in data-raw/....
# Update assess_yr here

# Biomass-at-age not available for 2026 assessment, because not saved when doing
# projections in Stock Synthesis.

library(r2stl)

assess_yr <- 2026

hake_dir <- paste0(here::here(),
                   "/data-raw/groundfish/hake-",
                   assess_yr,
                   "/")

# Get assess_yr value from pacea, else from data-raw (since still draft document
# so not updating in pacea yet)
if(exists(paste0("hake_total_biomass_at_age_",
                 assess_yr))){
  assign(h,                         # rows are years, columns are ages
         paste0("hake_total_biomass_at_age_",
                assess_yr))
  } else {
  h <- readRDS(paste0(hake_dir,
                      "hake_total_biomass_at_age_",
                      assess_yr,
                      ".rds"))
}

h

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

# r2stl(x,
#       y,
#       z,
#       filename = paste0("hake-total-biomass-at-age-",
#                         assess_yr,
#                         "-default.stl"),
#       show.persp = TRUE)

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
      filename = paste0("hake-total-biomass-at-age-",
                        assess_yr,
                        ".stl"),
      show.persp=TRUE)


