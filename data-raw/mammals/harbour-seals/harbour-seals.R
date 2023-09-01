# Harbour Seal estimates. Run code line-by-line and check plots.
#  See ?harbour-seals for details.

load_all()
library(dplyr)
library(ggplot2)
library(lubridate)

# From Strahan Tucker, 31st August 2023.

# Please find attached the harbour seal data to which GAMs were fit as per Fig 3
# in SAR. This is rolled up per region-we provide the count, the abundance
# estimate and the 95% CI, as well as proportion of area surveyed. Have also
# provided the predicted values (total abundance) from the fits by year by region
# (one would just sum across regions for the total)

# The sea lion data is different and will be passing that on when available

# This is the raw data
harbour_seals_raw <- readr::read_csv("N_region_data_request_June_2023.csv")

# This is the GAM results for each region
harbour_seals_gam_raw <- readr::read_csv("pred_all_regions.csv")

harbour_seals_gam_raw

# Can see multiple values for some year, because the GAM has a temporal time
# step of 0.6835 years:
unique(diff(harbour_seals_gam_raw$x))
# [1]   0.683544   0.683545   0.683545   0.683544 -54.000000
# So need to create a Date that is based on these

harbour_seals_gam <- mutate(harbour_seals_gam_raw,
                            Region = factor(Region,
                                            levels = unique(Region)), # retain order
                            year = floor(x),
                            year_fraction = x %% 1,   # how far through the year
                            leap_year = leap_year(year),  # TRUE/FALSE
                            julian_day_minus_1 = ifelse(leap_year,
                                                              year_fraction * 366,
                                                              year_fraction * 365),
                            date = as.Date(julian_day_minus_1,
                                           origin = as.Date(paste0(year,
                                                                   "-01-01"))),
                            date_2 = as.Date(make_date(year) +
                              julian_day_minus_1))  # alternative way

expect_equal(harbour_seals_gam$date,
             harbour_seals_gam$date_2)

harbour_seals_gam[1:30, ] %>% as.data.frame()

p <- ggplot(data = harbour_seals_gam,
            aes(x = date,
                y = y)) +
  geom_point()
p + facet_wrap(~Region)

# Matches SAR figure, TODO confidence intervals, coastwide, then save in pacea format.
HERE

hake, can delete, though need a few things maybe

# Recruitment

# Comment this out if you have copied the .rda files to the local directory
#  (you need to first run the code in the hake directory shown below), and
#  uncomment the second line
hake_dir <- paste0(here::here(), "/../hake-assessment/sandbox/andy/pacea-save/")
# hake_dir <- getwd()

load(paste0(hake_dir, "hake_recruitment_new.rda"))

class(hake_recruitment_new) <- c("pacea_recruitment",
                                 class(hake_recruitment_new))

attr(hake_recruitment_new, "axis_name") <-
  "Pacific Hake recruitment (billions of age-0 fish)"

if(check_index_changed(hake_recruitment,
                       hake_recruitment_new)){
  hake_recruitment <- hake_recruitment_new
  usethis::use_data(hake_recruitment,
                    overwrite = TRUE)
  plot(hake_recruitment)
}

# Spawning biomass

load(paste0(hake_dir, "hake_biomass_new.rda"))

class(hake_biomass_new) <- c("pacea_biomass",
                             class(hake_biomass_new))

attr(hake_biomass_new, "axis_name") <-
  "Pacific Hake spawning biomass (million t)"

if(check_index_changed(hake_biomass,
                       hake_biomass_new)){
  hake_biomass <- hake_biomass_new
  usethis::use_data(hake_biomass,
                    overwrite = TRUE)
  plot(hake_biomass, value = "val", style = "plain")
}

# Recruitment scaled by 2010 value

load(paste0(hake_dir, "hake_recruitment_over_2010_new.rda"))

class(hake_recruitment_over_2010_new) <- c("pacea_recruitment",
                                           class(hake_recruitment_over_2010_new))

attr(hake_recruitment_over_2010_new, "axis_name") <-
  "Age-0 Pacific Hake recruitment relative to that in 2010"

if(check_index_changed(hake_recruitment_over_2010,
                       hake_recruitment_over_2010_new)){
  hake_recruitment_over_2010 <- hake_recruitment_over_2010_new
  usethis::use_data(hake_recruitment_over_2010,
                    overwrite = TRUE)
  plot(hake_recruitment_over_2010)
  }
}

# Recruitment scaled by R0:

load(paste0(hake_dir, "hake_recruitment_over_R0_new.rda"))

class(hake_recruitment_over_R0_new) <- c("pacea_recruitment",
                                           class(hake_recruitment_over_R0_new))

attr(hake_recruitment_over_R0_new, "axis_name") <-
  "Age-0 Pacific Hake recruitment relative to unfished equilibrium recruitment"

if(check_index_changed(hake_recruitment_over_R0,
                       hake_recruitment_over_R0_new)){
  hake_recruitment_over_R0 <- hake_recruitment_over_R0_new
  usethis::use_data(hake_recruitment_over_R0,
                    overwrite = TRUE)
  plot(hake_recruitment_over_R0)
  }
}
