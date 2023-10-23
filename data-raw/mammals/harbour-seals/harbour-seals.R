# Harbour Seal estimates. Run code line-by-line and check plots.
#  See ?harbour-seals for details.

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
harbour_seals_data_raw <- readr::read_csv("N_region_data_request_June_2023.csv")

harbour_seals_data_raw

summary(harbour_seals_data_raw)

# This is the GAM results for each region
harbour_seals_gam_raw <- readr::read_csv("pred_all_regions.csv")

harbour_seals_gam_raw

summary(harbour_seals_gam_raw)

# Can see multiple values for some year, because the GAM has a temporal time
# step of 0.6835 years:
unique(diff(harbour_seals_gam_raw$x))
# [1]   0.683544   0.683545   0.683545   0.683544 -54.000000
# -54 is the jump back to the start for the next Region
# So need to create a Date that is based on these

harbour_seals_gam_new <- harbour_seals_gam_raw %>%
  mutate(Region = factor(Region,
                         levels = unique(Region)), # retain order
         year = floor(x),
         year_fraction = x %% 1,   # how far through the year
         leap_year = leap_year(year),  # TRUE/FALSE
         julian_day_minus_1 = ifelse(leap_year,
                                     year_fraction * 366,
                                     year_fraction * 365),
         date = as.Date(make_date(year) +
                        julian_day_minus_1)) %>%
  relocate(date,
           region = Region,
           low = ymin,
           mean = y,
           high = ymax)             # But then only keeping some columns when saving

harbour_seals_gam_new[1:30, ] %>% as.data.frame()

harbour_seals_data <- harbour_seals_data_raw %>%
  rename(region = RegionID,
         low = lowL,
         mean = Regional_N,
         high = uppL) %>%
   mutate(region = factor(region,
                          levels = unique(region)))  # retain order

p <- ggplot(data = harbour_seals_gam_new,
            aes(x = date,
                y = mean)) +
  geom_ribbon(aes(ymin = low,
                  ymax = high),
              fill = "grey") +
  geom_line(col = "blue") +
  geom_point(data = harbour_seals_data,
             aes(x = make_date(year),    # does 1st Jan so careful TODO
                 y = mean)) +
  facet_wrap(~region,
             scales = "free_y",
             ncol = 2)
p

#Looks like matches SAR figure. Could add data, but refer people to
# original SAR in the help file.


class(harbour_seals_gam_new) <- c("pacea_harbour_seals",
                              class(harbour_seals_gam_new))
attr(harbour_seals_gam_new, "axis_name") <-
  "Estimated abundance (number of seals)"

# Get rid of columns that don't need to be saved (either original ones or
# intermediates), and just save as harbour_seals. Can add data if people request
# it.

harbour_seals <- harbour_seals_gam_new %>%
  select(date,
         region,
         low,
         mean,
         high)                  # not keeping year as duplicated for some years

# Calculate coastwide numbers and uncertainty

coastwide <- dplyr::summarise(group_by(harbour_seals, date),
                              low = sum(low),          # TODO this and high are
                                        # wrong, emailed Kurtis
                              mean = sum(mean),
                              high = sum(high)) %>%
  dplyr::mutate(region = "Coastwide") %>%
  dplyr::relocate(date, region)

stopifnot(names(harbour_seals) == names(coastwide))

harbour_seals <- rbind(harbour_seals,
                       coastwide)

# Also need the final date of the data, to then colour code the
# figures.

harbour_seals_data_final_year <- harbour_seals_data %>%
   group_by(region) %>%
   summarise(final_year = max(year))

usethis::use_data(harbour_seals,
                  overwrite = TRUE)

usethis::use_data(harbour_seals_data_final_year,
                  overwrite = TRUE)
