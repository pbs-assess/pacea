# Loading and analysing EKE values from Wei Cheng. Adapting from
# https://help.marine.copernicus.eu/en/articles/5040824-how-to-open-and-visualize-copernicus-marine-data-in-r
# by Anais P.

# From Wei:
# I computed EKE from satellite altimetry data (available from the Copernicus
# marine data server). It can also be done using ROMS modeled sea surface
# height, see this paper (Coyle et al., 2012, its Fig. 25) I provide EKE to AFSC's annual ESR
# and ESP process. Would it be helpful if I send you daily EKE from 1993/01/01
# to near real time on a regular 0.125 deg lon x 0.125 deg lat grid? You can
# then average it spatially to any region you like (I can help with that as well
# if needed).
#
# Andy: If you have the daily values already then just sending them to me sounds
# easiest, to save me having to get them from Copernicus (I haven’t done that
# before, and any time savings are greatly appreciated). That would be awesome,
# I should be able to do the averaging (although defining the range of 0-30km
# from the coastline might be fiddly, haven’t done that before).
#
# Wei: Daily EKE based on satellite altimetry for your region is attached. Below
# is a picture of the long-term mean of the EKE (averaged
# 1993/01/01-2026/01/06) [see email]
#
# From the coast to 30-km offshore is slightly tricky to define, but I have done
# it for the Gulf of Alaska. I won't be able to get to it (for your region)
# today, but am happy to help if needed.

# So, am checking can load in the file okay and look at it. Hopefully the
# metadata is populated.



# Loading of the different libraries
library(ncdf4)
library(lubridate)
library(RColorBrewer)
library(lattice)

## Set the working directory and filename
nc_file <- nc_open("eke_cmems_0.125_19930101-20260106.nc")
print(nc_file)


## Structure of the .nc file
names(nc_file)

# Names of the variables
names(nc_file$var)
# Names of the dimensions
names(nc_file$dim)

## Get Coordinate variables
longitude <- nc_file$dim[[1]]$vals  # Example had 4, but didn't show the order
latitude <- nc_file$dim[[2]]$vals
time <- nc_file$dim[[3]]$vals

# Number of variable's values
nlon <- dim(longitude)
nlat <- dim(latitude)

print(c(nlon,nlat)) #to have them all

## Get time variable
time[1:10]
# Number of time steps
nt <- dim(time)
nt
# Time units attribute
t_units <- ncatt_get(nc_file, "TTAXIS", "units")
t_units

# That is days since 1950-01-01, lubridate starts at 1970, so take off twenty years
date <- lubridate::as_date(time) - lubridate::years(20)
range(date)
# Matches what Wei asked for

plot(date)   # So has every day:
max(diff(date))

months <- lubridate::month(date)

summary(as.factor(months))  # makes sense, with extra values for Jan 2026.

date_may_to_sep_ind <- months %in% 5:9
summary(as.factor(months[date_may_to_sep_ind]))

## Get an ocean variable
names(nc_file$var)

#sea_water_potential_temperature
eke_array_full <- ncvar_get(nc_file,nc_file$var[[1]])
dim(eke_array_full)
# eke <- "thetao"

eke_array <- eke_array_full[ , , date_may_to_sep_ind]

#variable's attributes
ncatt_get(nc_file, "EKE", "long_name")   #long name
ncatt_get(nc_file, "EKE", "units")       #measure unit
fillvalue <- ncatt_get(nc_file, "EKE", "_FillValue")  #(optional)
fillvalue

## Quick Map Plot
# set the time step
one_day_index <- 1 # 1993-01-01
eke_slice <- eke_array[ , , one_day_index]

eke_mean_each_cell <- apply(eke_array, c(1, 2), mean, na.rm = TRUE)   # TODO look at NA's
# more carefully
summary(eke_mean_each_cell)

TODO here, figure out the years
eke_mean_each_year <- apply(eke_array, c(1, 2), mean, na.rm = TRUE)   # TODO look at


# Plot a map
image(longitude,latitude,eke_slice, col = rev(brewer.pal(10,"RdBu")))

image(longitude,latitude,eke_mean, col = rev(brewer.pal(10,"RdBu")))

# Try to match Wei's, though this is now only May-Sept.
image(longitude,latitude,eke_mean, col = rev(brewer.pal(9,"YlGnBu"))) # emailed her


# ggplot:
library(ggplot2)

# not quite:
ggplot(eke_mean, aes(x = longitude, y = latitude)) +
  geom_density_2d_filled()


# Better map from Copernicus code, didn't work here but didn't try hard
#create a set of lonxlat pairs of values, one for each element in the Temp_array
grid <- expand.grid(lon=longitude, lat=latitude)
# set colorbar
cutpts <- c(12,13,14,15,16,17,18,19,20)
# plot
levelplot(eke_slice ~ lon * lat,
          data=grid, region=TRUE,
          pretty=T, at=cutpts, cuts=9,
          col.regions=(rev(brewer.pal(9,"RdBu"))), contour=0,
          xlab = "Longitude", ylab = "Latitude",
          main = "EKE...")
)

