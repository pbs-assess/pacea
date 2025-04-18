# Originally from from-others/roms_ncdf4.Rmf which was based on Linsday's
#  RomsExtraction_forAndy.Rmd, but now just doing .R not .Rmd.

library(ncdf4) # package for netcdf manipulation
library(dplyr)
library(sf)
library(terra)
library(lubridate)
load_all()     # pacea


# Opening example temperature results object from Angelica and print metadata to
#  see the data names and structure.
# Four years worth of monthly sst results on a spatial grid.
temp_nc <- nc_open("Roms_bcc42_mon_2008to2011_sst.nc")

# Don't think it loads in the full file, but opens it for extracting, and gets
#  the metadata and dimensions. Maybe.

temp_nc

# From Angelica:
# ocean_time - monthly value from 2008 to 2011 (4x12=48);
# lon_rho - longitude at the center of each cell
# lat_rho - latitude at the center of each cell
# mask_rho - land/ocean mask (1=ocean, 0=land)
# sst - sea surface temperature at each grid cell every month from 2008 to 2011

summary(temp_nc)
temp_nc$var   # details of the four variables excluding dimensions: lon_rho, lat_rho,
              #  mask_rho, sst

# Data exploration and looking at the metadata (which is largely non existent).

time <- ncvar_get(temp_nc, "ocean_time") %>% as.vector()
time    # vector of seconds from midnight 1st Jan 1970, with 48 values.
# tunits <- ncatt_get(temp_nc,
#                     "ocean_time",
#                     "units")  # FALSE, since I think not saved correctly.

as_datetime(time)      # These are all midnight, so can stick with just dates
date <- as_datetime(time) %>% as_date()

date

day(date) %>% range()  # Will ask Angelica why these aren't always in the middle
                       # of the month. Maybe the resolution of the model is not
                       # daily - if it's 3-day then that explains it.

# lat and lon
# [xi_rho and eta_rho are the physical model co-ordinates, don't think we need
#  to use those]. Angelica: The model domain has 236 grid cells in the x-axis and 410 in the
#  y-axis (1,1 is the left bottom corner of the model and 236,410 the upper right
#  corner of the domain).

# Doesn't work:
#xi_rho <- ncvar_get(temp_nc,
#                    "xi_rho")
# [1] "vobjtovarid4: **** WARNING **** I was asked to get a varid for dimension named eta_rho BUT this dimension HAS NO DIMVAR! Code will probably fail at this point"
# Error in nc$dim[[idobj$list_index]] :
#   invalid negative subscript in get1index <real>

# Doesn't work:
# eta_rho <- ncvar_get(temp_nc,
#                    "eta_rho")

lon <- ncvar_get(temp_nc,
                 "lon_rho")
lat <- ncvar_get(temp_nc,
                 "lat_rho")
dim(lon)       # lon is the longitude of the center of each cell
dim(lat)       # lat is the longitude of the center of each cell

lon[1:5, 1:5]
lat[1:5, 1:5]
lon[1, 1]      # Angelica: bottom left. Aha, grid is rotated, so furthest south
               #  but not furthest west
lon[1, 410]    # this seems to be furthest west
lon[236, 1]    # this seems to be furthest east
lon[236, 410]  # Angelica: upper right. Aha, grid is rotated, so furthest north
               #  but not furthest east.

# Plot values, but doesn't help too much.
# plot(lon[1, ], ylim = range(lon))
# points(lon[, 1], col = "red")
# points(lon[236, ], col = "blue")
# points(lon[, 410], col = "green")

# image(lon)   # clearly shows not a north-south east-west grid, as lon changes

plot(lon, lat, pch = 20, cex = 0.1, xlim = c(-130, -125), ylim = c(43.5, 48))

plot(lon, lat, pch = 20, cex = 0.1, xlim = c(-129, -127), ylim = c(43.5, 45)) # to print

# lat2 <- rev(lat[1,]) # Lindsay had

plot(Coastline)     # originally saved one, as an sf object, will likely change
points(lon, lat, pch = 20, cex = 0.1)    # hoping could just overlay, but didn't
                                        # work I think because likely need an sf object.


# sst
sst <- ncvar_get(temp_nc,
                 "sst")
# sst[which(is.nan(sst))] <- NA        # uncomment this when it all works later,
# doing it for example one getting working

dim(sst)             # 236 x 410 x 48 time steps
class(sst)
attributes(sst)
plot(sst[1, 1, 1:48],
     type = "o")    # One location, sst at each time point

dim(sst[1:236, 1:410, 1]) # First time point

sst_time_1 <- sst[, , 1]  # sst at first time point; 236 x 410, same as lat and lon

# Want to use st_as_sf() to convert to an sf object to then use
# stars::st_rasterize() (first attempt) or terra::project()  (from sf course)
# https://tmieno2.github.io/R-as-GIS-for-Economists/turning-a-data-frame-of-points-into-an-sf.html


# Instead of what I did below, trying this from ?read_ncdf
# sst_stars <- stars::read_ncdf("Roms_bcc42_mon_2008to2011_sst.nc",
#                            curvlinear = c("lon_rho", "lat_rho"),
#                            var = "sst")
# plot(sst_stars)
# While plotting what might look okay (48 panels of sst with nothing on land),
# the axes are xi_rho and eta_rho not lon and lat. Hard to extract those
# automatically from the netCDF file, so stick with doing it manually.


# STEPS - skip to later
# Need dataframe of sst, lon, lat in long format to then use sf::st_as_sf and
# then st_rasterize or other functions as tried later. Want to have values only
# north of, say 47 degrees.

# This is for just 10 values,
lon_first <- lon[1:10, 1]
lat_first <- lat[1:10, 1]
sst_first <- sst_time_1[1:10, 1]

# Make into long format
lon_lat_sst_array = tibble(lon = lon_first, lat = lat_first, sst = sst_first)

# lon_lat_sst_array <- tibble(lon, lat, sst) - needs tweaking since each one is not now a vector

sst_time_1_sf <- sf::st_as_sf(lon_lat_sst_array,
                              coords = c("lon","lat"))

sst_time_1_sf <- sf::st_set_crs(sst_time_1_sf,
                                value =
                                "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")  # NOT automatic from netCDF, is what Lindsay used

plot(sst_time_1_sf,
     axes = TRUE)
# Below, Lindsay had
# crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
# so I've used that
# **See if she confirms, check with Kelsey also, and may have to ask
# Angelica. Chris has that also in his code, and thinks he got from Angelica.

# Seem to want to use stars::st_rasterize for the conversion to the master grid:
#  xx <- stars::st_rasterize(sst_time_1_sf,
#                            test_grid_20)
# but need test_grid_20 as a stars object.

#xx <- stars::st_rasterize(sst_time_1_sf,
#                          stars::st_as_stars(test_grid_20))
# correctly doesn't work as first is in lat-lon and grid is in km. Kelsey said km
#  is probably better.

# Try to convert sst values into km as for grid

# ATTEMPT 1
test_grid_20
plot(test_grid_20,
     axes = TRUE)

sst_time_1_sf

sst_time_1_sf_km <- sf::st_transform(sst_time_1_sf,
                                     sf::st_crs(test_grid_20))

expect_equal(sf::st_crs(sst_time_1_sf_km), sf::st_crs(test_grid_20))
                                        # No error is good!

sst_time_1_sf_km
plot(sst_time_1_sf_km,
     axes = TRUE)    # This is giving negative northings

xx <- stars::st_rasterize(sst_time_1_sf_km,
                          stars::st_as_stars(test_grid_20))
                          #  align = TRUE) option gives:
                          # Error in stars::st_rasterize(sst_time_1_sf, stars::st_as_stars(test_grid_20_lon_lat),  :
                          #   is_regular_grid(template) is not TRUE

stars:::is_regular_grid(sst_time_1_sf_km)    # FALSE  (::: since function not exported)
stars:::is_regular_grid(test_grid_20)    # FALSE
# still didn't work, not quite sure why.
# ---



#could try fasterize, though may need to create the grid as a raster
#object. That's fine, can still save it as just sf.
# stars::aggregate maybe


# Repeating above ideas but for not just a vector of lon and of lat and of sst,
# but the full matrix of values (for just the first time step).

# STEPS
# Need dataframe of sst, lon, lat in long format to then use sf::st_as_sf and
# then st_rasterize or other functions as tried later. Want to have values only
# north of, say 47 degrees.

# This is for just 10 values,
# lon_first <- lon[1:10, 1]
# lat_first <- lat[1:10, 1]
# sst_first <- sst_time_1[1:10, 1]

# Make into long format

# ATTEMPT 2
lon_vec <- as.vector(lon)       # Constructs vector columnwise from matrix
lat_vec <- as.vector(lat)
sst_time_1_vec <- as.vector(sst_time_1)
sst_time_1_vec[which(is.nan(sst_time_1_vec))] <- NA     # Do this straight away
                                        # on sst earlier, doing it here while developing.

lon_lat_sst_tibble = tibble(lon = lon_vec,
                            lat = lat_vec,
                            sst = sst_time_1_vec)   # 96,760 x 3 = 236 * 410 * 3

sst_time_1_sf <- sf::st_as_sf(lon_lat_sst_tibble,
                              coords = c("lon","lat"))

sst_time_1_sf <- sf::st_set_crs(sst_time_1_sf,
                                value =
                                "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")  # NOT automatic from netCDF, is what Lindsay used

# Just see how big the objects are, note these includes lots of NA's and aren't
#  yet on the master grid

usethis::use_data(sst_time_1_sf)   # 1.2 Mb as an sf object
usethis::use_data(sst_time_1)      # 244 kb (one month)
usethis::use_data(sst)             # 11.5 Mb (but 48 months worth, only slightly more
                                   # efficient than saving 48 one-month ones =
                                   # 48 * 244 = 11.7 Mb)


# Below, Lindsay had
# crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
# so I've used that
# **See if she confirms, check with Kelsey also, and may have to ask
# Angelica. Chris has that also in his code, and thinks he got from Angelica.


plot(sst_time_1_sf, axes = TRUE,
     pch = 20,
     cex = 0.1)       # A point for each value, kind of shows the resolution
                      # (cex smaller makes no difference). Land just comes from
                      # NA's presumably.

plot(sst_time_1_sf, axes = TRUE,
     pch = 20,
     cex = 0.1,
     xlim = c(-140, -118),
     ylim = c(47.5, 55))    # think it kind of ignores ylim to get xlim okay

# Now do a restricted area, make quite small for testing the master grid stuff

lon_lat_sst_tibble_restrict <- filter(lon_lat_sst_tibble,
                                      lon > -125,
                                      lat > 48,
                                      lat < 50)

sst_time_1_sf_restrict <- sf::st_as_sf(lon_lat_sst_tibble_restrict,
                                       coords = c("lon","lat"))

sst_time_1_sf_restrict <- sf::st_set_crs(sst_time_1_sf_restrict,
                                         value =
                                         "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")  # NOT automatic from netCDF, is what Lindsay used
# Includes NA's for land

plot(sst_time_1_sf_restrict,
     axes = TRUE,
     pch = 20,
     cex = 0.1)    # southeast Vancouver Island

# Try to convert sst values into km as for grid

test_grid_20
plot(test_grid_20,
     axes = TRUE)

sst_time_1_sf_restrict

sst_time_1_sf_km_restrict <- sf::st_transform(sst_time_1_sf_restrict,
                                              sf::st_crs(test_grid_20))

expect_equal(sf::st_crs(sst_time_1_sf_km_restrict), sf::st_crs(test_grid_20))

sst_time_1_sf_km_restrict
plot(sst_time_1_sf_km_restrict,
     axes = TRUE,
     pch = 20,
     cex = 0.1) # Looks like matches above one

# Need both objects as terra spatRaster for terra::resample, although we (for
# now) don't want the layers that spatRaster creates (but maybe will do it that
# way for the time aspect?). Help for terra::rast doesn't seem to mention sf
# objects though.

sst_time_1_sf_km_restrict_rast <- terra::rast(sst_time_1_sf_km_restrict)
# That has no cell values:
plot(sst_time_1_sf_km_restrict_rast)

test_grid_20_rast <- terra::rast(test_grid_20)
# HERE
# Also has no cell values. Ask Kelsey. Maybe best to just not restrict the data
# so we get something (i.e. only looking at Juan de Fuca right now, which for a
# 20x20 grid will have ROMS NA's).


expect_equal(sf::st_crs(sst_time_1_sf_km_restrict_rast), sf::st_crs(test_grid_20_rast))

sst_time_1_sf_km_restrict_grid_20 <- terra::resample(sst_time_1_sf_km_restrict_rast,
                                                     test_grid_20_rast)

# That didn't give an error (same when trying terra::project(), which shouldn't
# need as have set both crs to be the same), but:

plot(sst_time_1_sf_km_restrict_grid_20)
# Plots blank screen (with co-ordinates) and warning:
# [plot] SpatRaster has no cell values

HERE


# test_grid_20_stars <- stars::st_as_stars(test_grid_20)

stop("Got to here")

# HERE - this still fails. Need to learn more about the packages.

xx <- stars::st_rasterize(sst_time_1_sf_km_restrict,   #["sst"],
                          template = test_grid_20_stars)
                          # Error in !all.equal(match(xydims, names(d)), 1:2) : invalid argument type
                          #  Think it's the ! giving the error, as:
                          #   > names(test_grid_20_stars)
                          # [1] "id"
                          # > names(sst_time_1_sf_km_restrict["sst"])
                          # [1] "sst"      "geometry"
                          #  align = TRUE) option gives:
                          # Error in stars::st_rasterize(sst_time_1_sf, stars::st_as_stars(test_grid_20_lon_lat),  :
                          #   is_regular_grid(template) is not TRUE

stars:::is_regular_grid(sst_time_1_sf_km_restrict)    # FALSE  (function not exported)
stars:::is_regular_grid(test_grid_20)    # FALSE
# still didn't work, not quite sure why.
# ---

# END of what was copied from above

# Andy got no further

This is what Joe has in ERDDAP_DATA.Rmd, but uses raster package which is
getting deprecated

  # project onto correct CRS using bilinear interpolation
  raster::crs(dataset_list) <-
    raster::crs('EPSG:4326')   # standard for ERDDAP

  dataset_list <-
    raster::projectRaster(
      dataset_list,
      crs = PACea::Coastline@proj4string
    )


# Rest of this is from Lindsay (and based on her ROMS files); keep for now in
#  case anything useful.

#-- Lindsay has:
#Project mean temp/oxy values onto a 3km raster (I have not interpolated missing values)
xx <- rasterize(x=vect(spdf), y=r, field=names(spdf)[1], fun=mean, na.rm=F)
plot(xx)
#
for (i in (2:dim(df)[2])){
rasterize <- rasterize(x=vect(spdf), y=r, field=names(spdf)[i], fun=mean, na.rm=F)
rasterizeresamp <- resample(rasterize, r, method="bilinear")
xx <- c(xx,rasterizeresamp)
}
# --

Really just want to do the sf stuff mapping to the master grid,
then save resulting values in each cell as a data.frame, with columns being
year, month, and master cells

x <- data.frame(lon[1,1:410 ])

nlon <- dim(lon)
nlat <- dim(lat)

print(c(nlon,nlat))
# print(c(lon,lat)) - don't do, prints everything.

#get attributes for bottom temp
#dlname <- ncatt_get(ncin,dname,"long_name")
#dunits <- ncatt_get(ncin,dname,"units")
#fillvalue <- ncatt_get(ncin,dname,"_FillValue")
#dim(tmp_array_bottom)

ncin <- temp_nc
# get global attributes, think these will all be empty.
# title <- ncatt_get(ncin,0,"title")
#institution <- ncatt_get(ncin,0,"institution")
#datasource <- ncatt_get(ncin,0,"source")
#references <- ncatt_get(ncin,0,"references")
#history <- ncatt_get(ncin,0,"history")
#Conventions <- ncatt_get(ncin,0,"Conventions")

```


Extract bottom and average water column temp and oxygen.
```{r get_data}

# open a netCDF file
avg <- nc_open("data-raw/ROMS_original/bcc42_era5b37r1_mon1995to2018_zAvg.nc")
print(avg) #average across the water column
bottom <- nc_open("data-raw/ROMS_original/bcc42_era5b37r1_mon1995to2018_bottom.nc")
print(bottom)

attributes(avg$dim)$names
attributes(avg$var)$names
attributes(avg$dim)$names[1]
attributes(avg$dim)$names[3]


#get temp and attributes
tmp_array_bottom <- ncvar_get(bottom,"temp")
dim(tmp_array_bottom) #first number rows, second columns, last number is the number of years * 12 months
oxy_array_bottom <- ncvar_get(bottom,"Oxygen")
tmp_array_col <- ncvar_get(avg,"temp")
oxy_array_col <- ncvar_get(avg,"Oxygen")

tmp_array_bottom[1:236, 1:410, 1] #one time slice with all of the data
tmp_array_bottom[1, 1, 1:288] #one grid cell with dat from all time slices

# get longitude and latitude
lon <- ncvar_get(bottom,"lon_rho") #the the first one as that corresponds to
dim(lon)

lat <- ncvar_get(bottom,"lat_rho")
lat2 <- rev(lat[1,])
nlat <- length(lat)



x <- data.frame(lon[1,1:410 ])

nlon <- dim(lon)



print(c(nlon,nlat))
print(c(lon,lat))

# get longitude and latitude of the average dataset - this is the same OR SHOUD be the same as above
avg_lon <- ncvar_get(avg,"lon_rho") #the the first one as that corresponds to
avg_lon <- avg_lon[,1]
navglon <- length(avg_lon)
avg_lat <- ncvar_get(avg,"lat_rho")
avg_lat <- avg_lat[1,]
navglat <- length(avg_lat)

print(c(nlon,nlat))


```




Functions for extraction monthly mean and max temperatures and oxygen from bottom and water column netcdfs.
```{r extract_monthly_averages}

#MEAN
mean_each_year <- function(array, x) {
m <- 1:12 #months, could change to summer months
m <- m*x
tmp_slice <- array[,,m] #one year
test4 <- brick(tmp_slice, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
mean <- mean(test4) #change this for max or mean...
year = 1994 #actual start date of data is 1995, to account for the 1 in the model below started one year earlier
r_brick <- flip(t(mean), direction='y') #this flips the coordinates
names(r_brick) <- c(paste0("meantemp", year+x))
return(r_brick)
}


#MAX
maxO2_each_year <- function(array, x) {
m <- 1:12 #could change the months here to just summer months, so months 5:8,
m <- m*x # x is number of years
tmp_slice <- array[,,m] #one year
test4 <- brick(tmp_slice, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
max <- max(test4) #change this for max or mean...
year = 1994 #change this value depending on the data
r_brick <- flip(t(max), direction='y') #this flips the coordinates
names(r_brick) <- c(paste0("maxO2", year+x))
return(r_brick)
}


#Min
minO2_each_year <- function(array, x) {
m <- 1:12 #could change the months here to just summer months, so mohths 5:8,
m <- m*x # x is number of years
tmp_slice <- array[,,m] #one year
test4 <- brick(tmp_slice, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
min <- min(test4) #change this for max or mean...
year = 1994 #change this value depending on the data
r_brick <- flip(t(min), direction='y') #this flips the coordinates
names(r_brick) <- c(paste0("minO2", year+x))
return(r_brick)
}


```



Write Rasterstacks **for water column** (mean and max) temp and oxygen. Create a raster brick for each of the years. Take the mean temperature for each year and export as a raster brick. Use this output to intersect with the survey points to get average temperature per grid cell per year.
```{r extract_columns}

#mean temp water column


temp_col <- raster()
for (x in seq_along(0:23)){ #24 years
  df <- mean_each_year(tmp_array_col, x)
  temp_col <- stack(df, temp_col)
}
names(temp_col)
plot(temp_col)
writeRaster(temp_col, file="output/temp_col_mean", overwrite = TRUE)


#mean oxygen water column
oxy_col <- raster()
for (x in seq_along(1:24)){ #24 years
  df <- mean_each_year(oxy_array_col, x)
  oxy_col <- stack(df, oxy_col)
}
names(oxy_col)
plot(oxy_col)
writeRaster(oxy_col, file="output/oxy_col_mean", overwrite = TRUE)


#max temp water column
temp_col_max <- raster()
for (x in seq_along(1:24)){ #24 years
  df <- max_each_year(tmp_array_col, x)
  temp_col_max <- stack(df, temp_col_max)
}
names(temp_col_max)
plot(temp_col_max)
writeRaster(temp_col_max, file="output/temp_col_max", overwrite = TRUE)
```
