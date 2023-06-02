# ROMs data from Angelina Pena - full bottom and surface variables that Andy requested

library(devtools)
library(dplyr)
library(terra)
library(gstat)
library(sf)
library(stars)
library(ncdf4)
library(ggplot2)
library(concaveman)

sf_use_s2(FALSE)  # remove spherical geometry (s2) for sf operations

# load pacea
load_all()


#####
# START - load data to environment

# transform bc_coast
tbc <- bc_coast

# convert to multilinestring
tbc.line <- st_cast(tbc, "MULTILINESTRING")

#####
# create polygons for cropping to roms data
nc_dat <- nc_open(paste0("data-raw/roms/bcc42_era5glo12r4_mon1993to2019_botTSO.nc"))

# load lon-lat and mask layers from netcdf
nc_lon <- as.vector(ncvar_get(nc_dat, "lon_rho"))
nc_lat <- as.vector(ncvar_get(nc_dat, "lat_rho"))

nc_var <- ncvar_get(nc_dat, "temp")
nc_varmat <- apply(nc_var, 3, c)

# put sst into dataframe and sf object
dat <- data.frame(x = nc_lon, y = nc_lat) %>% cbind(nc_varmat) %>%
  st_as_sf(coords = c("x", "y"), crs = "EPSG:4326") %>%
  st_transform(crs = "EPSG: 3005")

# create polygon for cropping ROMS data
roms_cave <- dat %>% 
  na.omit() %>%
  concaveman::concaveman()
roms_buff <- dat %>% 
  na.omit() %>%
  st_geometry() %>% 
  st_buffer(dist = 5000) %>%
  st_union()

rm(nc_dat, nc_lon, nc_lat, nc_var, nc_varmat, dat)
#####


#####
# PARAMETERS

# loop variables
idepth <- c("bot", "sur")
jvars <- c("temp", "salt", "Oxygen")

#idepth <- c("sur")
#jvars <- c("salt", "Oxygen")

# index table
vars_fullname <- c("temperature", "salinity", "oxygen")
vars_units <- c("Temperature (\u00B0C)",
                "Salinity (ppt)",
                "Dissolved oxygen content (mmol-oxygen m^-3)")
jvars_table <- cbind(jvars, vars_fullname, vars_units)

# function argument
llnames <- c("x", "y")
nmax <- 4

# column names
cnames <- paste(rep(1993:2019, each=12), 1:12, sep="_")

# version of data update
version <- "01"  

# processing times output
proctimes <- vector()

# END parameters
#####


for(i in idepth) {
  nc_dat <- nc_open(paste0("data-raw/roms/bcc42_era5glo12r4_mon1993to2019_",i,"TSO.nc"))
  
  # load lon-lat and mask layers from netcdf
  nc_lon <- as.vector(ncvar_get(nc_dat, "lon_rho"))
  nc_lat <- as.vector(ncvar_get(nc_dat, "lat_rho"))

  for(j in jvars) {
    
    start <- Sys.time()
    
    nc_var <- ncvar_get(nc_dat, j)
    nc_varmat <- apply(nc_var, 3, c)
    
    # put sst into dataframe and sf object
    dat <- data.frame(x = nc_lon, y = nc_lat) %>% cbind(nc_varmat)
    dat_sf <- st_as_sf(dat, coords = c("x", "y"), crs = "EPSG:4326")
    tdat_sf <- st_transform(dat_sf, crs = "EPSG: 3005")
    
    # interpolate data
    # 2 km res
    output2 <- point2rast(data = tdat_sf, spatobj = inshore_poly, loc = llnames, cellsize = 2000, nnmax = nmax, as = "SpatRast")
    # 6 km res
    output6 <- point2rast(data = tdat_sf, spatobj = offshore_poly, loc = llnames, cellsize = 6000, nnmax = nmax, as = "SpatRast")
    
    # crop out grid cells with polygon masks
    t2_sf2 <- output2 %>% 
      mask(romseez_poly) %>% 
      mask(inshore_poly) %>%
      stars::st_as_stars() %>%  ## check here for converting to points (not raster)
      st_as_sf()
    t2_sf6 <- output6 %>%
      mask(romseez_poly) %>%
      mask(offshore_poly) %>%
      stars::st_as_stars() %>%
      st_as_sf()
    
    # mask 2k grid with 6k grid, then combine grids
    t2_sf2 <- t2_sf2[!st_intersects(st_union(t2_sf6), t2_sf2, sparse=FALSE, prepared=TRUE),] %>%
      rbind(t2_sf2[st_intersects(st_union(t2_sf6), t2_sf2, sparse=FALSE, prepared=TRUE),])
    t2_sf26 <- t2_sf2 %>% rbind(t2_sf6)
    
    
    ##### BC MASK OPTION 1 - Using bc shapefile
    # index points that dont intersect with bc coast shapefile
    #  disjoint - do not share space
    # dis2 <- t2_sf26[st_disjoint(st_union(tbc), t2_sf26, sparse=FALSE, prepared=TRUE),]
    # 
    # #  convert bc coast to sf linestring and finding coastline intserections separately - increased processing speed
    # #  using st_intersects is much faster than other predicate functionss
    # sub.t2 <- t2_sf26[st_intersects(st_union(tbc), t2_sf26, sparse=FALSE, prepared=TRUE),]
    # inter.line <- sub.t2[st_intersects(tbc.line, sub.t2, sparse=FALSE, prepared=TRUE),]
    # t2_sf26 <- rbind(dis2, inter.line)
    
    ##### BC MASK OPTION 2 - Using roms outline 
    # use roms_cave
    t2_sf26 <- t2_sf26[roms_cave,]
    
    # use roms_buff to get haida gwaii outline
    t2_sf26 <- t2_sf26[roms_buff,]
    
    
    
    # assign column names as year_month
    names(t2_sf26)[1:(ncol(t2_sf26) - 1)] <- cnames 
    
    # assign pacea class 
    class(t2_sf26) <- c("pacea_st", class(t2_sf26))
    
    # assign units attribute
    attr(t2_sf26, "units") <- jvars_table[which(jvars_table[, 1] == j), 3]
    
    # name file and write data
    if(i == "bot") {ti <- "bottom"} else {ti <- "surface"}
    tj <- jvars_table[which(jvars_table[, 1] == j), 2]
    objname <- paste("roms", ti, tj, sep = "_")
    #filename <- paste0("../pacea-data/data/",objname, "_", version, ".rds")
    filename <- paste0("../pacea-data/data/",objname, ".rds")
    assign(objname, t2_sf26)
    
    do.call("save", list(as.name(objname), file = filename, compress = "xz"))

    end <- Sys.time()
    jtime <- end-start
    print(jtime)
    names(jtime) <- paste(i, j, sep="_")
    proctimes <- c(proctimes, jtime)
  }
}




