# ROMs data from Angelina Pena - full bottom and surface variables that Andy requested

library(devtools)
library(dplyr)
library(terra)
library(gstat)
library(sf)
library(stars)
library(ncdf4)
library(ggplot2)

sf_use_s2(FALSE)  # remove spherical geometry (s2) for sf operations

# load pacea
load_all()


#####
# START - load data to environment

# transform bc_coast
tbc <- st_transform(bc_coast, crs=crs(romseez_poly))

# convert to multilinestring
tbc.line <- st_cast(tbc, "MULTILINESTRING")

#####
# PARAMETERS
# loop variables
idepth <- c("bot", "sur")
jvars <- c("temp", "salt", "Oxygen")

llnames <- c("x", "y")
nmax <- 4

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
      stars::st_as_stars() %>%  ## check here fro converting to points (not raster)
      st_as_sf()
    t2_sf6 <- output6 %>%
      mask(romseez_poly) %>%
      mask(offshore_poly) %>%
      stars::st_as_stars() %>%
      st_as_sf()
    
    # mask 2k grid with 6k grid, then combine grids
    t2_sf2 <- t2_sf2[!st_intersects(st_union(t2_sf6), t2_sf2, sparse=F, prepared=T),] %>%
      rbind(t2_sf2[st_intersects(st_union(t2_sf6), t2_sf2, sparse=F, prepared=T),])
    t2_sf26 <- t2_sf2 %>% rbind(t2_sf6)
    
    # index points that dont intersect with bc coast shapefile
    #  disjoint - do not share space
    dis2 <- t2_sf26[st_disjoint(st_union(tbc), t2_sf26, sparse=F, prepared=T),]
    
    #  convert bc coast to sf linestring and finding coastline intserections separately - increased processing speed
    #  using st_intersects is much faster than other predicate functionss
    sub.t2 <- t2_sf26[st_intersects(st_union(tbc), t2_sf26, sparse=F, prepared=T),]
    inter.line <- sub.t2[st_intersects(tbc.line, sub.t2, sparse=F, prepared=T),]
    t2_sf26 <- rbind(dis2, inter.line)
    
    ##
    
    # name file and write data
    filename <- paste(i, j, sep = "_")
    assign(filename, t2_sf26)
    do.call("use_data", list(as.name(filename), compress = "xz"))

    end <- Sys.time()
    jtime <- end-start
    print(jtime)
    names(jtime) <- paste(i, j, sep="_")
    proctimes <- c(proctimes, jtime)
  }
}




