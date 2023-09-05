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

# #####
# # create polygons for cropping to roms data
# nc_dat <- nc_open(paste0("data-raw/roms/bcc42_era5glo12r4_mon1993to2019_botTSO.nc"))
# 
# # load lon-lat and mask layers from netcdf
# nc_lon <- as.vector(ncvar_get(nc_dat, "lon_rho"))
# nc_lat <- as.vector(ncvar_get(nc_dat, "lat_rho"))
# 
# nc_var <- ncvar_get(nc_dat, "temp")
# nc_varmat <- apply(nc_var, 3, c)
# 
# # put sst into dataframe and sf object
# dat <- data.frame(x = nc_lon, y = nc_lat) %>% cbind(nc_varmat) %>%
#   st_as_sf(coords = c("x", "y"), crs = "EPSG:4326") %>%
#   st_transform(crs = "EPSG: 3005")
# 
# # create polygon for cropping ROMS data
# roms_cave <- dat %>% 
#   na.omit() %>%
#   concaveman::concaveman()
# roms_buff <- dat %>% 
#   na.omit() %>%
#   st_geometry() %>% 
#   st_buffer(dist = 5000) %>%
#   st_union()
# 
# rm(nc_dat, nc_lon, nc_lat, nc_var, nc_varmat, dat)
# #####


#####
# PARAMETERS


# OPTION 1 FOR LOOPING THROUGH VARIABLES FOR EACH DEPTH
# loop variables
ifiles <- list.files("./data-raw/roms/", pattern = "TSOpH.nc")
jvars <- c("temp", "salt", "Oxygen", "pH")

# index table
vars_fullname <- c("temperature", "salinity", "oxygen", "pH")
vars_units <- c("Temperature (\u00B0C)",
                "Salinity (ppt)",
                "Dissolved oxygen content (mmol-oxygen m^-3)",
                "pH")
jvars_table <- cbind(jvars, vars_fullname, vars_units)



# OPTION 2 FOR LOOPING THROUGH ONLY SURFACE VARIABLES (PRIMARY PRODUCTION)
# loop variables
ifiles <- list.files("./data-raw/roms/", pattern = "zInt_PT.nc")
jvars <- c("phytoplankton", "PTproduction")

# index table
vars_fullname <- c("phytoplankton", "primaryproduction")
vars_units <- c("Phytoplankton (mmol-nitrogen m^-2)",
                "Total primary production (gC m^-2 d^-1)")
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

# surface mask layer
snc_dat <- nc_open("data-raw/roms/bcc42_era5glo12r4_mon1993to2019_surTSOpH.nc")
snc_lon <- as.vector(ncvar_get(snc_dat, "lon_rho"))
snc_lat <- as.vector(ncvar_get(snc_dat, "lat_rho"))
svar <- as.vector(ncvar_get(snc_dat, "temp", count = c(-1, -1, 1)))

sdat <- data.frame(x = snc_lon, y = snc_lat, value = svar) %>%
  st_as_sf(coords = c("x", "y"), crs = "EPSG:4326") %>%
  st_transform(crs = "EPSG:3005")

sroms_cave <- sdat %>% 
  na.omit() %>%
  concaveman::concaveman()
sroms_buff <- sdat %>% 
  na.omit() %>%
  st_geometry() %>% 
  st_buffer(dist = 2000) %>%
  st_union() %>%
  st_as_sf()

rm(snc_dat, snc_lon, snc_lat, svar, sdat)
# END parameters
#####






for(i in ifiles) {
  nc_dat <- nc_open(paste0("data-raw/roms/",i))
  
  # load lon-lat and mask layers from netcdf
  nc_lon <- as.vector(ncvar_get(nc_dat, "lon_rho"))
  nc_lat <- as.vector(ncvar_get(nc_dat, "lat_rho"))

  # depth from file name
  if(substr(i, 33, 35) %in% c("bot", "sur")){
    ti <- substr(i, 33, 35)
    if(ti == "bot") {ti <- "bottom"} else {ti <- "surface"}
  } else {
    ti <- strsplit(substr(i, 33, nchar(i)), "_")[[1]][1]
  }
  
  for(j in jvars) {
    
    start <- Sys.time()
    
    nc_var <- ncvar_get(nc_dat, j)
    nc_varmat <- apply(nc_var, 3, c)
    
    # put sst into dataframe and sf object
    dat <- data.frame(x = nc_lon, y = nc_lat) %>% cbind(nc_varmat)
    dat_sf <- st_as_sf(dat, coords = c("x", "y"), crs = "EPSG:4326")
    tdat_sf <- st_transform(dat_sf, crs = "EPSG: 3005")
    
    # create polygon for cropping ROMS data
    roms_cave <- tdat_sf %>% 
      na.omit() %>%
      concaveman::concaveman()
    roms_buff <- tdat_sf %>% 
      na.omit() %>%
      st_geometry() %>% 
      st_buffer(dist = 2000) %>%
      st_union() %>%
      st_as_sf()
    
    # interpolate data
    # 2 km res
    output2 <- point2rast(data = tdat_sf, spatobj = inshore_poly, loc = llnames, cellsize = 2000, nnmax = nmax, as = "SpatRast")
    # 6 km res
    output6 <- point2rast(data = tdat_sf, spatobj = offshore_poly, loc = llnames, cellsize = 6000, nnmax = nmax, as = "SpatRast")
    
    # crop out grid cells with polygon masks
    t2_sf2 <- output2 %>% 
      mask(bccm_eez_poly) %>% 
      mask(inshore_poly) %>%
      stars::st_as_stars() %>%  ## check here for converting to points (not raster)
      st_as_sf()
    t2_sf6 <- output6 %>%
      mask(bccm_eez_poly) %>%
      mask(offshore_poly) %>%
      stars::st_as_stars() %>%
      st_as_sf()
    
    # mask 2k grid with 6k grid, then combine grids
    t2_sf26a <- t2_sf2[!st_intersects(st_union(t2_sf6), t2_sf2, sparse=FALSE, prepared=TRUE),] %>%
      rbind(t2_sf2[st_intersects(st_union(t2_sf6), t2_sf2, sparse=FALSE, prepared=TRUE),]) %>% 
      rbind(t2_sf6)
    
    
    ##### BC MASK OPTION 1 - Using bc shapefile
    # index points that dont intersect with bc coast shapefile
    #  disjoint - do not share space
    # dis2 <- t2_sf26[st_disjoint(st_union(tbc), t2_sf26, sparse=FALSE, prepared=TRUE),]
    # 
    # #  convert bc coast to sf linestring and finding coastline intersections separately - increased processing speed
    # #  using st_intersects is much faster than other predicate functions
    # sub.t2 <- t2_sf26[st_intersects(st_union(tbc), t2_sf26, sparse=FALSE, prepared=TRUE),]
    # inter.line <- sub.t2[st_intersects(tbc.line, sub.t2, sparse=FALSE, prepared=TRUE),]
    # t2_sf26 <- rbind(dis2, inter.line)
    
    ##### BC MASK OPTION 2 - Using roms outline 
    # 1. use roms_cave
    t2_sf26b <- t2_sf26a[roms_cave,]
    
    # 2. use roms_buff to get haida gwaii outline and shore
    t2_sf26b <- t2_sf26b[roms_buff,]
    
    # 3. use default surface roms_cave
    t2_sf26b <- t2_sf26b[sroms_cave,]
    
    # 4. use default surface roms_buff
    t2_sf26 <- t2_sf26b[sroms_buff,]
    
    # data should have 41,288 grid cells
    # if(nrow(t2_sf26) != 41288){
    #   out.msg <- paste0(as.symbol(t2_sf26), " nrows = ", nrow(get(objname)), 
    #                     ". nrows not equal to 13,377,312...wrangle to long format (or somethinig else) failed.")
    #   stop(out.msg)
    # }
    
    # assign column names as year_month
    names(t2_sf26)[1:(ncol(t2_sf26) - 1)] <- cnames 
    
    # covert to long format data - Don't do long format as it is too big
    # t3_sf26 <- t2_sf26 %>%
    #   tidyr::pivot_longer(cols = !last_col(), cols_vary = "slowest", names_to = "date", values_to = "value")  %>%
    #   mutate(year = substr(date, 1, 4),
    #          month = substr(date, 6, 7)) %>%
    #   dplyr::select(-date) %>%
    #   relocate(value, .after = last_col()) %>%
    #   relocate(geometry, .after = last_col()) 
    
    # round to 6 decimal places to reduce file size 
    # t3_sf26[, "value"] <- t3_sf26$value %>% # for long format
    #   round(digits = 6)
    t3_sf26 <- t2_sf26 %>%
      st_drop_geometry() %>%
      round(digits = 6) %>%
      st_as_sf(geometry = st_geometry(t2_sf26))
    
    # assign pacea class 
    class(t3_sf26) <- c("pacea_st", class(t3_sf26), "tbl_df", "tbl")
    
    # assign units attribute
    attr(t3_sf26, "units") <- jvars_table[which(jvars_table[, 1] == j), 3]
    
    # name file and write data
    tj <- jvars_table[which(jvars_table[, 1] == j), 2]
    if(ti == "zInt"){
      objname <- paste("bccm", tj, sep = "_")
    } else {
      objname <- paste("bccm", ti, tj, sep = "_")
    }
    filename <- paste0("../pacea-data/data/",objname, "_", version, ".rds")
    #filename <- paste0("../pacea-data/data/",objname, ".rds")
    assign(objname, t3_sf26)
    
    do.call("save", list(as.name(objname), file = filename, compress = "xz"))

    end <- Sys.time()
    jtime <- end-start
    print(jtime)
    names(jtime) <- paste(ti, tj, sep="_")
    proctimes <- c(proctimes, jtime)
    
    # remove files 
    rm(dat, dat_sf, tdat_sf, roms_cave, roms_buff,
       output2, output6, t2_sf2, t2_sf6, t2_sf26,
       t2_sf26a, t2_sf26b, t3_sf26, nc_var, nc_varmat)
    rm(list = objname)
    gc()
  }
}




