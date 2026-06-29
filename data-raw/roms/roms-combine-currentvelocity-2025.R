#####
#
# This script is for combining the horizontal and vertical (u and v) current 
#  velocities to a single vector, which will include the current speed and angle
#  in radians and degrees. These will create additional data objects for pacea
# 
# 
# 


# load packages
library(sf)
library(dplyr)
library(ggplot2)
library(grid)
library(devtools)

# load pacea
load_all()

# directory to save to
pacea_data_dir <- paste0("../pacea-data/data-bccm-full/")  # Where to save .rds
version <- "03"


#####
# Surface current velocity

# read in bccm surface current velocity data
horiz_current<- bccm_surface_u_currentvelocity_full()
north_current<- bccm_surface_v_currentvelocity_full()

# horiz_current<- bccm_bottom_u_currentvelocity_full()
# north_current<- bccm_bottom_v_currentvelocity_full()

u_sf <- horiz_current   # eastward velocity
v_sf <- north_current   # northward velocity

dim(u_sf)
dim(v_sf)

bccm_surface_current_speed_full <- u_sf[,ncol(u_sf)]
bccm_surface_current_anglerad_full <- u_sf[,ncol(u_sf)]
bccm_surface_current_angledeg_full <- u_sf[,ncol(u_sf)]

for(i in 1:(ncol(u_sf) - 1)){
  
  tmonth <- names(u_sf)[i]
  u_vec <- u_sf[[tmonth]]
  v_vec <- v_sf[[tmonth]]
  
  tspeed <- sqrt(u_vec^2 + v_vec^2)
  trad <- atan2(v_vec, u_vec)
  tdeg <- trad * 180 / pi
  
  bccm_surface_current_speed_full <- bccm_surface_current_speed_full %>% mutate(tmonth = tspeed)
  bccm_surface_current_anglerad_full <- bccm_surface_current_anglerad_full %>% mutate(tmonth = trad)
  bccm_surface_current_angledeg_full <- bccm_surface_current_angledeg_full %>% mutate(tmonth = tdeg)
  
  names(bccm_surface_current_speed_full)[i+1] <- tmonth
  names(bccm_surface_current_anglerad_full)[i+1] <- tmonth
  names(bccm_surface_current_angledeg_full)[i+1] <- tmonth
  
}

# rearrange geometry column
bccm_surface_current_speed_full <- st_drop_geometry(bccm_surface_current_speed_full) %>% cbind(u_sf[,ncol(u_sf)]) %>% st_as_sf()
bccm_surface_current_anglerad_full <- st_drop_geometry(bccm_surface_current_anglerad_full) %>% cbind(u_sf[,ncol(u_sf)]) %>% st_as_sf()
bccm_surface_current_angledeg_full <- st_drop_geometry(bccm_surface_current_angledeg_full) %>% cbind(u_sf[,ncol(u_sf)]) %>% st_as_sf()

# assign units attribute
attr(bccm_surface_current_speed_full, "units") <- "Current velocity (m/s)"
attr(bccm_surface_current_anglerad_full, "units") <- "Current angle (radians)"
attr(bccm_surface_current_angledeg_full, "units") <- "Current angle (degrees)"

# assign depth attribute
attr(bccm_surface_current_speed_full, "depth") <- "surface"
attr(bccm_surface_current_anglerad_full, "depth") <- "surface"
attr(bccm_surface_current_angledeg_full, "depth") <- "surface"

# assign bccm_full attribute that will extend plotting (Andy's addition)
attr(bccm_surface_current_speed_full, "bccm_full") <- TRUE
attr(bccm_surface_current_anglerad_full, "bccm_full") <- TRUE
attr(bccm_surface_current_angledeg_full, "bccm_full") <- TRUE

class(bccm_surface_current_speed_full) <- c("pacea_st", "sf", "tbl_df", "tbl", "data.frame")
class(bccm_surface_current_anglerad_full) <- c("pacea_st", "sf", "tbl_df", "tbl", "data.frame")
class(bccm_surface_current_angledeg_full) <- c("pacea_st", "sf", "tbl_df", "tbl", "data.frame")

# save files in pacea-data folder
save(bccm_surface_current_speed_full, 
     file = paste0(pacea_data_dir, "bccm_surface_current_speed_full_", version, ".rds"), 
     compress = "xz")

save(bccm_surface_current_anglerad_full, 
     file = paste0(pacea_data_dir, "bccm_surface_current_anglerad_full_", version, ".rds"), 
     compress = "xz")

save(bccm_surface_current_angledeg_full, 
     file = paste0(pacea_data_dir, "bccm_surface_current_angledeg_full", "_", version, ".rds"), 
     compress = "xz")



#####
# Bottom current velocity
#

horiz_current<- bccm_bottom_u_currentvelocity_full()
north_current<- bccm_bottom_v_currentvelocity_full()

u_sf <- horiz_current   # eastward velocity
v_sf <- north_current   # northward velocity

dim(u_sf)
dim(v_sf)

bccm_bottom_current_speed_full <- u_sf[,ncol(u_sf)]
bccm_bottom_current_anglerad_full <- u_sf[,ncol(u_sf)]
bccm_bottom_current_angledeg_full <- u_sf[,ncol(u_sf)]

for(i in 1:(ncol(u_sf) - 1)){
  
  tmonth <- names(u_sf)[i]
  u_vec <- u_sf[[tmonth]]
  v_vec <- v_sf[[tmonth]]
  
  tspeed <- sqrt(u_vec^2 + v_vec^2)
  trad <- atan2(v_vec, u_vec)
  tdeg <- trad * 180 / pi
  
  bccm_bottom_current_speed_full <- bccm_bottom_current_speed_full %>% mutate(tmonth = tspeed)
  bccm_bottom_current_anglerad_full <- bccm_bottom_current_anglerad_full %>% mutate(tmonth = trad)
  bccm_bottom_current_angledeg_full <- bccm_bottom_current_angledeg_full %>% mutate(tmonth = tdeg)
  
  names(bccm_bottom_current_speed_full)[i+1] <- tmonth
  names(bccm_bottom_current_anglerad_full)[i+1] <- tmonth
  names(bccm_bottom_current_angledeg_full)[i+1] <- tmonth
  
}

# rearrange geometry column
bccm_bottom_current_speed_full <- st_drop_geometry(bccm_bottom_current_speed_full) %>% cbind(u_sf[,ncol(u_sf)]) %>% st_as_sf()
bccm_bottom_current_anglerad_full <- st_drop_geometry(bccm_bottom_current_anglerad_full) %>% cbind(u_sf[,ncol(u_sf)]) %>% st_as_sf()
bccm_bottom_current_angledeg_full <- st_drop_geometry(bccm_bottom_current_angledeg_full) %>% cbind(u_sf[,ncol(u_sf)]) %>% st_as_sf()

# assign units attribute
attr(bccm_bottom_current_speed_full, "units") <- "Current velocity (m/s)"
attr(bccm_bottom_current_anglerad_full, "units") <- "Current angle (radians)"
attr(bccm_bottom_current_angledeg_full, "units") <- "Current angle (degrees)"

# assign depth attribute
attr(bccm_bottom_current_speed_full, "depth") <- "bottom"
attr(bccm_bottom_current_anglerad_full, "depth") <- "bottom"
attr(bccm_bottom_current_angledeg_full, "depth") <- "bottom"

# assign bccm_full attribute that will extend plotting (Andy's addition)
attr(bccm_bottom_current_speed_full, "bccm_full") <- TRUE
attr(bccm_bottom_current_anglerad_full, "bccm_full") <- TRUE
attr(bccm_bottom_current_angledeg_full, "bccm_full") <- TRUE

class(bccm_bottom_current_speed_full) <- c("pacea_st", "sf", "tbl_df", "tbl", "data.frame")
class(bccm_bottom_current_anglerad_full) <- c("pacea_st", "sf", "tbl_df", "tbl", "data.frame")
class(bccm_bottom_current_angledeg_full) <- c("pacea_st", "sf", "tbl_df", "tbl", "data.frame")

# save files in pacea-data folder
save(bccm_bottom_current_speed_full, 
     file = paste0(pacea_data_dir, "bccm_bottom_current_speed_full_", version, ".rds"), 
     compress = "xz")

save(bccm_bottom_current_anglerad_full, 
     file = paste0(pacea_data_dir, "bccm_bottom_current_anglerad_full_", version, ".rds"), 
     compress = "xz")

save(bccm_bottom_current_angledeg_full, 
     file = paste0(pacea_data_dir, "bccm_bottom_current_angledeg_full", "_", version, ".rds"), 
     compress = "xz")






