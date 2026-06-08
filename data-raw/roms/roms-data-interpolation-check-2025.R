#####
#
# BCCM interpolation data check
#
#


# load packages
library(devtools)
library(dplyr)
library(sf)
library(ggplot2)

sf_use_s2(FALSE)  # remove spherical geometry (s2) for sf operations

# load pacea
load_all()

# file list
bccm.files <- list.files("../pacea-data/data-bccm-full/", pattern = "_03")

# pacea-data path
dpath <- "../pacea-data/data-bccm-full/"


# plot output
# pdf("data-raw/roms/bccm_to2025_interpolated_plots.pdf")

# loop to check outliers

for(i in 1:length(bccm.files)){
# for(i in 1:2){

  print(paste0("i = ", i))
  
  ## bccm to 2025 (03)
  # file path
  fpath <- bccm.files[i]
  
  # data object name
  dataname <- substr(fpath, 1, nchar(fpath) - 7)
  
  # split string of fpath
  fpathstring <- strsplit(fpath, "_")
  
  # depth string name
  tdepth <- fpathstring[[1]][2]
  
  # variable string name
  tvar <- fpathstring[[1]][3]
  if(tvar %in% c("u", "v")){
    tvar <- paste(fpathstring[[1]][c(3,4)], collapse = "_")
  }
  if(tdepth %in% c("phytoplankton", "primaryproduction")){
    tvar <- fpathstring[[1]][2]
  }
  
  # load data
  load(paste0(dpath, fpath))
  
  # rename data
  tdat <- get(dataname)
  
  ######################
  
  # create vector of all values to get quantiles
  tdat_vec <- as.vector(as.matrix(unname(st_drop_geometry(tdat))))
  summary(tdat_vec)
  
  # using quantiles
  thr_lo <- quantile(tdat_vec, 0.001, na.rm = TRUE)
  thr_up <- quantile(tdat_vec, 0.999, na.rm = TRUE)
  
  # Find the minimum and max value of each column
  col_mins <- sapply(st_drop_geometry(tdat), min, na.rm = TRUE)
  col_maxs <- sapply(st_drop_geometry(tdat), max, na.rm = TRUE)
  
  # Sort the column mins and get the names of the lowest 4
  col_mins_ids <- names(sort(col_mins)[1:4])
  col_maxs_ids <- names(sort(col_maxs, decreasing = TRUE)[1:4])
  
  # Select those columns from your original dataframe
  tdat_mins <- tdat[, col_mins_ids, drop = FALSE]
  tdat_maxs <- tdat[, col_maxs_ids, drop = FALSE]
  
  tdat_outlo <- data.frame()
  tdat_outup <- data.frame()
  
  for(j in 1:4){
    
    # lower bound outliers
    idx <- order(tdat_mins[[j]], na.last = TRUE)[1:30]
    
    tdat2 <- tdat_mins[idx, j] |>
      mutate(date = names(tdat_mins[j])[1])
    names(tdat2)[1] <- "value"
    
    tdat_outlo <- rbind(tdat_outlo, tdat2)
    
    # upper bound outliers
    idx <- order(tdat_maxs[[j]], decreasing = TRUE, na.last = TRUE)[1:30]
    
    tdat2 <- tdat_maxs[idx, j] |>
      mutate(date = names(tdat_maxs[j])[1])
    names(tdat2)[1] <- "value"
    
    tdat_outup <- rbind(tdat_outup, tdat2)
    
    rm(idx, tdat2)
  }
  
  trange_lo <- range(tdat_outlo$value)
  trange_up <- range(tdat_outup$value)
  
  # plot lower bounds
  plo <- ggplot() + 
    geom_sf(data=bc_coast, fill = NA, col = "grey") +
    geom_sf(data = tdat_outlo, aes(color = paste(trange_lo, collapse = " to "))) +
    scale_color_manual(values = setNames("green", paste(trange_lo, collapse = " to ")),
                       name = "Flagged cells") +
    # geom_sf(data = tdat_outup, aes(color = paste(trange_up, collapse = " to "))) +
    # scale_color_manual(values = setNames("purple", paste(trange_up, collapse = " to ")),
    #                    name = "Flagged cells") +
    facet_wrap(.~date) + 
    ggtitle(label = paste(tdepth, tvar, "values", sep = " "), 
            subtitle = paste(names(thr_lo), "=", thr_lo)) + 
    theme_bw()
  
  # plot upper bounds
  pup <- ggplot() + 
    geom_sf(data=bc_coast, fill = NA, col = "grey") +
    # geom_sf(data = tdat_outlo, aes(color = paste(trange_lo, collapse = " to "))) +
    # scale_color_manual(values = setNames("green", paste(trange_lo, collapse = " to ")),
    #                    name = "Flagged cells") +
    geom_sf(data = tdat_outup, aes(color = paste(trange_up, collapse = " to "))) +
    scale_color_manual(values = setNames("purple", paste(trange_up, collapse = " to ")),
                       name = "Flagged cells") +
    facet_wrap(.~date) + 
    ggtitle(label = paste(tdepth, tvar, "values", sep = " "), 
            subtitle = paste(names(thr_up), "=", thr_up)) + 
    theme_bw()
  
  print(plo)
  print(pup)
  
  rm(list = c(dataname))
  rm(fpath, fpathstring, tdepth, tvar, dataname, tdat_vec,
     tdat, thr_lo, thr_up, col_mins, col_maxs, col_mins_ids, col_maxs_ids,
     tdat_mins, tdat_maxs, tdat_outlo, tdat_outup, trange_lo, trange_up)
  rm(plo, pup)
}

dev.off()


