# get data from paceaData

# code adapted from 'bcmaps' package

layer <- "pcod"

get_data <- function(layer, ask = interactive(), force = FALSE) {
  
  ## edit message
  if (!is.character(layer)) {
    stop("You must refer to the map layer as a character string (in 'quotes')\n
         Use the function available_layers() to get a list of layers")
  }
  
  ## find data in row
  data_list <- pacea_data
  data_row <- data_list[data_list[["data_name"]]==layer, ]
  if (nrow(data_row) != 1L) {
    stop(layer, " is not an available data object")
  }
  
  # look for data in pacea cache folder, return dataset
  cache_dir <- pacea_cache()
  file_dir <- paste0(cache_dir, "/", layer, ".rds")
  file_dir <- "C:\\github\\pacea\\data/grid26.rda"  #test
  
  
  
  # if file already exists
  if (file.exists(file_dir)) {
    
    ## ISSUE: figure out how to read and save file to cache without it loading the same object name (SEE bcmaps), might have to save pacea data using saveRDS() function as .rds file?
    readRDS(file_dir)
    
  } else {
    
    
    
    ## internet errors for downloading
    if (!curl::has_internet()) stop("No access to internet", call. = FALSE) 
    
    ## interactive ask to store in cache folder - from bcmaps package
    if (ask) {
      ans <- ask(paste("pacea would like to download and store these data in the directory:",
                       cache_dir, "Is that okay?", sep = "\n"))
      if (!ans) stop("Exiting...", call. = FALSE)
    }
    
    # check if directory exists
    if (!dir.exists(cache_dir)) {
      message("Creating directory to hold pacea data at \n", cache_dir)
      dir.create(cache_dir, recursive = T)
    } else {
      message("Saving to pacea data directory at \n", cache_dir)
    }
    
    
    # download data using url, potentially use pacea_cache to store datafiles in a cache folder?
    #  this function (Rfssa) loads the compressed data as its original object name
    #  will need to use the user-identified name ("layer") to save data to cached folder to then be lazydata loaded
    fileurl <- "https://github.com/pbs-assess/pacea/blob/main/data/oni.rda?raw=true"
    fileurl <- paste0("https://github.com/pbs-assess/sdmTMB/blob/main/data/", layer, ".rda?raw=true")
    
    saveRDS(Rfssa::load_github_data(fileurl), file = paste0(cache_dir, "/", layer, ".rds"))
    
    
    
    
    
    
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # save layer to local cached pacea folder; make sure that data is loaded when pacakage is loaded for use
  saveRDS()
  
  
  
  ret <- get_catalogue_data(layer, ask = ask, force = force)
  
  ret
  
}


# list of pacea data
pacea_data <- function() {
  
  
}

# #' @noRd
ask <- function(...) {
  choices <- c("Yes", "No")
  cat(paste0(..., collapse = ""))
  utils::menu(choices) == which(choices == "Yes")
}





