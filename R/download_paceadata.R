#' Get data from paceaData
#'
#' Large spatiotemporal pacea data files can be downloaded individually from paceaData.
#' 
#' Some large pacea data sets are stored in paceaData and get_data allows users to choose which data to download locally. Refer to paceadata_list(?EDIT?) to view the data files available for download. Code adapted from 'bcmaps'
#'
#' @param layer Name of the data object.
#' @param ask Logical. Should the user be asked before downloading the data to local cache? Defaults to the value of interactive().
#'
#' @return Data object requested
#' 
#' @importFrom curl has_internet
#' @importFrom Rfssa load_github_data
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' get_data("roms_temperature_surface")
#' }
#' 

get_data <- function(layer, ask = interactive()) {
  
  ## edit message
  if (!is.character(layer)) {
    stop("Data object must be referred to as a character string (in 'quotes')\n
         Use the function ?...? to get a list of data objects available")
  }
  
  ## find data in row - CHANGE datalist name if necessary
  # data_list <- pacea_data
  # data_row <- data_list[data_list[["data_name"]]==layer, ]
  # if (nrow(data_row) != 1L) {
  #   stop(layer, " is not an available data object")
  # }
  
  # look for data in pacea cache folder, return dataset
  cache_dir <- pacea_cache()
  file_dir <- paste0(cache_dir, "/", layer, ".rds")
  
  
  # if file already exists
  if (file.exists(file_dir)) {
    
    dat <- readRDS(file_dir)
    return(dat)
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
      message("Saving to pacea cache directory at \n", cache_dir)
    }
    
    
    # download data using url, potentially use pacea_cache to store datafiles in a cache folder?
    #  this function (Rfssa) loads the compressed data as its original object name
    #  will need to use the user-identified name ("layer") to save data to cached folder to then be lazydata loaded
    
    
    fileurl <- paste0("https://github.com/pbs-assess/pacea/blob/main/data/", layer, ".rda?raw=true")  ## CHANGE URL 
    
    Rfssa::load_github_data(fileurl)
    
    dat <- get(layer)
    
    saveRDS(dat, file = paste0(cache_dir, "/", layer, ".rds"), compress = "xz")
    
    return(dat)
    
  }
}


#' Interactive function for Yes/No question
#' @noRd
ask <- function(...) {
  choices <- c("Yes", "No")
  cat(paste0(..., collapse = ""))
  utils::menu(choices) == which(choices == "Yes")
}


