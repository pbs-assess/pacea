#' Get large pacea data from repository
#'
#' Large spatiotemporal pacea data files can be downloaded individually from pacea-data.
#' 
#' Some large pacea data sets are stored in a GitHub repository, pacea-data, and get_pacea_data allows users to choose which data to download locally. Refer to paceadata_list(?EDIT?) to view the data files available for download. Code adapted from 'bcmaps'
#'
#' @param layer Name of the data object.
#' @param ask Logical. Should the user be asked before downloading the data to local cache? Defaults to the value of interactive().
#'
#' @return Data object requested
#' 
#' @importFrom httr GET content status_code
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' get_pacea_data("roms_surface_temperature")
#' }
#' 

get_pacea_data <- function(layer, ask = interactive()) {
  
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
    
    # dat <- get(load(file_dir, envir = .GlobalEnv))
    # return(dat)
    
  } else {
    
    ## internet errors for downloading
    if (!checkInternetConnection()) stop("No access to internet", call. = FALSE) 
    
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
    
    fileurl <- paste0("https://github.com/pbs-assess/pacea-data/blob/main/data/", layer, ".rds?raw=true") 
    
    filename <- paste0(layer,".rds")
    
    # Send GET request to retrieve the file
    response <- httr::GET(fileurl)
    
    # Check if the request was successful
    if (httr::status_code(response) == 200) {
      # Read the content of the response as raw data
      content_raw <- httr::content(response, "raw")
      
      # Create a temporary file path
      temp_file <- tempfile()
      
      # Write the raw data to the temporary file
      writeBin(content_raw, temp_file)
      
      # Read the .rds file into R
      dat <- get(load(temp_file))
      
      # Remove the temporary file
      file.remove(temp_file) 
      
      saveRDS(dat, file = file_dir, compress = "xz")

      return(dat)
      
    } else {
      
      stop("Error: Failed to download the file.")
      
    }
    
  }
}


#' Interactive function for Yes/No question
#' @noRd
ask <- function(...) {
  choices <- c("Yes", "No")
  cat(paste0(..., collapse = ""))
  utils::menu(choices) == which(choices == "Yes")
}

#' Check internet connection
#' @noRd
checkInternetConnection <- function() {
  url <- "http://www.google.com"
  con <- tryCatch({
    url("http://www.google.com", timeout = 5)
  }, error = function(e) {
    return(FALSE)
  })
  
  if (inherits(con, "try-error")) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}


