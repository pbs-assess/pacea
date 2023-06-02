#' Get large pacea data from repository
#'
#' Large spatiotemporal pacea data files can be downloaded individually from pacea-data.
#' 
#' Some large pacea data sets are stored in a GitHub repository, pacea-data, and get_pacea_data allows users to choose which data to download locally. Refer to paceadata_list(?EDIT?) to view the data files available for download. Code adapted from 'bcmaps'
#'
#' @param layer Name of the data object.
#' @param update Logical. Would you like to check for a newer version of the layer? 
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

get_pacea_data <- function(layer, update = FALSE, ask = interactive()) {
  
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
  file_list <- list.files(cache_dir)
  grep_list <- file_list[grep(layer, file_list)]
  
  # if file already exists
  if (length(grep_list) > 0) {
    
    local_filename <- grep_list[order(grep_list, decreasing = T)][1]
    
    local_file_dir <- paste0(cache_dir, "/", local_filename)
    
    if (update) {
      
      ## internet errors for downloading
      if (!checkInternetConnection()) {
        
        warning("No access to internet - could not check for updates.", call. = FALSE) 
        
        dat <- readRDS(local_file_dir)
        return(dat)
        
      }
      
      fileurl <- paste0("https://github.com/pbs-assess/pacea-data/blob/main/data/", layer, ".rds?raw=true") 
      
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
        
        # Read the .rds file into R and get name of file (can be different than path name)
        dat_name <- load(temp_file)
        
        # name data to generic object name
        dat <- get(dat_name)
        
        # Remove the temporary file
        file.remove(temp_file) 
        
        # compare versions of local file vs internet file
        tlocal_filename <- substr(local_filename, 1, nchar(local_filename) - 4)
        local_file_version <- substr(tlocal_filename, nchar(tlocal_filename) - 1, nchar(tlocal_filename))
        web_file_version <- substr(dat_name, nchar(dat_name) - 1, nchar(dat_name))
        
        if(local_file_version == web_file_version){
          
          message("Most recent version of data already downloaded in cache folder!")
          
          dat <- readRDS(local_file_dir)
          return(dat)
          
        } else {
          
          if (ask) {
            ans <- ask(paste("Newer version of data available and will delete previous version in local cache folder:",
                             cache_dir, "Is that okay?", sep = "\n"))
            if (!ans) {
              
              message("Returned local version of data.")
              
              dat <- readRDS(local_file_dir)
              return(dat)
              
            } else {
              
              # create file name with version number
              filename <- paste0(dat_name,".rds")
              file_dir <- paste0(cache_dir, "/", filename)
              
              saveRDS(dat, file = file_dir, compress = "xz")
              
              # delete previous version in local folder
              unlink(local_file_dir)
              
              return(dat)
              
              message("Data successfully updated!")
              
            }
          } 
        }
        
      } else { 
        
        dat <- readRDS(local_file_dir)
        return(dat)
        
        warning("Failed to download an updated version of the file.")
      }
      
    } else { # no update
      
      dat <- readRDS(local_file_dir)
      return(dat)
    }
    
  } else { # if file doesn't exist
    
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
      dir.create(cache_dir, recursive = TRUE)
    } else {
      message("Saving to pacea cache directory at \n", cache_dir)
    }
    
    fileurl <- paste0("https://github.com/pbs-assess/pacea-data/blob/main/data/", layer, ".rds?raw=true") 
    
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
      
      # Read the .rds file into R and get name of file (can be different than path name)
      #dat <- get(load(temp_file))
      dat_name <- load(temp_file)
      
      # name data to generic object name
      dat <- get(dat_name)
      
      # Remove the temporary file
      file.remove(temp_file)
      
      # create file name with version number
      filename <- paste0(dat_name,".rds")
      file_dir <- paste0(cache_dir, "/", filename)
      
      saveRDS(dat, file = file_dir, compress = "xz")

      return(dat)
      
    } else {
      
      stop("Error: Failed to download the file.")
      
    }
  }
}


#' Interactive function for Yes/No question - from bcmaps package
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
  response <- try(httr::GET(url), silent=TRUE)
  status_code <- try(status_code(response), silent=TRUE)
  return(status_code == 200)
}
