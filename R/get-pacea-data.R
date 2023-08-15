#' Get large pacea data from repository
#'
#' Large spatiotemporal pacea data files can be downloaded individually from pacea-data.
#' 
#' Some large pacea data sets are stored in a GitHub repository, pacea-data, and get_pacea_data allows users to choose which data to download locally. Refer to `pacea_data` to view the data files available for download. Code adapted from 'bcmaps'
#'
#' @param layer Name of the data object.
#' @param update Logical. Would you like to check for a newer version of the layer? 
#' @param ask Logical. Should the user be asked before downloading the data to local cache? Defaults to the value of interactive().
#' @param force Logical. Should download of data be forced? Overrides `ask` argument if TRUE. 
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

get_pacea_data <- function(layer, update = FALSE, ask = interactive(), force = FALSE) {
  
  ## edit message
  if (!is.character(layer)) {
    stop("Data object must be referred to as a character string (in 'quotes')\n
         Use the function ?...? to get a list of data objects available", call. = FALSE)
  }
  
  # testing data
  test_names <- c("test_data", "test_data_01", "test_data_02")
  
  ## find data in row - CHANGE datalist name if necessary
  data_list <- pacea_data
  data_row <- data_list[grep(layer, data_list[["data_name"]], ignore.case = TRUE), , drop = FALSE]  
  if (nrow(data_row) != 1L && !(layer %in% test_names)) {
    stop(layer, " is not an available data object")
  }
  
  # look for data in pacea cache folder, return dataset
  cache_dir <- pacea_cache()
  file_list <- list.files(cache_dir)
  grep_list <- file_list[grep(layer, file_list, ignore.case = TRUE)]
  
  # testthat data functions; end function here
  if (force == "testDataFunctions"){
    stop("testing data functions successful")
  }
  
  # if file already exists -- ## CHECK HERE FOR ERROR ON HALF DOWNLOADED FILE
  if (length(grep_list) > 0) {
    
    local_filename <- grep_list[order(grep_list, decreasing = TRUE)][1]
    
    local_file_dir <- paste0(cache_dir, "/", local_filename)
    
    if (update) {
      
      ## internet errors for downloading
      if (!checkInternetConnection() || force == "testInternetError") {
        
        warning("No access to internet - could not check for updates.", call. = FALSE) 
        
        dat <- readRDS(local_file_dir)
        return(dat)
        
      }
      
      # list files from github repository
      listurl <- "https://api.github.com/repos/pbs-assess/pacea-data/git/trees/main?recursive=1"
      req <- httr::GET(listurl)
      httr::stop_for_status(req)
      
      git_file_list <- unlist(lapply(content(req)$tree, "[", "path"), use.names = FALSE)
      git_file_dir <- git_file_list[grep(paste0("data/", layer), git_file_list, ignore.case = TRUE)]
      git_file_dir <- git_file_dir[order(git_file_dir, decreasing = TRUE)][1]
      git_filename <- strsplit(git_file_dir, "/")[[1]][2]
      
      # compare versions
      if(local_filename == git_filename) {
        
        warning("Most recent version of data already downloaded in cache folder!", call. = FALSE)
        
        dat <- readRDS(local_file_dir)
        return(dat)
        
      } else {
        
        ans <- TRUE
        
        if(ask && !force){
          ans <- ask(paste("Newer version of data available and previous version will be deleted from local cache folder:",
                           cache_dir, "Is that okay?", sep = "\n"))
        }
        
        if (!ans) {
          
          warning("Returned local version of data.", call. = FALSE)
          
          dat <- readRDS(local_file_dir)
          return(dat)
          
        } else {

          fileurl <- paste0("https://github.com/pbs-assess/pacea-data/blob/main/data/", git_filename, "?raw=true") 
          
          # Send GET request to retrieve the file
          response <- httr::GET(fileurl)
          httr::stop_for_status(response)
          
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
          
          # create file name with version number
          filename <- paste0(git_filename)
          file_dir <- paste0(cache_dir, "/", filename)
          
          saveRDS(dat, file = file_dir, compress = "xz")
          
          # delete previous version in local folder
          unlink(local_file_dir)
          
          message("Data successfully updated and downloaded to local cache folder!")
          
          return(dat)
          
        }
      }
      
    } else { # no update
      
      dat <- readRDS(local_file_dir)
      return(dat)
    }
    
  } else { # if file doesn't exist
    
    ## internet errors for downloading
    if (!checkInternetConnection() || force == "testInternetError") stop("No access to internet", call. = FALSE) 
    
    ## interactive ask to store in cache folder - from bcmaps package
    if (ask && !force) {
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
    
    # list files from github repository
    listurl <- "https://api.github.com/repos/pbs-assess/pacea-data/git/trees/main?recursive=1"
    req <- httr::GET(listurl)
    httr::stop_for_status(req)
    
    git_file_list <- unlist(lapply(content(req)$tree, "[", "path"), use.names = FALSE)
    git_file_dir <- git_file_list[grep(paste0("data/", layer), git_file_list, ignore.case = TRUE)]
    git_file_dir <- git_file_dir[order(git_file_dir, decreasing = TRUE)][1]
    git_filename <- strsplit(git_file_dir, "/")[[1]][2]
    
    # download data
    fileurl <- paste0("https://github.com/pbs-assess/pacea-data/blob/main/data/", git_filename, "?raw=true") 
    
    # Send GET request to retrieve the file
    response <- httr::GET(fileurl)
    httr::stop_for_status(response)
    
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
    
    # create file name with version number
    filename <- paste0(git_filename)
    file_dir <- paste0(cache_dir, "/", filename)
    
    saveRDS(dat, file = file_dir, compress = "xz")
    
    return(dat)
    
  }
}


#' Interactive function for Yes/No question - from bcmaps package
#' @noRd
ask <- function(...) {
  choices <- c("Yes", "No")
  cat(paste0(..., collapse = ""))
  
  # calling scope - testthat detection
  tb <- .traceback(x = 0)
  
  if(!any(unlist(lapply(tb, function(x) any(grepl("test_env", x))))) && interactive()){
    utils::menu(choices) == which(choices == "Yes")
  } else{
    return(TRUE)
  }
}

#' Check internet connection
#' @noRd
checkInternetConnection <- function() {
  url <- "http://www.google.com"
  response <- try(httr::GET(url), silent = TRUE)
  status_code <- try(status_code(response), silent = TRUE)
  return(status_code == 200)
}
