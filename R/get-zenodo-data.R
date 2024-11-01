#' Get large data products from Zenodo
#'
#' Assumes version `_01` for now. Have to adapt code if version gets updated,
#' but it's quite fiddly (though Travis did for BCCM).  TODO
#'
#' TODO update if get same function working for bccm_full.
#'
#' HOTSSea model results have been wrangled from the original results provided
#'  by Greig Oldford (see data-raw/hotssea). These are too big to be included in
#'  pacea as standard data objects, and so have been archived on Zenodo. The
#'  function downloads an object and saves it in on the user's computer. It is
#'  automatically called from TODO, and is not really needed separately by
#'  users. See that for usage.
#'
#' Based on [get_bccm_data()], but getting from Zenodo not `pacea-data/`.
#'
#' @param layer Name of the data object.
#' @param update Logical. Would you like to check for a newer version of the layer?
#' @param ask Logical. Should the user be asked before downloading the data to local cache? Defaults to the value of interactive().
#' @param force Logical. Should download of data be forced? Overrides `ask` argument if TRUE.
#' @param version Character. Version number of data on Zenodo (as in what is
#'   appended at the end of the filenames).
#' @param cache_subfolder Character. The folder
#'   `paste0(pacea::pacea_cache(), cache_subfolder)` will be
#'   searched first for the desired model output file, and if the no file is
#'   there then it will be downloaded from Zenodo into this directory.
#' @return Data object requested
#'
#' @importFrom httr GET content status_code stop_for_status
#' @importFrom foreach %dopar%
#' @author Andrew Edwards and Travis Tai
#' @export
#'
#' @examples
#' \dontrun{
#' tt <- get_zenodo_data("hotssea_surface_temperature_max")
#' plot(tt)
#' }
#'
get_zenodo_data <- function(layer,
                           update = FALSE,
                           ask = interactive(),
                           force = FALSE,
                           version = "01",
                           cache_subfolder = "hotssea"){
  ## edit message
  if (!is.character(layer)) {
    stop("Data object must be referred to as a character string (in 'quotes')\n
         Use the function ?...? to get a list of data objects available", call. = FALSE)
  }

  if(version != "01"){
    stop("get_zonodo_data() currently only set up for version 01")
  }

  # testing data
  test_names <- c("test_data", "test_data_01", "test_data_02", "test_corruptdata", "test_surftemp", "test_surfsal")

  ## find data in row - CHANGE datalist name if necessary
  data_list <- rbind(pacea::hotssea_data)   # TODO think would add bccm_full here
  data_row <- data_list[grep(layer, data_list[["data_name"]], ignore.case = TRUE), , drop = FALSE]
  if (nrow(data_row) != 1L && !(layer %in% test_names)) {
    stop(layer, " is not an available data object")
  }

  # look for data in pacea cache folder, return dataset
  cache_dir <- paste0(pacea::pacea_cache(),
                      "/",
                      cache_subfolder)
  file_list <- list.files(cache_dir)
  grep_list <- file_list[grep(layer, file_list, ignore.case = TRUE)]   # files
                                        # that contain 'layer' in filename (so
                                        # any version number, though not
                                        # worrying about versions yet)

  filename <- paste0(layer,
                     "_01.rds")
  # If versions then need local_filename and filename to be possibly
  # different
  local_filename <- filename
  local_file_dir <- paste0(cache_dir, "/", local_filename)

    # testthat data functions; end function here
  if (force == "testDataFunctions"){
    stop("testing data functions successful")
  }

  # if file already exists (but not worrying about versions yet, so length can
  #  only be 1)
  if (length(grep_list) > 0) {

    # local_filename <- grep_list[order(grep_list, decreasing = TRUE)][1]
    # read local data and check if corrupted
    # BCCM has readRDS, here need load as seemed to have saved slightly
    # differently. dat should automatically be the right name.
    suppressWarnings({dat <- try(load(local_file_dir), silent = TRUE)})
    if("try-error" %in% class(dat)){
      # nocov start    # start of lines to ignore when testing code coverage
      # delete previous version in local folder
      unlink(local_file_dir)

      stop("Local version of data is corrupt/incomplete, likely due to an interruption during download. Deleting corrupt file... Then run again")
      # nocov end
    }

    if (update) {

      ## internet errors for downloading
      if (!checkInternetConnection() || force == "testInternetError") {

        warning("No access to internet - could not check for updates.", call. = FALSE)

        dat <- load(local_file_dir)
        return(get(dat))
      }

      # list files from github repository
      #TODO Old:    # listurl <- "https://api.github.com/repos/pbs-assess/pacea-data/git/trees/main?recursive=1"
      #  listurl <- "https://zenodo.org/records/14019142"

      #      --
      # Ideally, want to get a list of filenames from Zenodo
      # OR just download the file if it's not there, assume version = 01, and
      # change that if we update things. Expect most people would just download
      # them all anyway, and that will come from a wrapper function
      # So not getting a list as not doing versions yet.

      #      req <- httr::GET(listurl)
      #      httr::stop_for_status(req)

      #      git_file_list <- unlist(lapply(content(req)$tree, "[", "path"), use.names = FALSE)
      #      git_file_dir <- git_file_list[grep(paste0("data/", layer), git_file_list, ignore.case = TRUE)]
      #      git_file_dir <- git_file_dir[order(git_file_dir, decreasing = TRUE)][1]
      #      git_filename <- strsplit(git_file_dir, "/")[[1]][2]
      #     --

      # compare versions
      # if(local_filename == git_filename) {

      # warning("Most recent version (_01) of data already downloaded in cache folder!", call. = FALSE)

      #   dat <- load(local_file_dir)
      # return(dat)

      # } else {
        # default ans = TRUE
      ans <- TRUE

        # interactive or forced download
      if(ask && !force){
        # nocov start
        # leave in although only version 01 for now
        ans <- ask(paste("Newer version of data available and previous version will be deleted from local cache folder:",
                         cache_dir, "Is that okay?", sep = "\n"))

        # testthat testing 'ans = FALSE' to deny updating data to cache
        if(layer == "test_data"){
          ans <- FALSE
        }
        # nocov end
      }

      if (!ans) {
        # nocov start
        warning("Returned local version of data.", call. = FALSE)

        dat <- load(local_file_dir)
        return(get(dat))
        # nocov end
      } else {

          # download data
        # turl <- paste0("https://github.com/pbs-assess/pacea-data/blob/main/data/", git_filename, "?raw=true")
          # dat <- dl_data(turl)

        zen4R::download_zenodo(doi = "10.5281/zenodo.14019141",
                               path = cache_dir,
                               files = paste0(data_row$data_name, "_01.rds"))

        # create file name with version number
        #  filename <- paste0(git_filename)
        #  file_dir <- paste0(cache_dir, "/", filename)

        #  saveRDS(dat, file = file_dir, compress = "xz")

          # delete previous version in local folder
        #  unlink(local_file_dir)

        message("Data successfully updated and downloaded to local cache folder!")

        dat <- load(local_file_dir)
        return(get(dat))
      }
    } else { # no update

      dat <- load(local_file_dir)
      return(get(dat))
    }

  } else { # if file doesn't exist at all

    ## internet errors for downloading
    if (!checkInternetConnection() || force == "testInternetError") stop("No access to internet", call. = FALSE)

    ## interactive ask to store in cache folder - from bcmaps package
    if (ask && !force) {
      # nocov start
      ans <- ask(paste("pacea would like to download and store these data in the directory:",
                       cache_dir, "Is that okay?", sep = "\n"))

      # testthat testing 'ans = FALSE' to deny downloading to cache
      if(layer == "test_data"){
        ans <- FALSE
      }

      if (!ans) stop("Exiting...", call. = FALSE)
      # nocov end
    }

    # check if directory exists
    if (!dir.exists(cache_dir)) {
      message("Creating directory to hold pacea data at \n", cache_dir)
      dir.create(cache_dir, recursive = TRUE)
    } else {
      message("Saving to pacea cache directory at \n", cache_dir)
    }

    # list files from github repository
    # listurl <- "https://api.github.com/repos/pbs-assess/pacea-data/git/trees/main?recursive=1"
    # req <- httr::GET(listurl)
    # httr::stop_for_status(req)

    # git_file_list <- unlist(lapply(content(req)$tree, "[", "path"), use.names = FALSE)
    # git_file_dir <- git_file_list[grep(paste0("data/", layer), git_file_list, ignore.case = TRUE)]
    # git_file_dir <- git_file_dir[order(git_file_dir, decreasing = TRUE)][1]
    # git_filename <- strsplit(git_file_dir, "/")[[1]][2]

    # download data
    # turl <- paste0("https://github.com/pbs-assess/pacea-data/blob/main/data/", git_filename, "?raw=true")
    # dat <- dl_data(turl)

    # create file name with version number
    # filename <- paste0(git_filename)
    # file_dir <- paste0(cache_dir, "/", filename)

    # saveRDS(dat, file = file_dir, compress = "xz")
    zen4R::download_zenodo(doi = "10.5281/zenodo.14019141",
                           path = cache_dir,
                           files = paste0(data_row$data_name, "_01.rds"))
    dat <- load(local_file_dir)   # seems to not be a true .rds file, should contain
                                  # the object 'layer'
    return(get(dat))
  }
}
