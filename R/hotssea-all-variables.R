#' Download all 40 HOTSSea model results to local drive.
#'
#' Type `hotssea_data` to view the full list of available variables. See help
#' files for specific variables for more details on HOTSSea model results, e.g.
#' [?hotssea_bottom_temperature_max].
#'
#' CAUTION: The HOTSSea files are large (3-9 Mb) and downloading all 40 may take
#' a while. TODO 200km versions (20kb ish) took 2 minutes; full versions are about 400 times
#' bigger, so might take around 800 minutes, i.e. 13 hours. Either leave running
#' overnight, or just download the files you definitely need, see
#' [?hotssea_bottom_temperature_max]. But this is a one-time exercise and then
#' you will have the files locally for all eternity. An alternative if you are at
#' the Pacific Biological Station is to get Andy to copy the files to an
#' external drive.
#'
#' @param variables character Either `c("temperature", "salinity")`,
#'   `"temperature"`, or `"salinity"`, describing what variables to
#'   download. Given the potentially long download times, some people may just
#'   want temperature or salinity only.
#' @param run_parallel logical Run the downloads in parallel using
#'   `parallel::foreach()`.
#'
#' @return downloaded files to `paste0(pacea_cache(), "/hotssea")` directory
#' @author Andrew Edwards and Travis Tai
#' @export
#' @examples
#' \dontrun{
#' hotssea_all_variables()
#' }
hotssea_all_variables <- function(variables = c("temperature", "salinity"),
                                  run_parallel = TRUE) {

  cache_dir <- paste0(pacea::pacea_cache(),
                      "/hotssea")
  hotssea_datalist <- pacea::hotssea_data

  ans <- ask(paste("Downloading all 40 HOTSsea files (3-9 Mb each)  may take many hours. TODO update with CAUTION in help. Files will be downloaded to  directory:",
                   cache_dir,
                   "If you get errors regarding corrupt files then empty the above directory, though this should be fixed now. Would you like to continue?", sep = "\n"))

  # calling scope - testthat detection; set ans = T for testthat
  tb <- .traceback(x = 0)
  if(any(unlist(lapply(tb, function(x) any(grepl("test_env", x)))))){
    #ans <- TRUE
    hotssea_datalist <- data.frame(data_name = c("test_data_01"))
  }

  if (!ans) stop("Exiting...", call. = FALSE)   # nocov

  if(.Platform$OS.type != "windows" & run_parallel){
    warning(paste0("The parallel code in `hotssea_all_variables()` has not been fully tested on non-Windows machines. If it does not work (you don't see any files being downloaded into ",
                   cache_dir,
                   " after a 'few' hours, then retry with `run_parallel = FALSE`"))
  }

  if(run_parallel){
    # Run loop in parallel
    # Set up parallel cluster (from
    # https://www.blasbenito.com/post/02_parallelizing_loops_with_r/), and done
    # in data-raw/hotssea/hotssea-data-interpolation.R

    num_cores <- parallel::detectCores() - 2
    # Create the cluster
    my_cluster <- parallel::makeCluster(
                              num_cores,
                              type = "PSOCK")

    # Register it to be used by %dopar%
    doParallel::registerDoParallel(cl = my_cluster)

    foreach::foreach(i = 1:nrow(hotssea_datalist)) %dopar% {

      data_name <- hotssea_datalist[i, 1]
      pacea::get_zenodo_data(data_name,
                             force = TRUE,
                             cache_subfolder = "hotssea")
    }

    parallel::stopCluster(cl = my_cluster)

  } else {
    # Run in series
    for(i in 1:nrow(hotssea_datalist)){
      data_name <- hotssea_datalist[i, 1]
      get_zenodo_data(data_name,
                      force = TRUE,
                      cache_subfolder = "hotssea")
    }
  }

  return(print("Download of all HOTSSea files: successful!"))
}
