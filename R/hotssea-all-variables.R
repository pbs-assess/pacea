#' Download all 40 HOTSSea model results from Zenodo to a local drive.
#'
#' This downloads (in parallel by default) all 40 HOTSSea model results from
#' https://zenodo.org/records/14027261 (as specified in [get_zenodo_data()] to
#' your local folder given by
#' `paste0(pacea_cache(), "/hotssea")`. Each file is around 3-9Mb, and so downloading them all may
#' take a few minutes or possibly longer. You can check your cache
#' folder for progress to see them appearing there. The download is done in
#' parallel (by default). Speed depends on network speed and number of cores.
#'
#' You can or set `variables` to "temperature" or
#' "salinity" if you only want one of those types of model output. Or you can just download the individual files you definitely need, see
#' [?hotssea_bottom_temperature_max]. Remember the downloading is a one-time
#' exercise (hence it's desirable to get all files at once) and then
#' you will have the files locally for all eternity (ish).
#'
#' If this function fails, you can try rerunning it; it will not re-download any
#' that were successful and this worked for someone testing it.
#' Else you could then also try `hotssea_all_variables(run_parallel = FALSE)`, or typing `options(timeout = 1200)`
#' (or higher) especially if running from the Pacific Biological Station. This
#' function is particularly hard to test independently under different scenarios. An alternative if you are at
#' the Pacific Biological Station is the 1990s solution of getting Andy to copy
#' the files to an external drive for you.
#'
#' Or you can just download them manually (download all) from
#' the Zenodo site given above) and put them in `paste0(pacea_cache(), "/hotssea")`.
#'
#' Type `hotssea_data` to view the full list of available variables. See help
#' files for specific variables for more details on HOTSSea model results, e.g.
#' [?hotssea_bottom_temperature_max].
#'
#' Also see the hotssea vignette.
#'
#' @param variables character Either `c("temperature", "salinity")`,
#'   `"temperature"`, or `"salinity"`, describing what variables to
#'   download. Given the potentially long download times, some people may just
#'   want temperature or salinity only.
#' @param run_parallel logical Run the downloads in parallel using
#'   `parallel::foreach()`.
#' @param timeout_value numeric Timeout (seconds) for downloading a single file from
#'   the internet. Gets used by [zen4R::download_zenodo()] via
#'   [get_zenodo_data()]. Try increasing if get an Error saying Timeout has been
#'   reached. If it says `Timeout of 60 seconds` then `timeout_value` is not
#'   getting used (the default is 4 hours). Maybe try the manual download option above.
#'
#' @return downloaded files to `paste0(pacea_cache(), "/hotssea")`
#'   directory.
#' @author Andrew Edwards and Travis Tai
#' @export
#'
#' @source Oldford, G.L., Jarníková, T., Christensen, V., and Dunphy, M. (in review).
#' HOTSSea v1: a NEMO-based physical Hindcast of the Salish Sea (1980–2018)
#' supporting ecosystem model development. Preprint. https://doi.org/10.5194/gmd-2024-58
#' @examples
#' \dontrun{
#' hotssea_all_variables()
#' plot(hotssea_avg150toBot_temperature_min())
#' }
hotssea_all_variables <- function(variables = c("temperature", "salinity"),
                                  run_parallel = TRUE,
                                  timeout_value = 14400) {

  stopifnot(variables %in% c("temperature", "salinity") |
            variables == c("temperature", "salinity"))

  cache_dir <- paste0(pacea::pacea_cache(),
                      "/hotssea")

  hotssea_data_vec <- pacea::hotssea_data$data_name  # Can get reduced in next loop


  # Just select temperature or salinity if specified
  if(length(variables) == 1){
    if(variables == "temperature"){
      hotssea_data_vec <- hotssea_data_vec[grep("temperature",
                                                hotssea_data_vec)]
    } else {
      hotssea_data_vec <- hotssea_data_vec[grep("salinity",
                                                hotssea_data_vec)]
    }
  }

  ans <-
                   ask(paste("Downloading all 40 HOTSsea files (3-9 Mb each) may take a few minutes (though it is hard to estimate). Please read the help file `?hotssea_all_variables` for faster options or solutions if this fails. Files will be downloaded to  directory:",
                   cache_dir,
                   "Would you like to continue?", sep = "\n"))

  # calling scope - testthat detection; set ans = T for testthat
  tb <- .traceback(x = 0)
  if(any(unlist(lapply(tb, function(x) any(grepl("test_env", x)))))){
    #ans <- TRUE
    hotssea_data_vec <- "test_data_01"
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

    foreach::foreach(i = 1:length(hotssea_data_vec)) %dopar% {

      data_name <- hotssea_data_vec[i]
      pacea::get_zenodo_data(data_name,
                             force = TRUE,
                             cache_subfolder = "hotssea",
                             timeout_value = timeout_value)
    }

    parallel::stopCluster(cl = my_cluster)

  } else {
    # Run in series
    for(i in 1:length(hotssea_data_vec)){
      data_name <- hotssea_data_vec[i]
      get_zenodo_data(data_name,
                      force = TRUE,
                      cache_subfolder = "hotssea",
                      timeout_value = timeout_value)
    }
  }

  return(print("Download of all HOTSSea files: successful!"))
}
