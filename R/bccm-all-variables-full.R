#' Download all 22 BCCM full model results from Zenodo to a local drive.
#'
#' This downloads (in parallel by default) all 22 BCCM full model results from
#' https://zenodo.org/records/14031460 to your local folder given by
#' `paste0(pacea_cache(), "/bccm_full")`. Each file is around 120 Mb, though
#'
#' # TODO carry on from here. Might only be slow on DFO network.3-9Mb, and so downloading them all may
#' take a few minutes or possibly longer. You can check your cache
#' folder for progress to see them appearing there. The download is done in
#' parallel (by default). Speed depends on network speed and number of cores.
#'
#' TODO Either leave running overnight, or set `variables` to "temperature" or
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
#' the Pacific Biological Station is to get Andy to copy the files to an
#' external drive.
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
#' @param variables character TODO Either `c("temperature", "salinity")`,
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
#' @return downloaded files to `paste0(pacea_cache(), "/bccm_full")`
#'   directory.
#' @author Andrew Edwards and Travis Tai
#' @export
#' @examples
#' \dontrun{
#' bccm_all_variables_full()
#' plot(bccm_avg100mtoBot_temperature_full())
#' }
bccm_all_variables_full <- function(variables = c("temperature", "salinity"),
                                    run_parallel = TRUE,
                                    timeout_value = 14400) {

  # TODO maybe not needed if speed is fast
  stopifnot(variables %in% c("temperature", "salinity") |
            variables == c("temperature", "salinity"))

  cache_dir <- paste0(pacea::pacea_cache(),
                      "/bccm_full")

  bccm_full_data_vec <- pacea::bccm_data_full$data_name  # Can get reduced in next loop

  # TODO remove if remove above TODO. Just select temperature or salinity if specified
  ## if(length(variables) == 1){
  ##   if(variables == "temperature"){
  ##     bccm_full_data_vec <- bccm_full_data_vec[grep("temperature",
  ##                                               bccm_full_data_vec)]
  ##   } else {
  ##     bccm_full_data_vec <- bccm_full_data_vec[grep("salinity",
  ##                                               bccm_full_data_vec)]
  ##   }
  ## }

  ans <-
                   ask(paste("Downloading all 22 bccm_full files (around 120 Mb each) may take a TODO few minutes (though it is hard to estimate). Please read the help file `?bccm_all_variables_full` for faster options or solutions if this fails. Files will be downloaded to  directory:",
                   cache_dir,
                   "Would you like to continue?", sep = "\n"))

  # calling scope - testthat detection; set ans = T for testthat
  tb <- .traceback(x = 0)
  if(any(unlist(lapply(tb, function(x) any(grepl("test_env", x)))))){
    #ans <- TRUE
    bccm_full_data_vec <- "test_data_01"
  }

  if (!ans) stop("Exiting...", call. = FALSE)   # nocov

  if(.Platform$OS.type != "windows" & run_parallel){
    warning(paste0("The parallel code in `bccm_all_variables_full()` has not been fully tested on non-Windows machines. If it does not work (you don't see any files being downloaded into ",
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
    my_cluster <- parallel::makeCluster(num_cores,
                                        type = "PSOCK")

    # Register it to be used by %dopar%
    doParallel::registerDoParallel(cl = my_cluster)

    foreach::foreach(i = 1:length(bccm_full_data_vec)) %dopar% {
      data_name <- bccm_full_data_vec[i]
      pacea::get_zenodo_data(data_name,
                             force = TRUE,
                             cache_subfolder = "bccm_full",
                             timeout_value = timeout_value)
    }

    parallel::stopCluster(cl = my_cluster)

  } else {
    # Run in series
    for(i in 1:length(bccm_full_data_vec)){
      data_name <- bccm_full_data_vec[i]
      get_zenodo_data(data_name,
                      force = TRUE,
                      cache_subfolder = "bccm_full",
                      timeout_value = timeout_value)
    }
  }

  return(print("Download of all bccm_full files: successful!"))
}
