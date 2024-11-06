#' Download all 22 BCCM full model results from Zenodo to a local drive.
#'
#' This downloads (in parallel by default) all 22 BCCM full model results from
#' https://zenodo.org/records/14031460 to your local cache directory given by
#' `paste0(pacea_cache(), "/bccm_full")`. Each file is around 120 Mb, and only
#' took about two minutes each on a home network (and since was running on 6
#' parallel cores, it all took less than 10 minutes. Any files that are already
#' present in your cache directory are not re-downloaded. You can look at your
#' cache directory to see progress (files should gradually appear).
#'
#' The download is done in parallel (by default). Speed depends on network speed
#' and number of cores.
#'
#' You can download individual files, see [bccm_bottom_oxygen_full()], but
#' since the downloading is a one-time exercise it's likely desirable to just get
#' them all at once, providing you have the disk space (around 2.4 Gb).
#'
#' If you get an error saying `Windows Defender Firewall has blocked some features of
#'   this app` when running from RStudio, try running from just R. You may as
#'   well try downloading all variables in one go with
#'   [bccm_all_variables_full()], then you will be fine as they will all then be
#'   cached on your machine. You can also try, in the
#'   command line in RStudio, the command
#'   `system2("R -e 'library(pacea); bccm_all_variables_full()'")`. If that does
#'   not work you will have to just run R and do `library(pacea);
#'   bccm_all_variables_full()`.
#'
#' If this function fails with a different error, you can try rerunning it; it will not re-download any
#' that were successful and this worked for someone testing it.
#' Else you could then also try `hotssea_all_variables(run_parallel = FALSE)`,
#' or increasing the `timeout` option even higher, especially if running from
#' the Pacific Biological Station. This
#' function is particularly hard to test independently under different
#' scenarios.
#'
#' Or you can always just download them manually (download all) from
#' the Zenodo site given above) and put them in `paste0(pacea_cache(), "/bccm_full")`.
#'
#' Type `bccm_data` to view the full list of available variables. See help
#' files for specific variables for more details on BCCM model results, e.g.
#' [bccm_bottom_temperature_full()].
#'
#' Also see the two BCCM vignettes.
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
bccm_all_variables_full <- function(run_parallel = TRUE,
                                    timeout_value = 14400) {

  cache_dir <- paste0(pacea::pacea_cache(),
                      "/bccm_full")

  bccm_full_data_vec <- pacea::bccm_data_full$data_name  # Can get reduced in next loop

  ans <- ask(paste("Downloading all 22 bccm_full files (around 120 Mb each) may take a TODO few minutes (though it is hard to estimate). Please read the help file `?bccm_all_variables_full` for faster options or solutions if this fails. Files will be downloaded to  directory:",
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
                   " after say 20 minutes, then retry with `run_parallel = FALSE`, and see the help."))
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
