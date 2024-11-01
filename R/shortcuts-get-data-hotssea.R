#' HOTSSea model output
#'
#' @description
#' Loading and/or downloading the HOTSsea physical hindcast of the Salish Sea
#'   model output to local drive.
#'
#' To download all HOTSsea data, use the function `hotssea_all_variables()`. See help page for details (`?hotssea_all_variables`).
#'
#' @details
#'
#' TODO edit all this for hotssea
#'
#' TODO The following functions serve to download specific individual ocean variables of the BCCM model data. Use `bccm_all_variables()` if you would like to download all variables - see help file `?bccm_all_variables` for details. If a variable has already been downloaded, the function will load data from `pacea_cache()` folder into user's local R environment.
#'
#' TODO The British Columbia continental margin (BCCM) model is an ocean circulation-biogeochemical model implementation of the regional ocean modelling system (ROMS). It has a horizontal resolution of 3km x 3km and a vertical discretization based on bathymetry of 42 depth levels increasing in resolution near the surface. These modelled output data were provided by Angelica Pena and the data is further detailed in Pena et al. (2019).
#'
#' TODOBCCM data were interpolated to a 2km x 2km inshore grid and a 6km x 6km offshore grid. Data provided are monthly means that span from 1993-2019. Each column represents a distinct 'year_month' combination (e.g. 2010_4).
#'
#'
#' \describe{
#'   \code{bccm_*depth*_oxygen()} Dissolved oxygen measured as mmol-oxygen m^-3\cr
#'   \code{bccm_*depth*_salinity()} Salinity in ppt\cr
#'   \code{bccm_*depth*_temperature()} Temperature in oC\cr
#'   \code{bccm_*depth*_ph()} pH\cr
#'   \code{bccm_phytoplankton()} Total phytoplankton biomass in mmol-nitrogen m^-2\cr
#'   \code{bccm_primaryproduction()} Total primary production in gC m^-2 d^-1
#' }
#'
#' NOTE:\cr
#' \code{*depth*} must be replaced by one of the following depth categories:
#'
#' \describe{
#'   \code{bottom}{sea bottom}\cr
#'   \code{0to40}{average between 0m and 40m depth}\cr
#'   \code{40to100}{average between 40m and 100m depth}\cr
#'   \code{100tobot} {:  average between 100m depth and sea bottom}\cr
#'   \code{surface}{sea surface}
#' }
#'
#' @format A simple features dataframe.
#'
#' @param update Logical. Would you like to check for a newer version of the layer?
#' @param ask Logical. Should the user be asked before downloading the data to local cache? Defaults to the value of interactive().
#' @param force Logical. Should download of data be forced? Overrides `ask` argument if TRUE.
#'
#' @return `sf` data object requested.
#' @export
#'
#' @source Peña, M.A., Fine, I. and Callendar, W. 2019. Interannual variability in primary production and shelf-offshore transport of nutrients along the northeast Pacific Ocean margin. Deep-Sea Research II, doi:10.1016/j.dsr2.2019.104637.
#'
#' @examples
#' \dontrun{
#' my_data <- bccm_bottom_oxygen()
#' }
hotssea_surface_salinity_min <- function(update = FALSE, ask = interactive(),
                                         force = FALSE, version = "01",
                                         cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_surface_salinity_min", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_surface_salinity_mean <- function(update = FALSE, ask = interactive(),
                                          force = FALSE, version = "01",
                                          cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_surface_salinity_mean", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_surface_salinity_max <- function(update = FALSE, ask = interactive(),
                                         force = FALSE, version = "01",
                                         cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_surface_salinity_max", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_surface_salinity_std <- function(update = FALSE, ask = interactive(),
                                         force = FALSE, version = "01",
                                         cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_surface_salinity_std", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_surface_temperature_min <- function(update = FALSE, ask = interactive(),
                                            force = FALSE, version = "01",
                                            cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_surface_temperature_min", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_surface_temperature_mean <- function(update = FALSE, ask = interactive(),
                                             force = FALSE, version = "01",
                                             cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_surface_temperature_mean", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_surface_temperature_max <- function(update = FALSE, ask = interactive(),
                                            force = FALSE, version = "01",
                                            cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_surface_temperature_max", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_surface_temperature_std <- function(update = FALSE, ask = interactive(),
                                            force = FALSE, version = "01",
                                            cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_surface_temperature_std", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_avg0to30m_salinity_min <- function(update = FALSE, ask = interactive(),
                                           force = FALSE, version = "01",
                                           cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_avg0to30m_salinity_min", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_avg0to30m_salinity_mean <- function(update = FALSE, ask = interactive(),
                                            force = FALSE, version = "01",
                                            cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_avg0to30m_salinity_mean", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_avg0to30m_salinity_max <- function(update = FALSE, ask = interactive(),
                                           force = FALSE, version = "01",
                                           cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_avg0to30m_salinity_max", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_avg0to30m_salinity_std <- function(update = FALSE, ask = interactive(),
                                           force = FALSE, version = "01",
                                           cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_avg0to30m_salinity_std", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_avg0to30m_temperature_min <- function(update = FALSE, ask = interactive(),
                                              force = FALSE, version = "01",
                                              cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_avg0to30m_temperature_min", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_avg0to30m_temperature_mean <- function(update = FALSE, ask = interactive(),
                                               force = FALSE, version = "01",
                                               cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_avg0to30m_temperature_mean", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_avg0to30m_temperature_max <- function(update = FALSE, ask = interactive(),
                                              force = FALSE, version = "01",
                                              cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_avg0to30m_temperature_max", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_avg0to30m_temperature_std <- function(update = FALSE, ask = interactive(),
                                              force = FALSE, version = "01",
                                              cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_avg0to30m_temperature_std", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_avg30to150m_salinity_min <- function(update = FALSE, ask = interactive(),
                                             force = FALSE, version = "01",
                                             cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_avg30to150m_salinity_min", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_avg30to150m_salinity_mean <- function(update = FALSE, ask = interactive(),
                                              force = FALSE, version = "01",
                                              cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_avg30to150m_salinity_mean", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_avg30to150m_salinity_max <- function(update = FALSE, ask = interactive(),
                                             force = FALSE, version = "01",
                                             cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_avg30to150m_salinity_max", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_avg30to150m_salinity_std <- function(update = FALSE, ask = interactive(),
                                             force = FALSE, version = "01",
                                             cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_avg30to150m_salinity_std", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_avg30to150m_temperature_min <- function(update = FALSE, ask = interactive(),
                                                force = FALSE, version = "01",
                                                cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_avg30to150m_temperature_min", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_avg30to150m_temperature_mean <- function(update = FALSE, ask = interactive(),
                                                 force = FALSE, version = "01",
                                                 cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_avg30to150m_temperature_mean", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_avg30to150m_temperature_max <- function(update = FALSE, ask = interactive(),
                                                force = FALSE, version = "01",
                                                cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_avg30to150m_temperature_max", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_avg30to150m_temperature_std <- function(update = FALSE, ask = interactive(),
                                                force = FALSE, version = "01",
                                                cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_avg30to150m_temperature_std", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_avg150mtoBot_salinity_min <- function(update = FALSE, ask = interactive(),
                                              force = FALSE, version = "01",
                                              cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_avg150mtoBot_salinity_min", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_avg150mtoBot_salinity_mean <- function(update = FALSE, ask = interactive(),
                                               force = FALSE, version = "01",
                                               cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_avg150mtoBot_salinity_mean", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_avg150mtoBot_salinity_max <- function(update = FALSE, ask = interactive(),
                                              force = FALSE, version = "01",
                                              cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_avg150mtoBot_salinity_max", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_avg150mtoBot_salinity_std <- function(update = FALSE, ask = interactive(),
                                              force = FALSE, version = "01",
                                              cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_avg150mtoBot_salinity_std", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_avg150mtoBot_temperature_min <- function(update = FALSE, ask = interactive(),
                                                 force = FALSE, version = "01",
                                                 cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_avg150mtoBot_temperature_min", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_avg150mtoBot_temperature_mean <- function(update = FALSE, ask = interactive(),
                                                  force = FALSE, version = "01",
                                                  cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_avg150mtoBot_temperature_mean", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_avg150mtoBot_temperature_max <- function(update = FALSE, ask = interactive(),
                                                 force = FALSE, version = "01",
                                                 cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_avg150mtoBot_temperature_max", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_avg150mtoBot_temperature_std <- function(update = FALSE, ask = interactive(),
                                                 force = FALSE, version = "01",
                                                 cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_avg150mtoBot_temperature_std", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_bottom_salinity_min <- function(update = FALSE, ask = interactive(),
                                        force = FALSE, version = "01",
                                        cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_bottom_salinity_min", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_bottom_salinity_mean <- function(update = FALSE, ask = interactive(),
                                         force = FALSE, version = "01",
                                         cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_bottom_salinity_mean", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_bottom_salinity_max <- function(update = FALSE, ask = interactive(),
                                        force = FALSE, version = "01",
                                        cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_bottom_salinity_max", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_bottom_salinity_std <- function(update = FALSE, ask = interactive(),
                                        force = FALSE, version = "01",
                                        cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_bottom_salinity_std", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_bottom_temperature_min <- function(update = FALSE, ask = interactive(),
                                           force = FALSE, version = "01",
                                           cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_bottom_temperature_min", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_bottom_temperature_mean <- function(update = FALSE, ask = interactive(),
                                            force = FALSE, version = "01",
                                            cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_bottom_temperature_mean", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_bottom_temperature_max <- function(update = FALSE, ask = interactive(),
                                           force = FALSE, version = "01",
                                           cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_bottom_temperature_max", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_bottom_temperature_std <- function(update = FALSE, ask = interactive(),
                                           force = FALSE, version = "01",
                                           cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_bottom_temperature_std", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}


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
#' you will have the files locally for eternity. An alternative if you are at
#' the Pacific Biological Station is to get Andy to copy the files to an
#' external drive.
#'
#' @return downloaded files to `paste0(pacea_cache(), "/hotssea")` directory
#' @export
#' @examples
#' \dontrun{
#' hotssea_all_variables()
#' }
hotssea_all_variables <- function() {

  cache_dir <- paste0(pacea::pacea_cache(),
                      "/hotssea")
  hotssea_datalist <- pacea::hotssea_data

  ans <-
                   ask(paste("Downloading all 40 HOTSsea files (3-9 Mb each)  may take many hours. TODO update with CAUTION in help. Files will be downloaded to  directory:",
                   cache_dir,
                   "If you get errors regarding corrupt files then empty the above directory. Would you like to continue?", sep = "\n"))

  # calling scope - testthat detection; set ans = T for testthat
  tb <- .traceback(x = 0)
  if(any(unlist(lapply(tb, function(x) any(grepl("test_env", x)))))){
    #ans <- TRUE
    hotssea_datalist <- data.frame(data_name = c("test_data_01"))
  }

  if (!ans) stop("Exiting...", call. = FALSE)   # nocov

  for(i in 1:nrow(hotssea_datalist)){
    data_name <- hotssea_datalist[i, 1]
    get_zenodo_data(data_name,
                    force = TRUE,
                    cache_subfolder = "hotssea")
  }

  return(print("Download of all HOTSSea files: successful!"))
}
