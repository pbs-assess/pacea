#' Load or download the HOTSSea model output
#'
#' Loading and/or downloading model output from HOTSsea physical hindcast of the
#' Salish Sea model. Loads it into your R environment from your local cache, and if
#' it has not been previously downloaded will first download it from Zenodo
#' into your local cache.
#' To download all HOTSsea model output in one go (recommended), first use the
#'   function [hotssea_all_variables()]. See the hotssea vignette for further explanations.
#'
#' The Hindcast of the Salish Sea (HOTSSea) is a physical ocean model that
#' recreates conditions throughout the Salish Sea from 1980 to 2018, filling in
#' the gaps in patchy measurements Oldford et al., (in review). The model
#' predicts physical ocean properties with sufficient accuracy to be useful for
#' a variety of applications. It corroborates observed ocean temperature trends
#' and can examine areas with few observations. Results indicate that some
#' seasons and areas are warming faster than others. HOTSSea was developed using
#' the Nucleus for European Modelling of the Ocean (NEMO)
#' engine, and temperature and salinty outputs are included in pacea (from version
#'  1 of HOTSSea).
#'
#' The HOTSSea outputs in pacea are adapted from results
#' provided by Greig Oldford (Fisheries and Oceans Canada). For model details see
#' Oldford et al., (in review), which should
#' be read and cited if you use the results, and Greig and Andrew Edwards
#' should be consulted for any clarifications and uses of the results.
#'
#' The following 40 functions serve to download specific individual ocean
#' calculations related to temperature or salinity from the HOTSSea model. If a
#' variable has already been downloaded, the function will load data from the
#' `hotssea` subfolder in the user's
#' `pacea_cache()` folder into user's local R environment. Each file is around
#' 3-9 Mb, but should not take long to download.
##'
#' The 40 variables available are desribed below as their respective
#' functions. The variables are listed in the object `hotssea_data`.
#' The variables are stored on Zenodo, with [get_zenodo_data()]
#' automatically taking care of the DOI address for the one-time download.
#'#'
#' #'
#' \describe{
#'   \code{hotssea_*depth*_temperature()_*statistic*} Temperature in oC\cr
#'   \code{hotssea_*depth*_salinity()} Salinity in PSU\cr
#' }
#'
#' NOTE:\cr
#' \code{*depth*} must be replaced by one of the following depth categories:
#'
#' \describe{
#'   \code{surface}{sea surface}\cr
#'   \code{avg0to30m}{average between 0m and 30m depth}\cr
#'   \code{avg30to150m}{average between 30m and 150m depth}\cr
#'   \code{150toBot} {average between 150m depth and sea bottom}\cr
#'   \code{bottom}{sea bottom}
#' }
#'
#' #' \code{*statistics*} must be replaced by one of the following statistics:
#'TODO HERE
#' \describe{
#'   \code{min}{sea bottom}\cr
#'   \code{mean}{average between 0m and 40m depth}\cr
#'   \code{max}{average between 40m and 100m depth}\cr
#'   \code{std} {:  average between 100m depth and sea bottom}\cr
#' }
#'
#' The five permissable depth values for four types of statistics for both
#' temperature and salinity yield the 40 different combinations.
#' For example, `hotssea_avg0to30m_temperature_max()` refers to the mean (over
#' the top 30 m) of each modelled depths' maximum (over the month) daily mean
#' temperature. See the hotssea vignette for a more detailed explanation of these
#' calculations.
#'
#' @format A simple features dataframe.
#'
#' @param update Logical. Would you like to check for a newer version of the layer?
#' @param ask Logical. Should the user be asked before downloading the data to local cache? Defaults to the value of interactive().
#' @param force Logical. Should download of data be forced? Overrides `ask` argument if TRUE.
#' @param version character Version number of data as saved on Zenodo. Only
#'   currently works for "01" as that's all that is currently available.
#' @param cache_subfolder character. Subfolder to put or look for the objects;
#'   defaults to `hotssea` and it is best to stick with this. The objects will
#'   be put in your `paste0(pacea::pacea_cache(), cache_subfolder)` directory.
#' @return `sf` data object requested.
#' @export
#'
#' @source Peña, M.A., Fine, I. and Callendar, W. 2019. Interannual variability in primary production and shelf-offshore transport of nutrients along the northeast Pacific Ocean margin. Deep-Sea Research II, doi:10.1016/j.dsr2.2019.104637.
#'
#' @examples
#' \dontrun{
#' my_data <- hotssea_bottom_oxygen()
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
hotssea_avg150toBot_salinity_min <- function(update = FALSE, ask = interactive(),
                                              force = FALSE, version = "01",
                                              cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_avg150toBot_salinity_min", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_avg150toBot_salinity_mean <- function(update = FALSE, ask = interactive(),
                                               force = FALSE, version = "01",
                                               cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_avg150toBot_salinity_mean", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_avg150toBot_salinity_max <- function(update = FALSE, ask = interactive(),
                                              force = FALSE, version = "01",
                                              cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_avg150toBot_salinity_max", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_avg150toBot_salinity_std <- function(update = FALSE, ask = interactive(),
                                              force = FALSE, version = "01",
                                              cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_avg150toBot_salinity_std", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_avg150toBot_temperature_min <- function(update = FALSE, ask = interactive(),
                                                 force = FALSE, version = "01",
                                                 cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_avg150toBot_temperature_min", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_avg150toBot_temperature_mean <- function(update = FALSE, ask = interactive(),
                                                  force = FALSE, version = "01",
                                                  cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_avg150toBot_temperature_mean", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_avg150toBot_temperature_max <- function(update = FALSE, ask = interactive(),
                                                 force = FALSE, version = "01",
                                                 cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_avg150toBot_temperature_max", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname hotssea_surface_salinity_min
#' @export
hotssea_avg150toBot_temperature_std <- function(update = FALSE, ask = interactive(),
                                                 force = FALSE, version = "01",
                                                 cache_subfolder = "hotssea"){
  get_zenodo_data("hotssea_avg150toBot_temperature_std", update = update, ask = ask,
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
