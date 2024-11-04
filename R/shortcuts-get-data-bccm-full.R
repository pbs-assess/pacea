#' BCCM model output for the full domain of the model (not just within Canada's
#' Exclusive Economic Zone).
#'
#' @description
#' Loading and/or downloading British Columbia Continental Margin (BCCM) model
#'   outputs for the full domain of the model (down into US waters) to local
#'   drive. See the functions without `_full` get only the values in Canada's Exclusive
#'   Economic Zone. See the two BCCM vignettes.
#'
#' To download all BCCM data for the full domain, use the function
#'   [bccm_all_variables_full()], and see that help for further details about
#'   the model.
#'
#' @details
#'
#' The following 22 functions serve to download specific individual ocean variables
#'  of the BCCM model data over its full domain, on a 2 km x 2 km grid. Each
#'  file is around 120 Mb, but only takes a couple of minutes (on a home
#'  network, may be slower from a work network because of firewalls). If a
#'  variable has already been downloaded, the function will load data from
#'  your `paste0(pacea_cache(), "/bccm_full"` folder into your local R environment.
#'
#' The 22 objects available are desribed below as their respective functions;
#'   the equivalent functions without `_full` are for the BCCM outputs
#'   restricted to Canada's EEZ and on a coarser grid for the offshore. The
#'   objects obtained here are stored on Zenodo, with [get_zenodo_data()]
#'   automatically taking care of the DOI address.
#'
#' \describe{
#'   \code{bccm_*depth*_oxygen_full()} Dissolved oxygen measured as mmol-oxygen m^-3\cr
#'   \code{bccm_*depth*_salinity_full()} Salinity in ppt\cr
#'   \code{bccm_*depth*_temperature_full()} Temperature in oC\cr
#'   \code{bccm_*depth*_ph_full()} pH\cr
#'   \code{bccm_phytoplankton_full()} Total phytoplankton biomass in mmol-nitrogen m^-2\cr
#'   \code{bccm_primaryproduction_full()} Total primary production in gC m^-2 d^-1
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
#' @inheritParams hotssea_surface_salinity_min
#'
#' @return `sf` data object requested.
#' @export
#'
#' @examples
#' \dontrun{
#' b <- bccm_bottom_temperature_full()
#' plot(b, months = 1:12)
#'
#' plot(bccm_primaryproduction_full(), months = 1:12)
#' }
bccm_bottom_oxygen_full <- function(update = FALSE, ask = interactive(),
                                    force = FALSE, version = "01",
                                    cache_subfolder = "bccm_full"){
  get_zenodo_data("bccm_bottom_oxygen_full", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname bccm_bottom_oxygen_full
#' @export
bccm_bottom_ph_full <- function(update = FALSE, ask = interactive(),
                                force = FALSE, version = "01",
                                cache_subfolder = "bccm_full"){
  get_zenodo_data("bccm_bottom_pH_full", update = update, ask = ask,
                  force = force, version = version,
                  cache_subfolder = cache_subfolder)
}

#' @rdname bccm_bottom_oxygen_full
#' @export
bccm_bottom_salinity_full <- function(update = FALSE, ask = interactive(),
                                      force = FALSE, version = "01",
                                      cache_subfolder = "bccm_full"){
  get_zenodo_data("bccm_bottom_salinity_full", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname bccm_bottom_oxygen_full
#' @export
bccm_bottom_temperature_full <- function(update = FALSE, ask = interactive(),
                                         force = FALSE, version = "01",
                                         cache_subfolder = "bccm_full"){
  get_zenodo_data("bccm_bottom_temperature_full", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname bccm_bottom_oxygen_full
#'
#' @export
bccm_surface_oxygen_full <- function(update = FALSE, ask = interactive(),
                                     force = FALSE, version = "01",
                                    cache_subfolder = "bccm_full"){

  get_zenodo_data("bccm_surface_oxygen_full", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname bccm_bottom_oxygen_full
#' @export
bccm_surface_ph_full <- function(update = FALSE, ask = interactive(),
                                 force = FALSE, version = "01",
                                 cache_subfolder = "bccm_full"){
  get_zenodo_data("bccm_surface_pH_full", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname bccm_bottom_oxygen_full
#' @export
bccm_surface_salinity_full <- function(update = FALSE, ask = interactive(),
                                       force = FALSE, version = "01",
                                       cache_subfolder = "bccm_full"){
  get_zenodo_data("bccm_surface_salinity_full", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname bccm_bottom_oxygen_full
#' @export
bccm_surface_temperature_full <- function(update = FALSE, ask = interactive(),
                                          force = FALSE, version = "01",
                                          cache_subfolder = "bccm_full"){
  get_zenodo_data("bccm_surface_temperature_full", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname bccm_bottom_oxygen_full
#' @export
bccm_avg0to40m_oxygen_full <- function(update = FALSE, ask = interactive(),
                                       force = FALSE, version = "01",
                                       cache_subfolder = "bccm_full"){
  get_zenodo_data("bccm_avg0to40m_oxygen_full", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname bccm_bottom_oxygen_full
#' @export
bccm_avg0to40m_ph_full <- function(update = FALSE, ask = interactive(),
                                   force = FALSE, version = "01",
                                   cache_subfolder = "bccm_full"){
  get_zenodo_data("bccm_avg0to40m_ph_full", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname bccm_bottom_oxygen_full
#' @export
bccm_avg0to40m_salinity_full <- function(update = FALSE, ask = interactive(),
                                         force = FALSE, version = "01",
                                         cache_subfolder = "bccm_full"){
  get_zenodo_data("bccm_avg0to40m_salinity_full", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname bccm_bottom_oxygen_full
#' @export
bccm_avg0to40m_temperature_full <- function(update = FALSE, ask = interactive(),
                                            force = FALSE, version = "01",
                                            cache_subfolder = "bccm_full"){
  get_zenodo_data("bccm_avg0to40m_temperature_full", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname bccm_bottom_oxygen_full
#' @export
bccm_avg40to100m_oxygen_full <- function(update = FALSE, ask = interactive(),
                                         force = FALSE, version = "01",
                                         cache_subfolder = "bccm_full"){
  get_zenodo_data("bccm_avg40to100m_oxygen_full", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname bccm_bottom_oxygen_full
#' @export
bccm_avg40to100m_ph_full <- function(update = FALSE, ask = interactive(),
                                     force = FALSE, version = "01",
                                     cache_subfolder = "bccm_full"){
  get_zenodo_data("bccm_avg40to100m_ph_full", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname bccm_bottom_oxygen_full
#' @export
bccm_avg40to100m_salinity_full <- function(update = FALSE, ask = interactive(),
                                           force = FALSE, version = "01",
                                           cache_subfolder = "bccm_full"){
  get_zenodo_data("bccm_avg40to100m_salinity_full", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname bccm_bottom_oxygen_full
#' @export
bccm_avg40to100m_temperature_full <- function(update = FALSE, ask =
                                                                interactive(),
                                              force = FALSE, version = "01",
                                              cache_subfolder = "bccm_full"){
  get_zenodo_data("bccm_avg40to100m_temperature_full", update = update, ask =
                                                                          ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname bccm_bottom_oxygen_full
#' @export
bccm_avg100mtoBot_oxygen_full <- function(update = FALSE, ask = interactive(),
                                          force = FALSE, version = "01",
                                          cache_subfolder = "bccm_full"){
  get_zenodo_data("bccm_avg100mtoBot_oxygen_full", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname bccm_bottom_oxygen_full
#' @export
bccm_avg100mtoBot_ph_full <- function(update = FALSE, ask = interactive(),
                                      force = FALSE, version = "01",
                                      cache_subfolder = "bccm_full"){
  get_zenodo_data("bccm_avg100mtoBot_ph_full", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname bccm_bottom_oxygen_full
#' @export
bccm_avg100mtoBot_salinity_full <- function(update = FALSE, ask = interactive(),
                                            force = FALSE, version = "01",
                                            cache_subfolder = "bccm_full"){
  get_zenodo_data("bccm_avg100mtoBot_salinity_full", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname bccm_bottom_oxygen_full
#' @export
bccm_avg100mtoBot_temperature_full <- function(update = FALSE,
                                               ask = interactive(),
                                               force = FALSE, version = "01",
                                               cache_subfolder = "bccm_full"){
  get_zenodo_data("bccm_avg100mtoBot_temperature_full", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname bccm_bottom_oxygen_full
#' @export
bccm_phytoplankton_full <- function(update = FALSE, ask = interactive(),
                                    force = FALSE, version = "01",
                                    cache_subfolder = "bccm_full"){
  get_zenodo_data("bccm_phytoplankton_full", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}

#' @rdname bccm_bottom_oxygen_full
#' @export
bccm_primaryproduction_full <- function(update = FALSE, ask = interactive(),
                                        force = FALSE, version = "01",
                                        cache_subfolder = "bccm_full"){
  get_zenodo_data("bccm_primaryproduction_full", update = update, ask = ask,
                  force = force, version = version, cache_subfolder = cache_subfolder)
}
