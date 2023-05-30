#' ROMS environmental variable data 
#' 
#' @description 
#' Regional Ocean Modelling System (ROMS) data for the BC Pacific region.
#' 
#' ROMS data interpolated to a 2km x 2km inshore grid and a 6km x 6km offshore grid. 
#' Data are monthly averages from 1993-2019. Each column represents a distinct 'year_month' combination (e.g. 2010_4).
#' 
#' \code{roms_bottom_oxygen()} Bottom oxygen measured as UNITS [02]\cr
#' \code{roms_bottom_salinity()} Bottom salinity in UNITS!\cr
#' \code{roms_bottom_temperature()} Bottom temperature in oC\cr
#' \code{roms_surface_oxygen()} Surface oxygen measured as UNITS [02]\cr
#' \code{roms_surface_salinity()} Surface salinity in UNITS!\cr
#' \code{roms_surface_temperature()} Surface temperature in oC
#' 
#' @format A simple features dataframe.
#'
#' @param ask Logical. Should the user be asked before downloading the data to local cache? Defaults to the value of interactive().
#'
#' @return `sf` data object requested.
#' @export
#'
#' @examples
#' \dontrun{
#' my_data <- roms_bottom_oxygen()
#' }
roms_bottom_oxygen <- function(ask = interactive()) {
  get_pacea_data("roms_bottom_oxygen", ask = ask)
}

#' @rdname roms_bottom_oxygen
roms_bottom_salinity <- function(ask = interactive()) {
  get_pacea_data("roms_bottom_salinity", ask = ask)
}

#' @rdname roms_bottom_oxygen
roms_bottom_temperature <- function(ask = interactive()) {
  get_pacea_data("roms_bottom_temperature", ask = ask)
}

#' @rdname roms_bottom_oxygen
roms_surface_oxygen <- function(ask = interactive()) {
  get_pacea_data("roms_surface_oxygen", ask = ask)
}

#' @rdname roms_bottom_oxygen
roms_surface_salinity <- function(ask = interactive()) {
  get_pacea_data("roms_surface_salinity", ask = ask)
}

#' @rdname roms_bottom_oxygen
roms_surface_temperature <- function(ask = interactive()) {
  get_pacea_data("roms_surface_temperature", ask = ask)
}

