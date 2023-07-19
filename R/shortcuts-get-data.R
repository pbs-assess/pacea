#' ROMS environmental variable data 
#' 
#' @description 
#' Regional Ocean Modelling System (ROMS) data for the BC Pacific region.
#' 
#' ROMS data interpolated to a 2km x 2km inshore grid and a 6km x 6km offshore grid. 
#' Data are monthly averages from 1993-2019. Each column represents a distinct 'year_month' combination (e.g. 2010_4).
#' 
#' @details
#' \describe{
#'   \code{roms_*depth*_oxygen()} Dissolved oxygen measured as mmol-oxygen m^-3\cr
#'   \code{roms_*depth*_salinity()} Salinity in ppt\cr
#'   \code{roms_*depth*_temperature()} Temperature in oC\cr
#'   \code{roms_*depth*_ph()} pH\cr
#'   \code{roms_phytoplankton()} Total phytoplankton biomass in mmol-nitrogen m^-2\cr
#'   \code{roms_primaryproduction()} Total primary production in gC m^-2 d^-1
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
#' @examples
#' \dontrun{
#' my_data <- roms_bottom_oxygen()
#' }
roms_bottom_oxygen <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("roms_bottom_oxygen", update = update, ask = ask, force = force)
}

#' @rdname roms_bottom_oxygen
roms_bottom_ph <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("roms_bottom_pH", update = update, ask = ask, force = force)
}

#' @rdname roms_bottom_oxygen
roms_bottom_salinity <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("roms_bottom_salinity", update = update, ask = ask, force = force)
}

#' @rdname roms_bottom_oxygen
roms_bottom_temperature <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("roms_bottom_temperature", update = update, ask = ask, force = force)
}

#' @rdname roms_bottom_oxygen
roms_surface_oxygen <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("roms_surface_oxygen", update = update, ask = ask, force = force)
}

#' @rdname roms_bottom_oxygen
roms_surface_ph <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("roms_surface_pH", update = update, ask = ask, force = force)
}

#' @rdname roms_bottom_oxygen
roms_surface_salinity <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("roms_surface_salinity", update = update, ask = ask, force = force)
}

#' @rdname roms_bottom_oxygen
roms_surface_temperature <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("roms_surface_temperature", update = update, ask = ask, force = force)
}

#' @rdname roms_bottom_oxygen
roms_avg0to40m_oxygen <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("roms_avg0to40m_oxygen", update = update, ask = ask, force = force)
}

#' @rdname roms_bottom_oxygen
roms_avg0to40m_ph <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("roms_avg0to40m_ph", update = update, ask = ask, force = force)
}

#' @rdname roms_bottom_oxygen
roms_avg0to40m_salinity <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("roms_avg0to40m_salinity", update = update, ask = ask, force = force)
}

#' @rdname roms_bottom_oxygen
roms_avg0to40m_temperature <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("roms_avg0to40m_temperature", update = update, ask = ask, force = force)
}

#' @rdname roms_bottom_oxygen
roms_avg40to100m_oxygen <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("roms_avg40to100m_oxygen", update = update, ask = ask, force = force)
}

#' @rdname roms_bottom_oxygen
roms_avg40to100m_ph <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("roms_avg40to100m_ph", update = update, ask = ask, force = force)
}

#' @rdname roms_bottom_oxygen
roms_avg40to100m_salinity <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("roms_avg40to100m_salinity", update = update, ask = ask, force = force)
}

#' @rdname roms_bottom_oxygen
roms_avg40to100m_temperature <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("roms_avg40to100m_temperature", update = update, ask = ask, force = force)
}

#' @rdname roms_bottom_oxygen
roms_avg100mtoBot_oxygen <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("roms_avg100mtoBot_oxygen", update = update, ask = ask, force = force)
}

#' @rdname roms_bottom_oxygen
roms_avg100mtoBot_ph <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("roms_avg100mtoBot_ph", update = update, ask = ask, force = force)
}

#' @rdname roms_bottom_oxygen
roms_avg100mtoBot_salinity <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("roms_avg100mtoBot_salinity", update = update, ask = ask, force = force)
}

#' @rdname roms_bottom_oxygen
roms_avg100mtoBot_temperature <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("roms_avg100mtoBot_temperature", update = update, ask = ask, force = force)
}

#' @rdname roms_bottom_oxygen
roms_phytoplankton <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("roms_phytoplankton", update = update, ask = ask, force = force)
}

#' @rdname roms_bottom_oxygen
roms_primaryproduction <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("roms_primaryproduction", update = update, ask = ask, force = force)
}

