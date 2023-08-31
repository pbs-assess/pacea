#' BCCM environmental variable data 
#' 
#' @description 
#' Loading and/or downloading British Columbia continental margin (BCCM) model data for the BC Pacific region to local drive.
#' 
#' @details
#' 
#' The following functions serve to download specific individual ocean variables of the BCCM model data. Use `bccm_all_variables()` if you would like to download all variables - see help file `?bccm_all_variables` for details. If a variable has already been downloaded, the function will load data from `pacea_cache()` folder into user's local R environment.
#' 
#' The British Columbia continental margin (BCCM) model is an ocean circulation-biogeochemical model implementation of the regional ocean modelling system (ROMS). It has a horizontal resolution of 3km x 3km and a vertical discretization based on bathymetry of 42 depth levels increasing in resolution near the surface. These modelled output data were provided by Angelica Pena and the data is further detailed in Pena et al. (2019). 
#' 
#' BCCM data were interpolated to a 2km x 2km inshore grid and a 6km x 6km offshore grid. Data provided are monthly means that span from 1993-2019. Each column represents a distinct 'year_month' combination (e.g. 2010_4).
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
#' @source Pena et al., 2019
#' 
#' @examples
#' \dontrun{
#' my_data <- bccm_bottom_oxygen()
#' }
bccm_bottom_oxygen <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("bccm_bottom_oxygen", update = update, ask = ask, force = force)
}

#' @rdname bccm_bottom_oxygen
#' @export
bccm_bottom_ph <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("bccm_bottom_pH", update = update, ask = ask, force = force)
}

#' @rdname bccm_bottom_oxygen
#' @export
bccm_bottom_salinity <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("bccm_bottom_salinity", update = update, ask = ask, force = force)
}

#' @rdname bccm_bottom_oxygen
#' @export
bccm_bottom_temperature <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("bccm_bottom_temperature", update = update, ask = ask, force = force)
}

#' @rdname bccm_bottom_oxygen
#' @export
bccm_surface_oxygen <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("bccm_surface_oxygen", update = update, ask = ask, force = force)
}

#' @rdname bccm_bottom_oxygen
#' @export
bccm_surface_ph <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("bccm_surface_pH", update = update, ask = ask, force = force)
}

#' @rdname bccm_bottom_oxygen
#' @export
bccm_surface_salinity <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("bccm_surface_salinity", update = update, ask = ask, force = force)
}

#' @rdname bccm_bottom_oxygen
#' @export
bccm_surface_temperature <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("bccm_surface_temperature", update = update, ask = ask, force = force)
}

#' @rdname bccm_bottom_oxygen
#' @export
bccm_avg0to40m_oxygen <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("bccm_avg0to40m_oxygen", update = update, ask = ask, force = force)
}

#' @rdname bccm_bottom_oxygen
#' @export
bccm_avg0to40m_ph <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("bccm_avg0to40m_ph", update = update, ask = ask, force = force)
}

#' @rdname bccm_bottom_oxygen
#' @export
bccm_avg0to40m_salinity <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("bccm_avg0to40m_salinity", update = update, ask = ask, force = force)
}

#' @rdname bccm_bottom_oxygen
#' @export
bccm_avg0to40m_temperature <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("bccm_avg0to40m_temperature", update = update, ask = ask, force = force)
}

#' @rdname bccm_bottom_oxygen
#' @export
bccm_avg40to100m_oxygen <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("bccm_avg40to100m_oxygen", update = update, ask = ask, force = force)
}

#' @rdname bccm_bottom_oxygen
#' @export
bccm_avg40to100m_ph <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("bccm_avg40to100m_ph", update = update, ask = ask, force = force)
}

#' @rdname bccm_bottom_oxygen
#' @export
bccm_avg40to100m_salinity <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("bccm_avg40to100m_salinity", update = update, ask = ask, force = force)
}

#' @rdname bccm_bottom_oxygen
#' @export
bccm_avg40to100m_temperature <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("bccm_avg40to100m_temperature", update = update, ask = ask, force = force)
}

#' @rdname bccm_bottom_oxygen
#' @export
bccm_avg100mtoBot_oxygen <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("bccm_avg100mtoBot_oxygen", update = update, ask = ask, force = force)
}

#' @rdname bccm_bottom_oxygen
#' @export
bccm_avg100mtoBot_ph <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("bccm_avg100mtoBot_ph", update = update, ask = ask, force = force)
}

#' @rdname bccm_bottom_oxygen
#' @export
bccm_avg100mtoBot_salinity <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("bccm_avg100mtoBot_salinity", update = update, ask = ask, force = force)
}

#' @rdname bccm_bottom_oxygen
#' @export
bccm_avg100mtoBot_temperature <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("bccm_avg100mtoBot_temperature", update = update, ask = ask, force = force)
}

#' @rdname bccm_bottom_oxygen
#' @export
bccm_phytoplankton <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("bccm_phytoplankton", update = update, ask = ask, force = force)
}

#' @rdname bccm_bottom_oxygen
#' @export
bccm_primaryproduction <- function(update = FALSE, ask = interactive(), force = FALSE) {
  get_pacea_data("bccm_primaryproduction", update = update, ask = ask, force = force)
}



#' Download all BCCM data
#' 
#' This function downloads all BCCM model data. 
#' 
#' @details 
#' 
#' Use `bccm_data` To view the full list of available variables
#' 
#' CAUTION: The BCCM files are large and downloading all data may take a while.
#'
#' @return downloaded files to `pacea_cache()` directory
#' @export
#'
#' @examples
#' \dontrun{
#' bccm_all_variables()
#' }
bccm_all_variables <- function() {
  
  cache_dir <- pacea_cache()
  
  ans <- ask(paste("Downloading all BCCM data may take many minutes. Files will be downloaded to pacea_cache directory:",
                   cache_dir, "Would you like to continue?", sep = "\n"))
  
  if (!ans) stop("Exiting...", call. = FALSE)
  
  bccm.datalist <- bccm_data
  
  for(i in 1:nrow(bccm.datalist)){
    data.name <- bccm.datalist[i, 1]
    get_pacea_data(data.name, force = TRUE)
  }
  
  return(print("Download of all BCCM files: successful!"))
}

