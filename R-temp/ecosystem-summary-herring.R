##' Some example ideas for a species-specific Ecosystem Summary. Currently
##' ignored in .Rbuildignore as developing.
##'
##' Doing a herring example for now, based on Jennifer's manuscript.
##' Figure 11 gives the indicators that were identified in one or more
##' pressure-respose model. The ones we already have in pacea are:
##'  * PDO
##'  * SST_Spring - Mar-May, 1982-2019. Satellite measured sea surface temperature,
##'     from AVHRR satellite
##' https://www.avl.class.noaa.gov/release/data_available/avhrr/index.htm -
##' website not working, 24/9/24.
##' ##'  * SST_Summer - Apr-Jul, 1982-2019
##'  * SST_Fall - Sept-Nov, 1981-2019

##'  * Upwelling (maybe not)
##'
##'
##' Do not have
##'  * copepdods and total zooplankton in HG
##'  * humpback whales
##'  * Steller SeaLion
##'  * hakeAcoustic_B - could get
##'  * Arrowtooth, dogfish, cod, IPHC catch rates
##'  * herring-related
##' @param species species of interest, might need stock also. Could save the
##'   list of desired variables as a data list object, to then apply to the
##'   relevant calculation and plotting functions.
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'
##' }
ecosystem_summary <- function(species = "herring-example"){

  par(mfcol = c(4, 2))
  years <- range(dplyr::filter(herring_recruitment,
                               region == "HG") %>%
                 select(year))

  x_lim = c(lubridate::dmy(paste0("0101", min(years))),   # day-month-year
            lubridate::dmy(paste0("0101", max(years))))

  plot(herring_recruitment,
       region = "HG",
       xlim = x_lim)

  plot(pdo,
       xlim = x_lim)

  # For quickness for now, let's just look at buoy data for buoys within the
  #  study area defined in the manuscript.
  # For now just eyeballing which buoys to use, easiest to id by names:
  names_hg <- c("Central Dixon Entrance",
                "South Moresby",   # is maybe just outside, but relevant?
                "North Hecate Strait",
                "South Hecate Strait",
                "West Sea Otter",
                "East Dellwood Knolls") # also maybe just outside

  stn_id_hg <- dplyr::filter(buoy_metadata,
                             name %in% names_hg)$stn_id  # not necessarily in
                                        # same order as stn_id_hg. Though think
                                        # will be given how I first listed the
                                        # names above. TODO

  buoy_sst_hg <- dplyr::filter(buoy_sst,
                               stn_id %in% stn_id_hg)   # generalise variable
                                        # names at some point to not be HG
                                        # specific TODO

  # Usual style of plot:
  #for(i in 1:length(stn_id_hg)){
  #  plot(buoy_sst_hg,
  #       stn_id = stn_id_hg[1]) %>% print()
  #}

  # Want to calculate mean for each time period
  buoy_sst_hg_spring <- list()
  for(i in 1:length(stn_id_hg)){
    buoy_sst_hg_spring[[i]] <-
      dplyr::filter(buoy_sst_hg,
                    stn_id %in% stn_id_hg[i],
                    lubridate::month(date) %in% c(3, 4, 5)) %>%
      dplyr::mutate(year = lubridate::year(date)) %>%
      dplyr::group_by(year) %>%
      summarise(mean = mean(sst,
                            na.rm = TRUE))

    # TODO add a check for how many days in the period, like in quality control
    # for buoy_sst.

    plot(buoy_sst_hg_spring[[i]],
         xlim = range(years),
         main = paste0("Mean spring SST, ",
                       names_hg[i]),
         type = "o")

  }

}
