##' Ecosystem summary for Pacific Hake. Currently
##' ignored in .Rbuildignore as developing.
##'
##' Using the drivers we have that were found by
##' Vestfals et al. These are currently (from Figure 7):
##'  * age-2+ herring spawning? biomass off WCVI (increased competition with herring on summer
##' feeding grounds leads to poorer feeding conditions and reduced adult
##' condition, so lower recruitment the following year)
##'  * north pacific current bifurcation index (northward shifted increases
##' advection of prey southwards leading to poorer feeding conditions off
##' BC/WA/OR and reduced adult condition, so lower recruitment the following
##' year.
##'  * PRED-age0-age1-hake - predation on age-0 hake by age-1 hake (so predation
##' in 2024 is due to age-1 in 2024). In the 3rd top model not the top one. But
##' show it as we have it.
##'
##' Do not yet have:
##' * EKE May-Sep (which is the most influential driver)
##' * AST-yolk
##' * STORMB-larv
##' * see others from Table 2. Does have NPGO and PDO in some models. TODO
##'
##' Might be simpler to tailor
##'   each species-specific function, and not have lots of if statements.
##' @param max_year maximum year to consider (TODO)
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'
##' }
ecosystem_summary_hake <- function(max_year = 2024){

  # Going to have to generate anomalies, and so just need medians, at least for now
  herring_competition <- dplyr::filter(herring_spawning_biomass,
                                       region == "WCVI") %>%
    dplyr::select(c("year",
                    "median"))

  bi

  hake_total_biomass_age_1  # only had median


#  par(mfrow = c(3, 1))
#  plot(herring_spawning_biomass, region = "WCVI")
#  plot(bi)
#  plot(hake_total_biomass_age_1)

  # Should generalise for adding more on. Think we should restrict each to the
  # full range of hake recruitment years. TODO need tweaking regarding year of effect.
  min_year <- min(hake_recruitment$year)
    #max(min(herring_competition$year),
    #              min(bi$year),
    #              min(hake_total_biomass_age_1$year))

  # Decide to calculate anomalies for only the time period given? I think so as
  # that's what would be used in any analysis. Call each an index.
  # TODO make a function for this, since will get used repeatedly. And check if
  # min year is min_year then no need to re-standardise
  herring_index <- dplyr::filter(herring_competition,
                                 year >= min_year) %>%
    dplyr::mutate(anomaly = standardise(median))
  class(herring_index) <- class(oni)  # so a pacea_index for plotting

# TODO - need to figure out shifting of years. x-axis should be the year of
 #  influenced age-0 hake recruitment
# TODO and flip axes


  #expect_equal(min(bi$year),
  #             min_year)
  bi_index  <- dplyr::filter(bi,
                             year >= min_year)

  hake_index <- dplyr::filter(hake_total_biomass_age_1,
                              year >= min_year) %>%
    dplyr::mutate(anomaly = standardise(median))
  class(hake_index) <- class(oni)

  x_lim <- c(lubridate::dmy(paste0("0101", 1965)),
             lubridate::dmy(paste0("0101", max_year)))  # TODO automate
  par(mfrow = c(4,1))
  lwd_index <- 16
  plot(hake_recruitment,
       xlim = x_lim,
       xlab = "",
       ylab = "")   # Else too much info; putting it into mtext
  mtext("Hake age-0 recruitment (billions of fish)", side = 3, adj = 0, cex = 0.7,
        line = 0.3)
  plot(herring_index, lwd = lwd_index,
       xlim = x_lim,
       xlab = "",
       ylab = "")
  mtext("Pacific Herring spawning biomass off WCVI - increases competition, lower recruitment next year",
        side = 3, adj = 0, cex = 0.7, line = 0.3)

  plot(bi_index, lwd = lwd_index,
       xlim = x_lim,
       xlab = "",
       ylab = "")
  mtext("North Pacific Current Bifurcation Index - poorer feeding conditions BC/WA/OR, lower recruitment next year",
        side = 3, adj = 0, cex = 0.7, line = 0.3)

  plot(hake_index, lwd = lwd_index,
       xlim = x_lim,
       xlab = "Year",
       ylab = "")

  mtext("Hake total biomass of age-1 fish - predation on age-0 fish",
        side = 3, adj = 0, cex = 0.7, line = 0.3)

  # Earlier temperature ideas:
  # For quickness for now, let's just look at buoy data for buoys within the
  #  study area defined in the manuscript.
  # For now just eyeballing which buoys to use, easiest to id by names:
  ## names_hg <- c("Central Dixon Entrance",
  ##               "South Moresby",   # is maybe just outside, but relevant?
  ##               "North Hecate Strait",
  ##               "South Hecate Strait",
  ##               "West Sea Otter",
  ##               "East Dellwood Knolls") # also maybe just outside

  ## stn_id_hg <- dplyr::filter(buoy_metadata,
  ##                            name %in% names_hg)$stn_id  # not necessarily in
  ##                                       # same order as stn_id_hg. Though think
  ##                                       # will be given how I first listed the
  ##                                       # names above. TODO

  ## buoy_sst_hg <- dplyr::filter(buoy_sst,
  ##                              stn_id %in% stn_id_hg)   # generalise variable
  ##                                       # names at some point to not be HG
  ##                                       # specific TODO

  # Usual style of plot:
  #for(i in 1:length(stn_id_hg)){
  #  plot(buoy_sst_hg,
  #       stn_id = stn_id_hg[1]) %>% print()
  #}

  ## # Want to calculate mean for each time period
  ## buoy_sst_hg_spring <- list()
  ## for(i in 1:length(stn_id_hg)){
  ##   buoy_sst_hg_spring[[i]] <-
  ##     dplyr::filter(buoy_sst_hg,
  ##                   stn_id %in% stn_id_hg[i],
  ##                   lubridate::month(date) %in% c(3, 4, 5)) %>%
  ##     dplyr::mutate(year = lubridate::year(date)) %>%
  ##     dplyr::group_by(year) %>%
  ##     summarise(mean = mean(sst,
  ##                           na.rm = TRUE))

  ##   # TODO add a check for how many days in the period, like in quality control
  ##   # for buoy_sst.

  ##   plot(buoy_sst_hg_spring[[i]],
  ##        xlim = range(years),
  ##        main = paste0("Mean spring SST, ",
  ##                      names_hg[i]),
  ##        type = "o")

  ## }
  #return(list(herring_index = herring_index,
  #            bi_index = bi_index,
  #            hake_index= hake_index,
  #            x_lim = x_lim))
}
