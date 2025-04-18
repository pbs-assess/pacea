##' Ecosystem summary for Pacific Hake. Currently
##' ignored in .Rbuildignore as developing. Need to write proper help once
##' assessment done. TODO. Keeping pacea and GLORYS ones separate for now as
##' ranges of years differ.
##'
##' Flipping anomaly (+/-) for some variables so that +ve is good for hake recuitment, as per table
##' in 2025 draft hake assessment. Need to document properly.
##'   Also shifting years based on that table. # year (x-axis) is the year of
##'  influenced age-0 hake recruitment
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
##' NPGO_pre (Table A1 has pre and _AS but just go with calling it pre for
##' now). Shows up in Table 2.
##'
##' Do not yet have:
##' * EKE May-Sep (which is the most influential driver)
##' * AST-yolk
##' * STORMB-larv
##' * see others from Table 2. Does have NPGO and PDO in some models. TODO
##'
##' Might be simpler to tailor
##'   each species-specific function, and not have lots of if statements.
##' @param max_year maximum year to consider (TODO), xlim is then one less (but
##'   plots further). Default is NULL which then does
##'   `max(hake_recruitment$year)` (which was 2025 for 2025 assessment)
##' @param par_mar vector for `par(mar)` TODO
##' @param short_talk_version short version explicitly (for now) for SOPO 2025
##'   talk, only plots hake age-0, herring, bifurcation index, npgo
##' @param rec_devs_zero_final_years number of years at the end for which
##'   recruitment deviations were set to zero in the assessment model, and so
##'   are not influenced by data; now not plotting anything for those (for hake
##'   2025 assessment we did plot them, but outside of that it makes sense not
##'   to, to make the point that we do not know anything about the cohort size).
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'
##' }
ecosystem_summary_hake <- function(max_year = NULL,
                                   lwd_index = 6,   # 8 was good on screen
                                   par_mar = c(2, 3, 1.2, 1),
                                   par_oma = c(1.5, 0, 0, 0),
                                   par_mgp = c(2, 1, 0),
                                   short_talk_version = FALSE,
                                   rec_devs_zero_final_years = 3){
  # From https://stackoverflow.com/questions/13239986/avoid-wasting-space-when-placing-multiple-aligned-plots-onto-one-page
  # par(mfrow = c(2, 2),     # 2x2 layout
  #    oma = c(2, 2, 0, 0), # two rows of text at the outer left and bottom margin
  #  mar = c(1, 1, 0, 0), # space for one row of text at ticks and to separate plots
  #  mgp = c(2, 1, 0),    # axis label at 2 rows distance, tick labels at 1 row
  #  xpd = NA)            # allow content to protrude into outer margin (and beyond)

  if(is.null(max_year)){
    max_year <- max(hake_recruitment$year)
  }

  x_lim <- c(lubridate::dmy(paste0("0101", 1965)),
             lubridate::dmy(paste0("0101", max_year - 1)))

  # Do not plot the last rec_devs_zero_final_years, since these have not been
  #  estimated from data
  hake_recruitment_to_plot <- dplyr::filter(hake_recruitment,
                                            year < max_year - rec_devs_zero_final_years + 1)


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
  min_year <- min(hake_recruitment_deviations$year)
    #max(min(herring_competition$year),
    #              min(bi$year),
    #              min(hake_total_biomass_age_1$year))

  # Decide to calculate anomalies for only the time period given? I think so as
  # that's what would be used in any analysis. Call each an index.
  # TODO make a function for this, since will get used repeatedly. And check if
  # min year is min_year then no need to re-standardise.
  # year becomes year that hake recruitment is influenced.
  herring_index <- dplyr::filter(herring_competition,
                                 year >= min_year - 1) %>%  # -1 since going to
                                        # increment next.
    dplyr::mutate(year = year + 1,
                  anomaly = standardise(median))
  class(herring_index) <- class(oni)  # so a pacea_index for plotting


  # shift years by 1 like herring and then restandardise
  bi_index  <- dplyr::filter(bi,
                             year >= min_year - 1) %>%
    dplyr::mutate(year = year + 1,
                  anomaly = standardise(anomaly))

  # shift years by 1 like herring, average over Apr-Sep as in Vestfals, and then restandardise
  pdo_pre_index  <- dplyr::filter(pdo,
                                  year >= min_year - 1,
                                  month %in% 4:9) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(anomaly = mean(anomaly)) %>%
    dplyr::mutate(year = year + 1,
                  anomaly = standardise(anomaly))
  class(pdo_pre_index) <- class(oni)  # so a pacea_index for plotting

  # shift years by 1 like herring, average over Apr-Sep as in Vestfals, and then restandardise
  npgo_pre_index  <- dplyr::filter(npgo,
                                   year >= min_year - 1,
                                   month %in% 4:9) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(anomaly = mean(anomaly)) %>%
    dplyr::mutate(year = year + 1,
                  anomaly = standardise(anomaly))
  class(npgo_pre_index) <- class(oni)  # so a pacea_index for plotting


  # no shifting, take log (as in Vestfals)  and then restandardise
  hake_log_age1_index <- dplyr::filter(hake_total_biomass_age_1,
                              year >= min_year) %>%
    dplyr::mutate(year = year,
                  log_median = log(median),
                  anomaly = standardise(log_median))
  class(hake_log_age1_index) <- class(oni)

  par(mfrow = c(ifelse(short_talk_version,
                       4,
                       6),
                1),
      mar = par_mar,
      oma = par_oma,
      mgp = par_mgp)

  plot(hake_recruitment_to_plot,
       xlim = x_lim,
       xlab = "",
       ylab = "")   # Else too much info; putting it into mtext
  mtext("Hake age-0 recruitment (billions of fish)", side = 3, adj = 0, cex = 0.7,
        line = 0.3)

  plot(herring_index, lwd = lwd_index,
       xlim = x_lim,
       xlab = "",
       ylab = "",
       ylim = rev(range(herring_index$anomaly)),   # TODO add to
                                        # plot.pacea_index and test everything,
                                        # so don't need to specify this if
                                        # y_axis_revers = TRUE
       y_axis_reverse = TRUE)
  mtext("Pacific Herring spawning biomass off WCVI - increases competition, lower recruitment next year",
        side = 3, adj = 0, cex = 0.7, line = 0.3)

  plot(bi_index, lwd = lwd_index,
       xlim = x_lim,
       xlab = "",
       ylab = "",
       ylim = rev(range(bi_index$anomaly)),
       y_axis_reverse = TRUE)
  mtext("North Pacific Current Bifurcation Index - poorer feeding conditions BC/WA/OR, lower recruitment next year",
        side = 3, adj = 0, cex = 0.7, line = 0.3)

  if(!short_talk_version){
    plot(pdo_pre_index, lwd = lwd_index,
         xlim = x_lim,
         xlab = "",
         ylab = "",
         ylim = rev(range(pdo_pre_index$anomaly)),
         y_axis_reverse = TRUE)
    mtext("Pacific Decadal Oscillation preconditioning index - lower general production, lower recruitment next year",
          side = 3, adj = 0, cex = 0.7, line = 0.3)
  }

  plot(npgo_pre_index, lwd = lwd_index,
       xlim = x_lim,
       xlab = "",
       ylab = "")
  mtext("North Pacific Gyre Oscillation preconditioning index - higher general production, higher recruitment next year",
        side = 3, adj = 0, cex = 0.7, line = 0.3)

  if(!short_talk_version){
    plot(hake_log_age1_index, lwd = lwd_index,
         xlim = x_lim,
         xlab = "",
         ylab = "",
         ylim = rev(range(hake_log_age1_index$anomaly)),
         y_axis_reverse = TRUE)

    mtext("Hake total log biomass of age-1 fish - more predation on age-0 fish lowers recruitment that year",
          side = 3, adj = 0, cex = 0.7, line = 0.3)
  }

  mtext("Year of hake recruitment", side = 1, line = 2)

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
  #            hake_age1_index= hake_age1_index,
  #            x_lim = x_lim))
}
