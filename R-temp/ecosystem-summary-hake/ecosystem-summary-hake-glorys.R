##' Ecosystem summary from GLORYS output for Pacific Hake. Currently
##' ignored in .Rbuildignore as developing. Run in this folder until generalise.
##' Doing quickly before assessment. Using values from Kristin.
##'
##' TODO.
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
##' @param par_mar vector for `par(mar)` TODO
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'
##' }
ecosystem_summary_hake_glorys <- function(max_year = 2024,   # though does to 2026
                                          lwd_index = 6,  # 8 was good on screen
                                          par_mar = c(2, 3, 1.2, 1),
                                          par_oma = c(1.5, 0, 0, 0),
                                          par_mgp = c(2, 1, 0)){
  # See ecosystem-summary-hake.R for explanations of those.

  glorys_new <- read.csv("DATA_Combined_glorys_hake_UW_for_Andy.csv") %>%
    tibble::as_tibble()
    # dplyr::rename("year" = "YEAR",
    #              "anomaly" = "ALEUTIAN.LOW.PRESSURE.INDEX..ALPI.")

  # Going to generate anomalies like in ecosystem_summary(). Goint to do usual
  # lower case variable names, but this will help to highlight that these are
  # standardised values (think of like nondimensionalising). And using Vestfals'
  # variable names (names in .csv file are slightly different).

  # Tspawn - TEMPspawn
  # SSHjac - SSH_jac (this matches)
  # LSTegg - CST_eggs.s or CST_eggs.n or AST_eggs ?   # Change ast_eggs below
  # once find out.
  # PUTlate - PU_latelarv
  # MLDlate - MLD_latelarv

  temp_spawn_index <- dplyr::select(glorys_new,
                                    year,
                                    value = "Tspawn") %>%
    dplyr::mutate(anomaly = standardise(value))
  class(temp_spawn_index) <- class(oni)

  ssh_jac_index <- dplyr::select(glorys_new,
                                 year,
                                 value = "SSHjac") %>%
    dplyr::mutate(anomaly = standardise(value))
  class(ssh_jac_index) <- class(oni)

  ast_eggs_index <- dplyr::select(glorys_new,
                                  year,
                                  value = "LSTegg") %>%
    dplyr::mutate(anomaly = standardise(value))
  class(ast_eggs_index) <- class(oni)

  pu_late_larv_index <- dplyr::select(glorys_new,
                                      year,
                                      value = "PUTlate") %>%
    dplyr::mutate(anomaly = standardise(value))
  class(pu_late_larv_index) <- class(oni)

  mld_late_larv_index <- dplyr::select(glorys_new,
                                      year,
                                      value = "MLDlate") %>%
    dplyr::mutate(anomaly = standardise(value))
  class(mld_late_larv_index) <- class(oni)

  mld_yolk_index <- dplyr::select(glorys_new,
                                      year,
                                      value = "MLDyolk") %>%
    dplyr::mutate(anomaly = standardise(value))
  class(mld_yolk_index) <- class(oni)


  # Should generalise for adding more on. Think we should restrict each to the
  # full range of hake recruitment years. TODO need tweaking regarding year of effect.
  min_year <- min(glorys_new$year)
    #max(min(herring_competition$year),
    #              min(bi$year),
    #              min(hake_total_biomass_age_1$year))

  # Decide to calculate anomalies for only the time period given? I think so as
  # that's what would be used in any analysis. Call each an index.
  # TODO make a function for this, since will get used repeatedly. And check if
  # min year is min_year then no need to re-standardise
#  herring_index <- dplyr::filter(herring_competition,
#                                 year >= min_year) %>%
#    dplyr::mutate(anomaly = standardise(median))
# class(herring_index) <- class(oni)  # so a pacea_index for plotting

# TODO - need to figure out shifting of years. x-axis should be the year of
 #  influenced age-0 hake recruitment
# TODO and flip axes

  x_lim <- c(lubridate::dmy(paste0("0101", min_year)),
             lubridate::dmy(paste0("0101", max_year)))  # TODO automate
  par(mfrow = c(7,1),
      mar = par_mar,
      oma = par_oma,
      mgp = par_mgp)

  plot(hake_recruitment,
       xlim = x_lim,
       y_max = max(dplyr::filter(hake_recruitment, year >= min_year)$high),
       xlab = "",
       ylab = "")   # Else too much info; putting it into mtext
  mtext("Hake age-0 recruitment (billions of fish)", side = 3, adj = 0, cex = 0.7,
        line = 0.3)

  plot(temp_spawn_index, lwd = lwd_index,
       xlim = x_lim,
       xlab = "",
       ylab = "")
  mtext("Mean temperature during spawning - when higher fish are less likely spawn but larvae grow quicker*",
        side = 3, adj = 0, cex = 0.7, line = 0.3)

  plot(ast_eggs_index, lwd = lwd_index,
       xlim = x_lim,
       xlab = "",
       ylab = "",
       ylim = rev(range(ast_eggs_index$anomaly)),
       y_axis_reverse = TRUE)
  mtext("Net along-shore transport - increased northward advection reduces recruitment",
        side = 3, adj = 0, cex = 0.7, line = 0.3)

  plot(mld_yolk_index, lwd = lwd_index,
       xlim = x_lim,
       xlab = "",
       ylab = "",
       ylim = rev(range(mld_yolk_index$anomaly)),
       y_axis_reverse = TRUE)
  mtext("Mean mixed layer depth during yolk stage (Jan-Apr) - shallower reduces recruitment",
        side = 3, adj = 0, cex = 0.7, line = 0.3)

  plot(ssh_jac_index, lwd = lwd_index,
       xlim = x_lim,
       xlab = "",
       ylab = "")
  mtext("Average sea-surface height (off California from Jan-Apr) - higher increases recruitment",
        side = 3, adj = 0, cex = 0.7, line = 0.3)

  plot(mld_late_larv_index, lwd = lwd_index,
       xlim = x_lim,
       xlab = "",
       ylab = "")
  mtext("Mean mixed layer depth during late larval stage (Mar-Jun) - shallower reduces recruitment",
        side = 3, adj = 0, cex = 0.7, line = 0.3)

  plot(pu_late_larv_index, lwd = lwd_index,
       xlim = x_lim,
       xlab = "",
       ylab = "",
       ylim = rev(range(pu_late_larv_index$anomaly)),
       y_axis_reverse = TRUE)
  mtext("Strength of poleward undercurrent - increased northward advection reduces recruitment",
        side = 3, adj = 0, cex = 0.7, line = 0.3)

  mtext("Year of hake recruitment", side = 1, line = 2)
}
