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
##' @param assessment_year year of assessment, plots years to
##' `assessement_year+2` it seems. If 2025 then uses GLORYS values provided by Kristin
##' Marshall (NOAA). If 2026 uses values calculated in `glorys_2026.Rmd` here.
##' @param index_list_rds name of .rds file in which `glorys_2026.Rmd` saves the indices
##' @param par_mar vector for `par(mar)` TODO
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'
##' }
ecosystem_summary_hake_glorys <- function(assessment_year = 2026,
                                          index_list_rds = "../glorys/from-noaa-folks/glorys_indices_2026.Rds",
                                          lwd_index = 6,  # 8 was good on screen
                                          par_mar = c(2, 3, 1.2, 1),
                                          par_oma = c(1.5, 0, 0, 0),
                                          par_mgp = c(2, 1, 0)){
  # See ecosystem-summary-hake.R for explanations of those.

  if(assessment_year == 2025){
    glorys_new <- read.csv("DATA_Combined_glorys_hake_UW_for_Andy.csv") %>%
      tibble::as_tibble()
    # dplyr::rename("year" = "YEAR",
    #              "anomaly" = "ALEUTIAN.LOW.PRESSURE.INDEX..ALPI.")

  # Going to generate anomalies like in ecosystem_summary(). Goint to do usual
  # lower case variable names, but this will help to highlight that these are
  # standardised values (think of like nondimensionalising). And using Vestfals'
  # variable names (names in .csv file are slightly different).

  # Tspawn - temp_spawn
  # SSHjac - ssh_jac (this matches)
  # LSTegg - CST_eggs.s or CST_eggs.n or AST_eggs ?   # Change ast_eggs below
  # once find out.
  # PUTlate - pu_late_larv
  # MLDlate - mld_late_larv
  # MLDyolk - mld_yolk
  # then all get ..index appended

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

    min_year <- min(glorys_new$year)
    #max(min(herring_competition$year),
    #              min(bi$year),
    #              min(hake_total_biomass_age_1$year))

  }

  if(assessment_year == 2026){
    glorys_index_list = readRDS(index_list_rds)

    min_year <- sapply(glorys_index_list,
                       function(x) min(x[, "year"])) %>%
        min()

    # Need them all, put in global so can see them
    list2env(glorys_index_list,
             .GlobalEnv)

    # 21 Jan 2026  TODO resolve when time
    # SSTjac, ASTeggs, PUlatelarv not available for update (see glorys_2026.Rmd)
    #  as unsure of domain, so use the 2025 values for those
    glorys_2025 <- read.csv("DATA_Combined_glorys_hake_UW_for_Andy.csv") %>%
      tibble::as_tibble()

    ssh_jac_index <- dplyr::select(glorys_2025,
                                   year,
                                   value = "SSHjac") %>%
      dplyr::mutate(anomaly = standardise(value))
    class(ssh_jac_index) <- class(oni)

    ast_eggs_index <- dplyr::select(glorys_2025,
                                    year,
                                    value = "LSTegg") %>%
      dplyr::mutate(anomaly = standardise(value))
    class(ast_eggs_index) <- class(oni)

    pu_late_larv_index <- dplyr::select(glorys_2025,
                                        year,
                                        value = "PUTlate") %>%
      dplyr::mutate(anomaly = standardise(value))
    class(pu_late_larv_index) <- class(oni)

    # Change here and in ecosystem-summary-hake.R, maybe make a function TODO
    hake_recruitment_with_na <- hake_recruitment
    # TODO this should be more generalisable, though will see any missing years in
    # output if more are needed, plus it's fine for 2026
    if(max(hake_recruitment_with_na$year < assessment_year)){
        hake_recruitment_with_na <- rbind(hake_recruitment_with_na,
                                          c(assessment_year,
                                            NA,
                                            NA,
                                            NA))
    }

    # Now change values for years that weren't estimated to data NA's
    # TODO needs generalising, works for 2026
    hake_recruitment_with_na[hake_recruitment_with_na$year %in% 2023:2025,
                             c("low", "median", "high")] <- NA
  }


  # Should generalise for adding more on. Think we should restrict each to the
  # full range of hake recruitment years. TODO need tweaking regarding year of effect.

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
             lubridate::dmy(paste0("0101", assessment_year)))  # may want more
  par(mfrow = c(7,1),
      mar = par_mar,
      oma = par_oma,
      mgp = par_mgp)

  plot(hake_recruitment_with_na,    # TODO wont work for 2025 right now
       xlim = x_lim,
       y_max = max(dplyr::filter(hake_recruitment_with_na, year >=
                                                           min_year)$high,
                   na.rm = TRUE),
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

  # TODO remove when have available
  if(exists("ast_eggs_index")){
  plot(ast_eggs_index, lwd = lwd_index,
       xlim = x_lim,
       xlab = "",
       ylab = "",
       ylim = rev(range(ast_eggs_index$anomaly)),
       y_axis_reverse = TRUE)
  mtext("Net along-shore transport - increased northward advection reduces recruitment",
        side = 3, adj = 0, cex = 0.7, line = 0.3)
  }

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

  # TODO remove when have available
  if(exists("pu_late_larv_index")){

  plot(pu_late_larv_index, lwd = lwd_index,
       xlim = x_lim,
       xlab = "",
       ylab = "",
       ylim = rev(range(pu_late_larv_index$anomaly)),
       y_axis_reverse = TRUE)
  mtext("Strength of poleward undercurrent - increased northward advection reduces recruitment",
        side = 3, adj = 0, cex = 0.7, line = 0.3)
}
  mtext("Year of hake recruitment", side = 1, line = 2)

# Create one big tibble to return; ensure each new index to add is just year and
#  anomaly. Then make a cumulative index by just summing the normalised indices.
  tib <- hake_recruitment_with_na %>%
    dplyr::rename(hake_recruitment_low = low,
                  hake_recruitment_median = median,
                  hake_recruitment_high = high) %>%
    dplyr::left_join(dplyr::select(temp_spawn_index,
                                   year,
                                   anomaly),
                     by = "year") %>%
    dplyr::rename(temp_spawn_index = anomaly) %>%
    dplyr::left_join(dplyr::select(ast_eggs_index,
                                   year,
                                   anomaly),
                     by = "year") %>%
    dplyr::rename(ast_eggs_index = anomaly) %>%
    dplyr::left_join(mld_yolk_index,
                     by = "year") %>%
    dplyr::rename(mld_yolk_index = anomaly) %>%
    dplyr::left_join(ssh_jac_index,
                     by = "year") %>%
    dplyr::rename(ssh_jac_index = anomaly) %>%
    dplyr::left_join(dplyr::select(mld_late_larv_index,
                                   year,
                                   anomaly),
                     by = "year") %>%
    dplyr::rename(mld_late_larv_index = anomaly) %>%
    dplyr::left_join(dplyr::select(pu_late_larv_index,
                                   year,
                                   anomaly),
                     by = "year") %>%
    dplyr::rename(pu_late_larv_index = anomaly) %>%

    dplyr::mutate(cumulative_index =
             rowSums(dplyr::across(c("temp_spawn_index",
                                     "ast_eggs_index",
                                     "mld_yolk_index",
                                     "ssh_jac_index",
                                     "mld_late_larv_index",
                                     "pu_late_larv_index"))))
    # cumulative_index will have NA for any rows with an NA, which are the first
    # two here because bi has NA in first two years.

    return(tib)
}

