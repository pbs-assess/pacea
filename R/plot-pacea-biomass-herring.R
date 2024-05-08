##' Plot a herring biomass time series object for one or all herring regions
##'
##' Adapted slightly from `plot.pacea_recruitment_herring()`. Plots in the
##' biomass style developed in `plot.pacea_biomass()`.
##'
##' @param obj a `pacea_biomass_herring` object, which is a tibble of
##'   annual spawning biomass or total biomass estimates (TODO not yet
##'   implemented) for each of the five herring Stock Assessment Regions.
##' @param region if `NULL` (the default) then plots a panel plot of biomass
##'   for all five regions, else plots the specified region, which must be one
##'   of HG, PRD, CC, SOG, or WCVI.
##' @param x_lab label for x-axis (default is "Year", only shown for bottom
##'   panel when plotting all five regions)
##' @param y_lab label for y-axis (default is specified by `attr(obj, "axis_name")`.
##' @param mar_all_regions `mar` value for `par()`, may need tweaking for
##'   five-panel plot; for single-panel just specific `mar`; see `?par`
##' @param oma_all_regions similar to `mar_all_regions`
##' @param title if `NULL` then no figure title (using `main()`), if `full` (the
##'   default) then spell out the Stock Assessment Region, and if `short` then
##'   just use the acronym.
##' @param ... further options passed onto `plot.pacea_biomass()` that can
##'   also pass onto `plot.default()`
##' @inherit plot.pacea_index
##' @return plot of the herring biomass time series as joined up medians with
##'   shaded regions showing uncertainty, for either all regions or the
##'   specified region.
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{ TODO
##'
##' }
plot.pacea_biomass_herring <- function(obj,
                                       region = NULL,
                                       x_lab = "Year",
                                       y_lab = attr(obj, "axis_name"),
                                       mar_all_regions = c(3, 3, 2, 0),
                                       oma_all_regions = c(2, 1, 1, 1),
                                       y_tick_by = 5,
                                       title = "full",
                                       ...){
  regions_all <- c("HG", "PRD", "CC", "SOG", "WCVI")

  stopifnot("region must be one of HG, PRD, CC, SOG, WCVI" =
              region %in% regions_all)

  stopifnot("title must be one of full, short, or NULL" =
              title %in% c("full", "short", "NULL"))

  regions_full <- c("Haida Gwaii",
                    "Prince Rupert District",
                    "Central Coast",
                    "Strait of Georgia",
                    "West Coast of Vancouver Island")
  if(is.null(title)){
    title_text_vec = ""} else
                       {
                         if(title == "full"){
                           title_text_vec = regions_full
                         }

                         if(title == "short"){
                           title_text_vec = regions_all
                         }
                       }

  # Plot one region
  if(!is.null(region)){

    title_text <- title_text_vec[which(regions_all == region)] # works for
                                        # title_text_vec = "" as returns NA

    region_choice <- region     # Else region == region in next line does not work

    obj_region <- dplyr::filter(obj,
                                region == region_choice)

    plot.pacea_biomass(obj_region,
                       y_tick_by = y_tick_by,
                       main = title_text,
                       ...)
  } else {
    # Plot all regions
    par_mfrow_orig <- par()$mfrow  # To reset at end
    par_mar_orig <- par()$mar
    par_oma_orig <- par()$oma

    par(mfrow = c(5, 1),
        mar = mar_all_regions,
        oma = oma_all_regions)

    for(i in 1:length(regions_all)){
      plot.pacea_biomass(dplyr::filter(obj,
                                           region == regions_all[i]),
                             main = title_text_vec[i],
                             xlab = "",
                             ylab = "",
                             y_tick_by = y_tick_by,
                             ...)
      if(i == 5){
        mtext(text = x_lab,
              side = 1,
              line = 2)
      }

      if(i == 3){
        mtext(text = y_lab,
              side = 2,
              line = 2)
      }
    }
    # Not sure these really help
    par(mfrow = par_mfrow_orig,
        mar = par_mar_orig,
        oma = par_oma_orig)
  }

  invisible()
}
