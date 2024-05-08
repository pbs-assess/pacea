plot.pacea_recruitment_herring <- function(obj,
                                           region = NULL,
                                           x_lab = "Year",
                                           y_lab = attr(obj, "axis_name"),
                                           mar_all_regions = c(3, 3, 2, 0),
                                           oma_all_regions = c(2, 1, 1, 1),
                                           y_tick_by = 0.25,

                                           ...){
  stopifnot("region must be one of HG, PRD, CC, SOG, WCVI" =
              region %in% c("HG", "PRD", "CC", "SOG", "WCVI"))

  # Plot one region
  if(!is.null(region)){
    region_choice <- region     # Else region == region in next line does not work
    obj_region <- dplyr::filter(obj,
                                region == region_choice)
    plot.pacea_recruitment(obj_region,
                           y_tick_by = y_tick_by,
                           ...)
  } else {
    # Plot all regions
    par_mfrow_orig <- par()$mfrow  # To reset at end
    par_mar_orig <- par()$mar
    par_oma_orig <- par()$oma

    par(mfrow = c(5, 1),
        mar = mar_all_regions,
        oma = oma_all_regions)
    regions_all <- c("HG", "PRD", "CC", "SOG", "WCVI")

    # Just do x and y labels once each
#    xlab = c("", "", "", "", x_lab)
#    ylab = c("", "", y_lab, "", "")

    for(i in 1:length(regions_all)){
      plot.pacea_recruitment(dplyr::filter(obj,
                                           region == regions_all[i]),
                             main = regions_all[i],
                             # CHANGE TO LONG NAMES
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
