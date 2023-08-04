# Top is from hake-assessment (v2.2) and bottom is existing
# plot.pacea_recruitment(). Adapt the second using the first to give biomass
# plot with shaded intervals.

make.biomass.plot <- function(model,    ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                              equil.yr, ## Year in which unfished equilibium is assumed
                              start.yr, ## Year the timeseries starts (i.e. first year in model)
                              end.yr,   ## Year the timeseries ends (i.e. last year in model)
                              color = "blue"
                              ){
  oldpar <- par()

  slower <- model$mcmccalcs$slower
  smed <- model$mcmccalcs$smed
  supper <- model$mcmccalcs$supper

  unfished.eq.s <- model$mcmccalcs$sinit

  yrs <- equil.yr:end.yr
  par(mfrow=c(1,1),las=1,mar=c(3.5,3.5,1,1))

  start.equil.diff <- start.yr - equil.yr
  non.equil.yrs <- yrs[-(1:start.equil.diff)]
  non.equil.smed <- smed[names(smed) %in% non.equil.yrs]

  plot(non.equil.yrs,
       non.equil.smed,
       type = "o",
       lwd = 2,
       ylim = c(0, max(supper) + 0.1),
       xlab = "Year",
       ylab = "Female Spawning Biomass (million t)",
       xlim = range(yrs),
       cex.axis =0.9,
       cex.lab = 1,
       mgp = c(2.3,1,0),
       xaxt = "n",
       yaxs = "i")

  axis(1, at = big.ticks)
  axis(1,
       at = little.ticks,
       lab = rep("",length(little.ticks)), tcl = -0.3)
  axis(1,
       at = yrs[1],
       lab = paste0("Unfished\nequilibrium"),
       cex.axis = 0.9,
       mgp = c(3,2.5,0))
  box()
  points(equil.yr, unfished.eq.s[2], pch=16)
  arrows(equil.yr, unfished.eq.s[1], equil.yr, unfished.eq.s[3],
         angle = 90, code = 3, length = 0.06, col = color)
  addpoly(non.equil.yrs, slower[names(slower) %in% non.equil.yrs],
          supper[names(supper) %in% non.equil.yrs], color)
  par <- oldpar
}


##' Plot a pacea recruitment time series object (currently assumes annual values)
##'
##' Temporal plot for a pacea recruitment time series (of class
##' `pacea_recruitment`) object. The `style` option here (unlike for
##' `plot.pacea_index()` defaults to `no_uncertainty` and gets changed to
##' `uncertainty` if `low` and `high` are columns of `obj`.
##'
##' @param obj a `pacea_recruitment` object, which is a time series. Function
##'   will run on other objects (not give an error) but is not tested on those.
##' @inherit plot.pacea_index
##' @param x_tick_extra_years number of extra years to expand around the range
##'   of data for which to add annual tick marks
##' @param uncertainty_bar_col colour for uncertainty bars for certain types of
##'   plot (e.g. estimated fish recruitment)
##' @param y_max maximum y value for certain types of plot (use this if you get
##'   an error when specifying `ylim`)
##' @return plot of the time series as median with bars showing uncertainty (if
##'   `low` and `high` are columns of `obj) to the current device; returns nothing.
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' plot(hake_recruitment)
##' plot(hake_recruitment,
##'      xlim = c(lubridate::dmy(01011950),
##'               lubridate::dmy(01012040))) # to expand x-axis
##' }
plot.pacea_recruitment <- function(obj,
                                   value = "median",
                                   xlab = "Year",
                                   ylab = attr(obj, "axis_name"),
                                   # smooth_over_year = FALSE,
                                   y_tick = 0.25,
                                   x_tick_extra_years = 20,
                                   style = "no_uncertainty",
                                   uncertainty_bar_col = "blue",
                                   y_max = NULL,
                                   ...
                                   ){
  stopifnot("value must be a column of the pacea_recruitment object" =
            value %in% names(obj))

  stopifnot("function currently assumes annual (not monthly) recruitments" =
            !("month" %in% names(obj)))

  obj_lub <- lubridate_pacea_series(obj = obj)

  if(all(c("low", "high") %in% names(obj))){   # we have uncertainties so plot them
    style = "uncertainty"
  }

  if(style == "uncertainty"){
    plot_with_uncertainty(obj_lub,
                          value = value,
                          xlab = xlab,
                          ylab = ylab,
                          y_tick = y_tick,
                          x_tick_extra_years = x_tick_extra_years,
                          uncertainty_bar_col = uncertainty_bar_col,
                          y_max = y_max,
                          ...)
  } else {
    plot.default(obj_lub$date,
                 obj_lub[[value]], # [[]] returns a vector not a tibble
                 xlab = xlab,
                 ylab = ylab,
                 # y_tick = y_tick,
                 # x_tick_extra_years = x_tick_extra_years,
                 ...)
    # Add tickmark function here
  }
}
