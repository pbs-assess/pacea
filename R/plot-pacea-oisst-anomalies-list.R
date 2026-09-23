##' Plot anomalies of the OISST sea-surface temperature values, as either a
##' single or series of spatial plots, or a heatmap style.
##'
##' See `heatmap_style` arguments for the resulting type of plot.
##' @param pacea_oisst_anomalies_list object of class `pacea_oisst_anomalies_list`
##' obtained from running `caclulate_anomalies()` on oisst values.
##' @param heatmap_style logical, if `FALSE` (default) then do a spatial plot of
##' the absolute anomalies with one map for each month and year, with default
##' (`months = NULL` being the latest month of data. See [plot.pacea_oianom()] for arguments.
##' If `heatmap_style = TRUE` then do a heatmap style figure showing the
##' absolute anomalies for each month averaged over the designated area. Has
##' January at the top and December at the bottom of the y-axis, with years
##' along the x-axis. See [plot_pacea_oisst_anomalies_heatmap_style()] for
##' arguments.
##' @param ... arguments to be passed onto
##' [plot_pacea_oisst_anomalies_heatmap_style()] or [plot.pacea_oianom()].
##' @author Andrew Edwards and Andrea Hilborn
##' @examples
##' \dontrun{
##' # See the vignette for explanations and use of the options.
##' oisst_anomalies <- calculate_anomalies(oisst_month)
##' plot(oisst_anomalies)         # spatial map of anomalies for most recent much
##' plot(oisst_anomalies, heatmap_style = TRUE)   # mean anomaly for each month
##'                                    # averaged over the whole spatial domain
##' }
plot.pacea_oisst_anomalies_list <- function(pacea_oisst_anomalies_list,
                                            heatmap_style = FALSE,
                                            ...){
  if(heatmap_style){
    plot_pacea_oisst_anomalies_heatmap_style(pacea_oisst_anomalies_list,
                                    ...)
  } else {
    plot.pacea_oianom(pacea_oisst_anomalies_list$anomalies,
                      ...)
  }
}
