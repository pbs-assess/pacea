##' Plot anomalies of the OISST sea-surface temperature values
##'
##' Does a spatial plot of the anomalies.  TODO
##'
##' @param pacea_oisst_anomalies_list object of class `pacea_oisst_anomalies_list`
##' obtained from running `caclulate_anomalies()` on oisst values.
##' @param stn_id_to_plot character vector of station IDs (`stn_id` values OR
##' `name` values ) from the `buoy_sst` data
##' object. If `NULL` (default), anomalies for all buoys (in
##' `pacea_buoy_anomalies_list_object`) are plotted. If specified, only the
##' buoys in this vector are plotted. If `length(stn_id_to_plot == 1)` then anomalies
##' for each month are shown, with January at the top and December at the
##' bottom.
##' @param months numeric vector of months (1-12) to include. If not specified
##' then defaults to 4
##' (April), except when only one `stn_id_to_plot` when all months are plotted (unless specified). Can have more than one month, e.g. 6:9. For a 'winter' average,
##' say Nov-Mar, specify the months as `c(11, 12, 1, 2, 3)`. The winter average
##' anomaly will be calculated and named for the year in which January
##' falls. `months` must be consecutive (except for the overwintering `12, 1` as
##' in the above example).
##' @param main title for the plot, if `NULL` then created automatically,
##' including detailing the months selected. May need to manually specify `main`
##' if many non-consecutive months are chosen (which seems unlikely).
##' @param xlab x-axis label
##' @param ylab y-axis label
##' @param use_stn_id_name logical, whether to use the name of the buoy
##' (e.g. `Middle NOMAD`) or the `stn_id` (e.g. C46004). See `buoy_metadata` for
##' the names.
##' @param sst_plot character, one of:
##'   * `"anomalies"` (default) - plots SST anomalies
##'   * `"mean"` - plots mean SST values for each month
##'   * `"count"` - plots count of daily SST values used to calculate monthly mean
##' @param count_breaks numeric vector of break points for the count plot colour scale.
##' Only used when `sst_plot = "count"`. Default of `NULL` actually defaults to
##' `c(0, 10, 15, 20, 31) * length(months)` if `months` is specified, else it is
##' `c(0, 10, 15, 20, 31)`.
##' @param return_results logical, if `FALSE` (default) returns the plot object only.
##' If `TRUE`, prints the plot and returns a list with both `plot` and `results`.
##' @param require_requested_months numeric, number of months that must be available
##' out of the requested `months` to compute an average anomaly. If `NULL` (default),
##' defaults to `length(months)`. Only used when `length(months) > 1` and
##' `length(stn_id_to_plot) > 1`.
##' @param scale_limits numeric vector of length 2 specifying the limits for the colour scale.
##' If `NULL` (default), limits are calculated automatically based on the data. Only used
##' for non-count plots. If any plotted values are outside of `scale_limits`
##' then they are coloured grey.
##' @return a ggplot object (when `return_results = FALSE`) or a list with `plot` and `results`
##' (when `return_results = TRUE`)
##' @export
##' @author Andrew Edwards and Andrea Hilborn
##' @examples
##' \dontrun{
##' # See the vignette for explanations and use of the options.
##' all_buoys_anomalies <- calculate_anomalies(buoy_sst,
##'                                            climatology_time = "month")
##' plot(all_buoys_anomalies)
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
