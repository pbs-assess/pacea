##' Create an annual standardised index based on a pacea object
##'
##' Create an annual index based on a pacea object. The annual value can be
##' calculated from annual values, such as `hake_recruitment`, or averaged for
##' specific monthly values (e.g. PDO for Apr to Sep). See the examples and
##' vignette TODO
##' Calculations are based on the class of the object: [create_index()] is a generic function that calls `create_index.<class>()`
##' where `<class>` is `class(data)`. [create_index.pacea_recruitment()] is a
##' kind of master file that gets used by others when possible (could not for
##' herring), and just needs columns `year` and a column named
##' `index_statistic`, other information is supplied from other functions in the
##' arguments.
##'
##' [create_index.pacea_harbour_seals()] linearly interpolates the
##' roughly eight-monthly population estimates to the 1st January to give a
##' single value for each year. (Hake assessment outputs are also for the start
##' of the year, TODO herring I think might be 1st April).
##'
##' [create_index.pacea_oi()] works directly on the `oisst_month` data,
##' by using [calculate_anomalies.pacea_oi()] and then calculating a mean
##' anomaly averaged over the specified `months` specified and spatial `area` (one value for
##' each year)
##'
##' [create_index.pacea_oisst_anomalies_list()] works on the results of
##' [calculate_anomalies()] on `oisst_month` data, which can be used before
##' calculating the index  to show spatial variation. The resulting
##' [create_index()] on the results then
##' creates an annual index of the mean anomaly averaged over the specified `months`
##' and spatial `area` (one value for  each year).
##'
##' TODO add a test to make sure they match, and default months is always April.
##'
##' The index is normalised (subtract the mean and divide by the standard
##' deviation) across the specified years. This means that different types of
##' indices can be compared to each other. Indices are returned in a
##' standardised format for plotting. TODO: Medians are used if there is uncertainties
##' are given in the original data, though functions may have choices
##' @param data One of the data objects (or a reduced version that retains the
##' class of the object) in pacea:
##' Biological populations:
##' * `herring_recruitment`
##' * `herring_spawning_biomass`
##' * `hake_recruitment`
##' * `hake_biomass`
##' * `harbour_seals`
##' * `zooplankton_sog` (zooplankton data in the Strait of Georgia)
##' Oceanography:
##' * `buoy_sst` - sea-surface temperature from buoys
##' * `oisst_month` - optimally interpolated sea-surface temperature
##' @param ... arguments passed onto the respective `create_index.<class>`
##' function, such as:
##' @param years numeric vector years to restrict the data to, and then to
##' normalise over. If `NULL`, the default, then use the full range available
##' for the type of data being used. For `buoy_sst` this will be the available
##' data for the chosen `stn_id` buoy. TODO check what happens for other types
##' of data.
##' @param index_label character string for the name to be used as the label in
##' the plotting. Default is for hake recruitment, and will need to be manually
##' changed if doing another type of recruitment (except for herring, which is
##' automated; currently there are no other recruitments in pacea).
##' @param index_name character string for the name of the index. If `NULL` (default),
##' the name is derived from the variable name of `data`. Used internally when delegating
##' between `create_index` methods to preserve the original variable name.
##' @param index_statistic string for the column of `data` to use to create the
##' index. Default is `median` (for hake recruitment), but user can choose any
##' column of the data. When looking at harbour seals the function automatically
##' uses the `mean`. For `buoy_sst` it is either `anomalies` or
##' `mean`, to prescribe whether to create the index based on means of anomalies
##' (over the prescribed months) from a climatology, or just the means of the
##' actual SST values. These may well be the same anyway, or only differ if no
##' climatology can be built (which can depend on the years used for the
##' climatology). For `oisst_month` only `anomalies` (the default) can be used.
##' @param herring_region string for the region to be used when making an index related
##' to `herring_recruitment` or `herring_spawning_biomass`.
##' @param seal_region string for the region to be used when making an index related
##' to `harbour_seals`.
##' @param `zooplankton_species_group` string for the zooplankton species group
##' to be used from `zooplankton_sog`, must be one of the biological columns
##' (`total_biomass` onwards) of `zooplankton_sog`.
##' @param months vector of months (default `4` for April) for `buoy_sst` and
##' `oisst_month`
##' data, over which to average the values to create the annual index. See [plot.pacea_buoy_anomalies_list()] for full details of options
##' (e.g. `c(11, 12, 1, 2, 3)` for a winter average).
##' @param stn_id string of the station ID (`stn_id` value OR
##' `name` value from `buoy_metadata`) to create an index from the `buoy_sst` data
##' object.
##' @param require_requested_months number of months that must be available
##' out of the requested `months` to compute an average anomaly to then use to
##' create the index. If `NULL` (default), defaults to `length(months)`. See
##' [calculate_anomalies.pacea_buoy()] also for more options for `buoy_sst` data, such as the
##' minimum daily SST values required in a month to do the calculations.
##' (i.e. every month requested needs enough daily values).
##' @param ... arguments passed onto the respective method, and also onto
##' [calculate_anomalies.pacea_buoy()] for `buoy_sst` data. TODO test these
##'
##' @return tibble of class `pacea_standardised_index` containing columns of
##'   * `index` - name of the index
##'   * `year` - year of the value
##'   * `value` - the value of the index in that year
##'   * `index_label` - label to be used for plotting
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' create_index(hake_recruitment)
##' }
create_index <- function(data,
                         ...){
  UseMethod("create_index")
}
