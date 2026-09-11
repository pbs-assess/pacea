##' Create an annual standardised index based on a pacea object
##'
##' Create an annual index based on a pacea object. The annual value can be
##' calculated from annual values, such as `hake_recruitment`, or averaged for
##' specific monthly values (e.g. PDO for Apr to Sep). See the examples and
##' vignette TODO
##' Calculations are based on the class of the object: [create_index()] is a generic function that calls `create_index.<class>()`
##' where `<class>` is `class(data)`. [create_index.pacea_recruitment()] is a
##' kind of master file that gets used by others when possible (could not for
##' herring). [create_index.pacea_harbour_seals()] linearly interpolates the
##' roughly eight-monthly population estimates to the 1st January to give a
##' single value for each year. (Hake assessment outputs are also for the start
##' of the year, TODO herring I think might be 1st April).
##'
##' The index is normalised (subtract the mean and divide by the standard
##' deviation) across the specified years. This means that different types of
##' indices can be compared to each other. Indices are returned in a
##' standardised format for plotting. TODO: Medians are used if there is uncertainties
##' are given in the original data, though functions may have choices
##' @param data One of: TODO
##' * `hake_recruitment` (of class `pacea_recruitment`).
##' @param ... arguments passed onto the respective `create_index.<class>`
##' function, such as:
##' @param years numeric vector years to restrict the data to, and then to normalise over.
##' @param index_label character string for the name to be used as the label in
##' the plotting. Default is for hake recruitment, and will need to be manually
##' changed if doing another type of recruitment (except for herring, which is
##' automated; currently there are no other recruitments in pacea).
##'
##' @param index_name character string for the name of the index. If `NULL` (default),
##' the name is derived from the variable name of `data`. Used internally when delegating
##' between `create_index` methods to preserve the original variable name.
##' @param index_statistic string for the column of `data` to use to create the
##' index. Default is `median` (for hake recruitment), but user can choose any
##' column of the data. When looking at harbour seals the function automatically
##' uses the `mean`.
##' @param herring_region string for the region to be used when making an index related
##' to `herring_recruitment` or TODO
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
