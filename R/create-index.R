##' Create an annual standardised index based on a pacea object
##'
##' Create an annual index based on a pacea object. The annual value can be
##' tailored to be anything (e.g. PDO for Apr to Sep). See the examples and
##' vignette TODO
##' Calculations are based on the class of the object.
##'
##' The index is normalised (subtract the mean and divide by the standard
##' deviation) across the specified years. This means that different types of
##' indices can be compared to each other. Indices are returned in a
##' standardised format for plotting. TODO: Medians are used if there is uncertainties
##' are given in the original data, though functions may have choices
##'
##' @param data One of: TODO
##' * `size_spectrum_mlebins` object
##' * `determine_xmin_and_fit_mlebins` object
##' * `remove_outliers_mlebins` object
##' @param ... arguments passed onto the respective `create_index.<class>` function
##' @return tibble containing columns of
##'   * `index` - name of the index
##'   * `year` - year of the value
##'   * `value` - the value of the index in that year
##'   * `index_label` - label to be used for plotting
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' # See ?remove_outliers for link to example code
##' }
create_index <- function(data,
                                ...){
  UseMethod("create_index")
}
