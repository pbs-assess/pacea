##' TODO this is for `calculate_climatology()`. Edit whole help file.
##' If already run through `remove_outliers.size_spectrum_mlebins()` then it wil
##' use `detect_outliers.remove_outliers.mlebins()` and can keep iterating
##' that. See [remove_outliers] for further details and example code.
##'
##' @param res One of:
##' * `size_spectrum_mlebins` object
##' * `determine_xmin_and_fit_mlebins` object
##' * `remove_outliers_mlebins` object
##' @param ... arguments passed onto the respective `detect_outliers.<class>` function
##' @return tibble containing just the data values needed for
##' calculations, which are the original four columns:
##'   * `species`
##'   * `bin_min`
##'   * `bin_max`
##'   * `bin_count`
##' plus
##'   * `gap` which is `bin_min` minus the previous `bin_min`
##'   * `gap_ratio` which is the ratio of `gap` to the second largest gap, so makes
##' it easy to see if the largest gap is much much larger than the second largest.
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' # See ?remove_outliers for link to example code
##' }
calculate_climatology <- function(data,
                                  ...){
  UseMethod("calculate_climatology")
}
