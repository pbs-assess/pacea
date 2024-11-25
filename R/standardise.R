##' Standardise a vector of values to a mean of 0 and standard deviation of 1
##'
##' @param x numeric
##' @param range indices for which the mean and standard deviation should be
##'   calculated over (default is to use all of `x`)
##' @return numeric with standardised value corresponding to each value of `x`
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' standardise((-5):5)
##' standardise((-5):5, range = 1:5)   # 3rd value becomes 0
##' # Say you want to standarised the North Pacific Current Bifurcation Index to
##'   years only up to and including 2020:
##' bi_standardise_to_2020 <- dplyr::mutate(bi,
##'                                         anomaly_2020 = standardise(value,
##'                                         range = 1:which(bi_new$year == 2020)))
##' mean(bi_standardise_to_2020$anomaly_2020)   # Not zero because includes
##'   years after 2020.
##' mean(dplyr::filter(bi_standardise_to_2020, year <= 2020)$anomaly_2020) # zero
##' }
standardise <- function(x,
                        range = 1:length(x)){
  x_mean <- mean(x[range])
  x_sd <- sd(x[range])

  (x - x_mean)/x_sd
}
