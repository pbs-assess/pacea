##' Create an index time series based on hake recruitment estimates.
##'
##' TODO put all the helps into create_index() when done.
##'
##' pacea_recruitment is just for hake, should add hake at the end at some
##' point maybe. TODO
##' Create an annual index based on estimates of `hake_recruitment` saved in
##' pacea.
##'
##' years identify the vector of years to restrict to, and then to normalise over.
##'
##' @param data One of: TODO
##' * `size_spectrum_mlebins` object
##' * `determine_xmin_and_fit_mlebins` object
##' * `remove_outliers_mlebins` object
##' @param ... arguments passed onto the respective `detect_outliers.<class>` function
##' @return tibble containing columns of
##'   * `index` - name of the index
##'   * `year` - year of the value
##'   * `index_plot_value` - the value of the index in that year
##' @export
##' @author Andrew Edwards
##' @rdname create_index
##' @examples
##' \dontrun{
##' create_index(hake_recruitment)
##' }
create_index.pacea_recruitment <- function(data,
                                           years = NULL,
                                           index_label = "Hake recruitment",
                                           ...){
  if(is.null(years)){
    years <- min(data$year):max(data$year)
  }

  res <- dplyr::filter(data,
                       year %in% years)

  value_mean <- mean(res$median)
  value_sd <- sd(res$median)

  index_name <- deparse(substitute(data))
  res <- res %>%
    dplyr::mutate(index_plot_value =
                    (median - value_mean) / value_sd,
                  index = index_name) %>%
    dplyr::select(index,
                  year,
                  index_plot_value)

  res
}
