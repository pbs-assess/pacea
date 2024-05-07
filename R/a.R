##' Shorthand for `as.data.frame()` for printing all of a tibble.
##'
##' @details Often want to print all of a (small) tibble, but `as.data.frame()`
##'   is cumbersome.
##'
##' @param tib `tibble` to be printed to console, R Markdown file, etc.
##'
##' @return print `a` as a data.frame, so show all the rows
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' # To see all the years (rows of a long tibble):
##' oni %>% a()
##'
##' # To see all the species (columns of a wide tibble):
##' tail(zooplankton_sog) %>% a()
##' }
##' @export
a <- function(tib){
  as.data.frame(tib)
}
