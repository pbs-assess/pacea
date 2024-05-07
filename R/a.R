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
##' # See all the years (rows)
##' oni %>% a()
##' # Want to see all the species (columns)
##' tail(zooplankton_sog) %>% a()
##' }
##' @export
a <- function(tib){
  as.data.frame(tib)
}
