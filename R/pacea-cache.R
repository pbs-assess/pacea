##' Get directory of where to cache large data sets
##'
##' Some data sets are too large to save as data objects within pacea and need
##' to be cached locally; this gives the default location.
##'
##' Some data sets need to be cached locally and will not be needed by all users
##' (and so do not really need to be stored as R data objects in the
##' project). Also, some large ones may be regularly updated. When such data are
##' needed, we want to cache them locally rather than download them from an
##' external website each time pacea is used in a new R session, and make them
##' easy to update.
##'
##' @return the cache directory on the user's machine, based on
##'   `rappdirs::user_cache_dir("pacea")`. On Windows in 2023 this seems to be
##'   `C:\\Users\\<username>\\AppData\\Local\\pacea\\Cache`. If you want to
##'   put use something else then see `?rappdirs::user_cache_dir` and change
##'   your `R_USER_CACHE_DIR`. To do that see
##'   `https://github.com/r-lib/rappdirs/issues/34`.
##'
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' pacea_cache()
##' }
pacea_cache <- function(){
  rappdirs::user_cache_dir(appname = "pacea",
                           appauthor = NULL)
}
