
#' pacea clear cache
#'
#' Delete all data files in cache folder.
#'
#' This function will prompt the user on whether to delete all files in the cache directory. This cannot be undone and the user will have to download the files again.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' bccm_avg0to40m_oxygen()
#' list.files(pacea_cache())
#' pacea_clear_cache()
#' }
#'
pacea_clear_cache <- function() {

  cache_dir <- pacea::pacea_cache()

  cache_list <- list.files(pacea_cache())

  ## interactive ask to delete files from cache
  ans <- ask(paste("About to delete ALL data files in cache directory:",
                   cache_dir, "Is that okay?", sep = "\n"))

  if (!ans) stop("Exiting...", call. = FALSE) # nocov

  pat <- "/bccm_*"

  # calling scope - testthat detection
  tb <- .traceback(x = 0)
  if(any(unlist(lapply(tb, function(x) any(grepl("test_env", x)))))){
    pat <- "/test_*"
  }

  unlink(paste0(cache_dir, pat))

  cat("All BCCM data files removed from cache.")
}
