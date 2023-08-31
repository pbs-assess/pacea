
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
#' get_pacea_data("roms_surface_temperature")
#' }
#' 
pacea_clear_cache <- function() {
  
  cache_dir <- pacea_cache()
  
  cache_list <- list.files(pacea_cache())
  
  ## interactive ask to delete files from cache
  ans <- ask(paste("About to delete ALL data files in cache directory:",
                   cache_dir, "Is that okay?", sep = "\n"))
  
  if (!ans) stop("Exiting...", call. = FALSE)
  
  unlink(paste0(cache_dir,"/roms_*"))
  
  cat("All ROMS data files removed from cache.")
}

