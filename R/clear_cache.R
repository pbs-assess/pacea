
#' ROMS clear cache
#'
#' Delete all ROMS data files in cache folder
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' get_pacea_data("roms_surface_temperature")
#' }
#' 
roms_clear_cache <- function() {
  
  cache_dir <- pacea_cache()
  
  ## interactive ask to delete files from cache
  ans <- ask(paste("About to delete ALL ROMS data files in cache directory:",
                   cache_dir, "Is that okay?", sep = "\n"))
  
  if (!ans) stop("Exiting...", call. = FALSE)
  
  unlink(paste0(cache_dir,"/roms_*"))
  
  cat("All ROMS data files removed from cache.")
}

