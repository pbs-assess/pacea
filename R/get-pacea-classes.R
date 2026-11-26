##' Create a tibble of unique pacea classes from package data objects
##'
##' Scans all data objects in the pacea package, extracts their classes,
##' filters for classes starting with "pacea_", and returns a tibble with
##' Class and Description columns. This is useful for documenting the custom
##' S3 classes used throughout the package.
##'
##' @return A tibble with two columns:
##' \describe{
##'   \item{Class}{Character vector of unique class names (sorted)}
##'   \item{Description}{Character vector of NA values (placeholder for user annotations)}
##' }
##' @examples
##' \dontrun{
##'   pacea_classes_df <- get_pacea_classes()
##'   print(pacea_classes_df)
##' }
##'
##' @export
get_pacea_classes <- function(){
  # Get list of data objects in the package
  data_obj_list <- data(package = "pacea")$results[, "Item"]

  # Get classes for all data objects
  all_classes <- character()

  for(obj_name in data_obj_list){
    obj <- get(obj_name)
    classes <- class(obj)
    all_classes <- c(all_classes, classes)
  }

  # Get unique classes in sorted order
  unique_classes <- unique(all_classes)
  unique_classes <- sort(unique_classes)

  # Keep only classes that start with "pacea_"
  unique_classes <- unique_classes[grepl("^pacea_", unique_classes)]

  # Create tibble with Class and Description columns
  pacea_classes <- tibble::tibble(
    Class = unique_classes,
    Description = NA_character_
  )

  pacea_classes
}
