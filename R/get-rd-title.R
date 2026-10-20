##' Extract title from an .Rd help file
##'
##' Reads a .Rd (R documentation) file from the man/ directory and extracts
##' the title from the \title{} field. Useful for programmatically accessing
##' help file metadata.
##'
##' If the .Rd file for object_name does not exist, searches data.R for the
##' object. If the object has an @rdname directive pointing to another object,
##' looks in that object's .Rd file for an \item{object_name}{description} line
##' and extracts the description.
##'
##' @param object_name Character string of the object name (e.g., "sasquatch").
##'   The function looks for the corresponding .Rd file in the man/ directory
##'   of the development version.
##'
##' @return Character string containing the title from the .Rd file, or
##'   the description from an \item{} entry, or NA_character_ if not found.
##'   The first word is capitalised.
##'
##' @examples
##' \dontrun{
##'   get_rd_title("sasquatch")
##'   get_rd_title("inshore_poly")
##' }
##'
##' @export
get_rd_title <- function(object_name){
  capitalize_first <- function(x){
    if(is.na(x)) return(x)
    paste0(toupper(substr(x, 1, 1)),
           substr(x, 2, nchar(x)))
  }

  rd_file <- file.path(here::here(),
                       "man",
                       paste0(object_name, ".Rd"))

  if(file.exists(rd_file)){
    rd_content <- readLines(rd_file)
    title_line <- grep("^\\\\title\\{",
                       rd_content,
                       value = TRUE)

    if(length(title_line) > 0){
      title_text <- sub("^\\\\title\\{", "", title_line)
      title_text <- sub("\\}$", "", title_text)
      return(capitalize_first(title_text))
    }
  }

  data_file <- file.path(here::here(),
                         "R",
                         "data.R")
  if(!file.exists(data_file)){
    return(NA_character_)
  }

  data_content <- readLines(data_file)

  object_pattern <- paste0('"', object_name, '"')
  object_line_idx <- grep(object_pattern, data_content)

  if(length(object_line_idx) == 0){
    return(NA_character_)
  }

  rdname_idx <- NA
  for(i in (object_line_idx[1] - 1):1){
    if(grepl("^#'\\s*@rdname", data_content[i])){
      rdname_idx <- i
      break
    }
  }

  if(is.na(rdname_idx)){
    return(NA_character_)
  }

  rdname_line <- data_content[rdname_idx]
  rdname_object <- sub("^#'\\s*@rdname\\s+", "", rdname_line)
  rdname_object <- trimws(rdname_object)

  rdname_file <- file.path(here::here(),
                           "man",
                           paste0(rdname_object, ".Rd"))
  if(!file.exists(rdname_file)){
    return(NA_character_)
  }

  rdname_content <- readLines(rdname_file)
  item_pattern <- paste0("^\\\\item\\{", object_name, "\\}")
  item_line <- grep(item_pattern, rdname_content, value = TRUE)

  if(length(item_line) == 0){
    return(NA_character_)
  }

  title_text <- sub(paste0("^\\\\item\\{", object_name, "\\}\\{"),
                    "",
                    item_line)
  title_text <- sub("\\}$", "", title_text)
  return(capitalize_first(title_text))
}
