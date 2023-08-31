
#' pacea spatiotemporal data convert to long/wide format
#' 
#' Simplified function for users, taking the `tidyr::pivot_longer` and `tidyr::pivot_wider` functions to transform pacea.st data into long or wide format for simpler data manipulation, summarising, and plotting. Writted to only work with 'pacea.st' (spatiotemporal) data.
#'
#' @param data pacea.st data object in wide (default) format to pivot.
#' @param names_to A character value for the column name representing the date. Defaults to separate 'year' and 'month' for BCCM data.
#' @param values_to A character value for the column name representing variable data (e.g. temperature)
#' 
#' @import dplyr
#' @importFrom tidyr pivot_longer pivot_wider
#'
#' @return pacea spatiotemporal data in long/wide format
#' @export
#'
#' @examples
#' \dontrun{
#' pdata <- bccm_surface_temperature()[,1:2]
#' pdata_long <- pacea_long(pdata)
#' head(pdata_long)
#' }
pacea_long <- function(data, names_to = "date", values_to = "value") {
  
  # object size > 100 MB
  if(object.size(data) > 100*10^6) {
    ans <- ask("Current object size may result in a long format of data >1GB (and upwards of 10GB), which may affect local computational performance. Do you wish to continue?")
    
    if (!ans) stop("Exiting...", call. = FALSE)
  }
           
  dat <- data %>% 
    tidyr::pivot_longer(cols = !last_col(), cols_vary = "slowest", names_to = names_to, values_to = values_to)  %>%
    mutate(year = substr(get(names_to), 1, 4),
           month = substr(get(names_to), 6, 7)) %>%
    select(-date) %>%
    relocate(value, .after = last_col()) %>%
    relocate(geometry, .after = last_col()) 
    
  return(dat)
}


#' pacea spatiotemporal data convert to wide format
#'
#' @param data pacea.st data object in long format to pivot.
#' @param names_from A character vector representing the date from which to name columns from. If vector is greater than 1, column values will be pasted together, separated by '_'. Defaults to paste BCCM data in 'year_month' format.
#' @param values_from A character value representing the column names from which to get variable data values.
#'
#' @export
#' @rdname pacea_long
#'
#' @examples
#' \dontrun{
#' pdata <- bccm_surface_temperature()[,1:2]
#' pdata_long <- pacea_long(pdata)
#' pdata_wide <- pacea_wide(pdata)
#' head(pdata_wide)
#' }
pacea_wide <- function(data, names_from = c("year", "month"), values_from = "value") {
  
  # errors
  if(!any(names_from %in% colnames(data))) {
    stop("invalid 'names_from' values. Columns do not exist in data")
  }
  
  if(length(values_from) != 1) {
    stop("'values_from' argument must be of vector length = 1")
  }
  
  if(!values_from %in% colnames(data)) {
    stop("invalid 'values_from' column. Column does not exist in data")
  }
  
  # paste values from date for column names of wide format
  tdat <- data[, names_from] %>% st_drop_geometry()
  out.vec <- as.vector(unlist(tdat[, 1]))
  
  if(length(names_from) > 1) {
    for(i in 2:length(names_from)) {
      out.vec <- paste(out.vec, as.vector(unlist(tdat[, i])), sep = "_")
    }
  }
  
  dat <- data %>% 
    mutate(date = out.vec) %>%
    tidyr::pivot_wider(id_cols = "geometry", names_from = "date", values_from = values_from) %>%
    relocate(geometry, .after = last_col()) 
  
  return(dat)
}

