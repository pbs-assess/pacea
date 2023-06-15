# plot pacea_st function

#' Title
#'
#' @param obj a `pacea_st` object, which is an `sf` object
#' @param months character or numeric vector to indicate which months to include (e.g. `c(1, 2)`, `c("April", "may")`, `c(1, "April")`)
#' @param years vector of years to include, from 1993 to 2019
#' @param bc logical. Should BC coastline layer be plotted? Can only be plotted with one plot layer.
#' @param eez logical. Should BC EEZ layer be plotted? Can only be plotted with one plot layer.
#' @param ... optional arguments passed on to `plot.sf()`
#'
#' @return plot of the spatial data to the current device (returns nothing)
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' dat <- roms_surface_temperature()
#' plot(dat)
#' }
plot.pacea_st <- function(obj,
                          months = c("April"),
                          years = c(1993, 1998, 2003, 2008, 2013, 2018),
                          bc = FALSE, 
                          eez = FALSE,
                          ...) {
  
  stopifnot("obj must be of class `sf`" =
              "sf" %in% class(obj))
  
  
  month_table <- data.frame(month.name = month.name,
                            month.abb = month.abb,
                            month.num = 1:12)
  
  # indexing time for selection of plots to show
  obj_names <- matrix(as.numeric(unlist(strsplit(names(obj %>% st_drop_geometry()), split = "_"))), ncol = 2, byrow = TRUE)
  
  stopifnot("Must enter valid numerals for 'years'" = !any(is.na(suppressWarnings(as.numeric(years)))))
  
  # object units attribute
  obj_unit <- attributes(obj)$units
  
  
  # indexing month and year
  tind <- obj_names[obj_names[,1] %in% as.numeric(years) &
                      obj_names[,2] %in% month_index(months), , drop = FALSE] %>% as.data.frame()
  tind <- merge(tind, month_table, by.x = "V2", by.y = "month.num", sort = FALSE)[,c("V1", "V2", "month.name", "month.abb")] %>%
    arrange(V1, V2)

  tind1 <- do.call(paste, c(tind[c("V1", "V2")], sep = "_"))
  tind2 <- do.call(paste, c(tind[c("V1", "month.abb")], sep = "_"))
  
  tobj <- obj[, tind1]
  names(tobj) <- c(tind2, "geometry")
  
  plot(tobj, border = NA, key.pos = 4, reset = FALSE, ...)
  
  if(ncol(tobj) == 2){
    mtext(text = obj_unit, side = 4, line = 0)
    if(bc == TRUE){
      plot(bc_coast, border = "grey50", col = "grey80", add = TRUE,)
    }
    if(eez == TRUE){
      plot(bc_eez, border = "black", col = NA, lty = 2, add = TRUE)
    }
  } else {
    mtext(text = obj_unit, side = 4, line = -4)
  }
}





#' function to index months in plot argument
#' @noRd
month_index <- function(month = month) {
  
  month_table <- data.frame(month.name = month.name,
                            month.abb = month.abb,
                            month.num = 1:12)
  
  m_ind <- vector()
  
  for(imonth in month) {
    if(is.na(suppressWarnings(as.numeric(imonth)))){
      tind <- as.vector(unlist(apply(month_table, 2, function(x) {
        grep(pattern = imonth, x = x, ignore.case = TRUE)
      })))
      if(length(unique(tind)) != 1) stop("Month name incorrect")
      
      m_ind <- c(m_ind, unique(tind))
    } else {
      as.numeric(imonth)
      
      tind <- which(month_table$month.num == as.numeric(imonth))
      
      m_ind <- c(m_ind, unique(tind))
    }
  }
  
  return(m_ind)
}


