##' Restrict a pacea spatial or spatiotemporal object to a given area, retaining
##' the class of the original object
##'
##' @param data sf object of class `pacea_oi` TODO list them all
##' @param area sf object of a polygon for which to restrict the spatial data
##' to. Restriction is based on the centres of the cells of the data being
##' inside the area (thanks Kelsey!).
##'
##' @return object of the same class as `data`, but with spatial values only
##' within the defined area
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' sst_sasquatch <- restrict_to_area(oisst_month,
##'                                   sasquatch)
##' plot(sst_sasquatch, years = 2015:2016, months = 1:12)   # Sasquatch in a marine heatwave)
##' }
restrict_to_area <- function(data,
                             area){
  if(!("sf" %in% class(data)) | !("sf" %in% class(area))){
    stop("data and area both need to be sf objects in `restrict_to_area()`")
  }

  ret <- data[area, ]
  class(ret) <- class(data)

  ret
}


