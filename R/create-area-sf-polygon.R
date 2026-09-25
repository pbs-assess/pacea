##' Create an sf polygon object for an area described by lon and lat values
##'
##' The spatial data can be easily indexed using sf (spatial features) objects. This function
##' converts a data.frame with columns `lat` and `lon` into a an sf
##' polygon, with the required lat-lon WGS 84 projection (4326).
##' Users should plot the resulting object to make sure it looks
##' correct.
##'
##' @param data a 2-column matrix with longitude in the first column and
##' latitude in the second, or a data.frame with named columns `lat` and `lon`,
##' representing a
##' polygon (continuous outline, no doughnuts with holes in). If it does not
##' create an enclosed polygon then the first point is then repeated at the end
##' to make a polygon.
##' @return sf polygon object using co-ordinate reference system WGS84 (4326).
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' pfma_area_126_lat_lon_matrix <-
##'   matrix(c(-127.1506, -128.2331, -129.3492, -127.9167, -127.1847, -126.8200, -127.1506,
##'     49.85766, 49.00000, 48.99991, 50.11915, 50.40183, 50.24466, 49.85766),
##'     ncol = 2)
##' pfma_area_126_from_scratch <- create_area_sf_polygon(pfma_area_126_lat_lon_matrix)
##' plot(pfma_area_126_from_scratch)    # just draws the polygon of the area
##' # Plot the latest OISST data plus draw the polygon of the area
##' p <- plot(oisst_month)
##' p + geom_sf(data = pfma_area_126_from_scratch,
##'             fill = NA,
##'             colour = "black",
##'             linewidth = 1)
##' # Plot the latest OISST data just for the area
##' # TODO add area to plot.pacea_oi
##' }
##'
create_area_sf_polygon <- function(data){
  if("tbl_df" %in% class(data)){
    stopifnot("lon" %in% names(data) & "lat" %in% names(data))

    area_as_list_of_matrix <- list(
      matrix(data[, "lon"],
             data[, "lat"],
             ncol = 2))
  }

  if("matrix" %in% class(data)){
    area_as_list_of_matrix <- list(data)
  }

  # Create polygon object with lat-lon WGS 84 projection (4326)
  area_polygon <- sf::st_sfc(sf::st_polygon(area_as_list_of_matrix),
                             crs = 4326) %>%
    sf::st_as_sf()

  area_polygon
}
