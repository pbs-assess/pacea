##' Average ROMS output over a geographic area to give a time series
##'
##' Given some ROMS output and a geographic area, average the values (such as
##' sea surface temperature or bottom oxygen) over the area to give a monthly
##' time series.
##'
##' @param pacea_st_obj class `pacea_st` object, such as from ROMS output
##'   or satellite sea surface temperature.
##' @param area a `list` object of numeric matrices (likely just one matrix)
##'   with points in rows (see example) as longitude (negative) and latitude, to
##'   be an input to `sf::st_polygon()`. Default gives an example area, based on
##'   statistical fishing area 126 TODO double check definition.
##' @return tibble with columns year, month, and value, similar to the oceanic indices
##'   (`oni` etc. but with no anomaly column; anomalies can be defined by the
##'   user if desired).
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' roms_sst <- roms_surface_temperature()
##' avg <- roms_average(roms_sst)
##' plot.pacea_index(avg, value = "value", ylab = "Average ROMS SST over area", style = "plain")
##' }
roms_average <- function(pacea_st_obj,
                         area = list(matrix(c(-127.1506, -128.2331, -129.3492, -127.9167, -127.1847, -126.8200, -127.1506,
                                              49.85766, 49.00000, 48.99991, 50.11915, 50.40183, 50.24466, 49.85766),
                                            ncol = 2))
                         ){
  stopifnot("pacea_st_obj must be of class pacea_st" =
              ("pacea_st" %in% class(pacea_st_obj)))

  # convert area to a simple features object, with the correct projection
  area_sf <- sf::st_sfc(sf::st_polygon(area),
                        crs = 4326) %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = 3005)

  # this filters to just the required area
  obj_area <- pacea_st_obj[area_sf, ]

  obj_area_drop <- sf::st_drop_geometry(obj_area) %>%
    as_tibble()

  avg <- colMeans(obj_area_drop)

  obj_area_tib <- tibble::tibble(value = avg)

  obj_area_tib$year <- as.numeric(substr(names(avg),
                                         1,
                                         4))
  obj_area_tib$month <- as.numeric(substr(names(avg),
                                          6,
                                          7))

  obj_area_tib <- dplyr::relocate(obj_area_tib,
                                  year,
                                  month)
  obj_area_tib
}
