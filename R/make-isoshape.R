#' function to create in/off shore isobath polygon with the bccm_eez_polygon
#' @noRd
make_isoshape <- function(inshore = TRUE){

  requireNamespace("sf", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)

  # bounding box of the BCCM eez polygon (masked to eez, buffer, and roms extent)
  roms_bbox <- sf::st_bbox(bccm_eez_poly)

  # getting limits of data points
  pt_crds <- as.data.frame(sf::st_coordinates(isobath_200m)[, c(1,2)])

  pt1 <- pt_crds[which(pt_crds$Y == max(pt_crds$Y)),] %>%
    mutate(X = X,
           Y = Y + 5000)
  pt2 <- pt_crds[which(pt_crds$Y == min(pt_crds$Y)),] %>%
    mutate(X = X,
           Y = Y - 5000)
  pt3 <- data.frame(X = roms_bbox[3] + 5000,
                    Y = pt2$Y)
  pt4 <- data.frame(X = roms_bbox[3] + 5000,
                    Y = pt1$Y)

  in_poly <- pt2 %>%
    rbind(pt_crds, pt1, pt4, pt3, pt2) %>%
    sf::st_as_sf(coords = c("X", "Y"), crs = "EPSG: 3005") %>%
    summarise(geometry = sf::st_combine(geometry)) %>%
    sf::st_cast("POLYGON") %>%
    sf::st_intersection(bccm_eez_poly)

  if(inshore){
    return(in_poly)
  } else {
    out_poly <- bccm_eez_poly %>%
      sf::st_difference(in_poly)
    return(out_poly)
  }
}
