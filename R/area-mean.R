#' Isobath (200m) separated summary of values across an area
#' 
#' Estimate summary statistics across a spatial area, and separated by the 200m isobath that indicates inshore and offshore regions. 
#'
#' @param x pacea 'sf' data object with only one column of values (and a geometry column)
#'
#' @return list of summary statistics
#' 
#' @importFrom sf st_intersects st_drop_geometry 
#' @importFrom dplyr relocate last_col
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' pdata <- bccm_surface_temperature()
#' 
#' yrs <- c(2019)
#' mths <- c(8) # august
#' ym <- paste(yrs, mths, sep = "_")
#' 
#' tdat <- pdata %>% dplyr::select(all_of(ym))
#' 
#' area_mean(tdat)
#' }
area_mean <- function(x){
  
  # must be sf feature
  stopifnot("'x' must be of class 'sf'" = 'sf' %in% class(x))
  
  # only two columns, geometry and value column
  stopifnot("'x' must have only one column of values" = ncol(x) == 2)
  
  # inshore and offshore polygon 
  ipoly <- make_isoshape(inshore = TRUE)
  
  # order columns correctly
  x2 <- x %>% 
    dplyr::relocate(geometry, .after = dplyr::last_col()) %>%
    st_transform(crs = "EPSG: 3005")

  # rename value column
  colnames(x2)[1] <- "value"
  
  # vector for inshore values
  pts_in <- lengths(st_intersects(x2, ipoly)) > 0 
  
  # all area
  sum_all <- x2 %>% 
    st_drop_geometry() %>%
    summarise(mean = mean(value, na.rm = TRUE),
              median = median(value, na.rm = TRUE),
              sd = sd(value, na.rm = TRUE),
              n = sum(!is.na(value))) %>%
    as.data.frame()
  
  # isobath inshore
  sum_in <- x2[pts_in,] %>% 
    st_drop_geometry() %>%
    summarise(mean = mean(value, na.rm = TRUE),
              median = median(value, na.rm = TRUE),
              sd = sd(value, na.rm = TRUE),
              n = sum(!is.na(value))) %>%
    as.data.frame()
  
  # isobath offshore 
  sum_out <- x2[!pts_in,] %>% 
    st_drop_geometry() %>%
    summarise(mean = mean(value, na.rm = TRUE),
              median = median(value, na.rm = TRUE),
              sd = sd(value, na.rm = TRUE),
              n = sum(!is.na(value))) %>%
    as.data.frame()
  
  output <- list('All area' = sum_all, 
                 'Inshore area' = sum_in, 
                 'Offshore area' = sum_out)
  
  return(output)
}


#' function to create in/off shore isobath polygon with the bccm_eez_polygon
#' 
#' @importFrom sf st_bbox st_coordinates st_as_sf st_combine st_cast st_intersection st_difference
#' 
#' @noRd
make_isoshape <- function(inshore = TRUE){
  
  # bounding box of the BCCM eez polygon (masked to eez, buffer, and roms extent)
  roms_bbox <- st_bbox(bccm_eez_poly)
  
  # getting limits of data points
  pt_crds <- as.data.frame(st_coordinates(isobath_200m)[, c(1,2)])
  
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
    st_as_sf(coords = c("X", "Y"), crs = "EPSG: 3005") %>% 
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON") %>%
    st_intersection(bccm_eez_poly)
  
  if(inshore){
    return(in_poly)
  } else {
    out_poly <- bccm_eez_poly %>%
      st_difference(in_poly)
    return(out_poly)
  }
}
