#' Isobath (200m) separated summary of values across an area
#'
#' Estimate summary statistics across a spatial area, and separated by the 200m isobath that indicates inshore and offshore regions.
#'
#' @param x pacea 'sf' data object with only one column of values (and a geometry column)
#'
#' @return list of summary statistics
#'
#' @importFrom sf st_intersects st_drop_geometry st_bbox st_intersection st_transform
#' @importFrom dplyr relocate last_col
#' @importFrom stats median sd
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
