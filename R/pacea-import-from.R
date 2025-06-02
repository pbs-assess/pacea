#' pacea package importFrom requirements
#'
#' Adapting from https://github.com/pbs-assess/gfiphc/blob/master/R/gfiphc.R
#' (see that if get issues with column names within dplyr functions for what to
#'  add). And from
#' https://github.com/andrew-edwards/sizeSpectra/blob/master/R/sizeSpectra.R for
#' some tibble related stuff.
#' And from the 'Consider adding' output from check(vignettes = FALSE), adding
#' here to then go into NAMESPACE
#'
#' @name pacea_import_from
## usethis namespace: start
#' @importFrom dplyr filter mutate summarise select group_by n arrange ungroup
#' @importFrom dplyr inner_join left_join right_join anti_join full_join
#' @importFrom dplyr semi_join row_number
#' @importFrom dplyr bind_rows case_when pull contains tibble rename as_tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats na.omit
#' @importFrom grDevices rgb
#' @importFrom graphics abline axis lines mtext par plot.default points polygon segments
#' @importFrom stats approx setNames
#' @importFrom utils tail type.convert
## usethis namespace: end
NULL

#' pacea: an R package of Pacific ecosystem information to help facilitate an
#'   ecosystem approach to fisheries management
#' @name pacea
#' @keywords internal
"_PACKAGE"
NULL
