#' pacea package dplyr-related things
#'
#' Adapting from https://github.com/pbs-assess/gfiphc/blob/master/R/gfiphc.R
#' (see that if get issues with column names within dplyr functions for what to
#'  add). And from
#' https://github.com/andrew-edwards/sizeSpectra/blob/master/R/sizeSpectra.R for
#' some tibble related stuff.
#'
#' @docType package
#' @name gfiphc
#' @importFrom dplyr filter mutate summarise select group_by n arrange ungroup
#' @importFrom dplyr inner_join left_join right_join anti_join full_join
#' @importFrom dplyr semi_join row_number
#' @importFrom dplyr bind_rows case_when pull contains tibble rename as_tibble
NULL
