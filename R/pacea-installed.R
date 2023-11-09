##' Check the date of the last local installation of `pacea`
##'
##' Run this and then compare the date with the recent updates of `pacea`, given
##' by the dates of the commits at `https://github.com/pbs-assess/pacea/commits/main`
##'
##' @return Prints text giving days since last local installation, and directs
##'   users to website to check the latest updates on GitHub.
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' pacea_installed()
##' }
pacea_installed <- function(){
  last_installed <- file.info(paste0(.libPaths(), "/pacea/DESCRIPTION"))$ctime %>%
                                                                       lubridate::as_date()
  last_installed_days_ago <- as.numeric(lubridate::today() - last_installed)
  writeLines(paste0("You last locally installed pacea on ", last_installed, " which is ",
         last_installed_days_ago,
         " days ago.\nCompare that with the dates of the latest commits at https://github.com/pbs-assess/pacea/commits/main"))
}
