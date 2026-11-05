##' Package environment for storing session-level state
##'
##' Internal environment used to track whether startup messages have been shown
##' in the current session.
##'
##' @keywords internal
.pacea_env <- new.env()

##' Pacea package startup hook
##'
##' Runs automatically when the package is loaded (via \code{library(pacea)} or
##' \code{load_all()}). Handles timezone configuration for users in the
##' "British Columbia Standard Time" timezone.
##'
##' R does not recognise the newer "British Columbia Standard Time" timezone
##' designation on Windows systems. This hook automatically sets the timezone
##' to "Etc/GMT+7" for affected users to prevent warnings. The message is only
##' shown once per session.
##'
##' @param libname The library directory where the package is installed
##' @param pkgname The name of the package being loaded
##'
##' @keywords internal
.onLoad <- function(libname, pkgname){
  if(Sys.timezone() == "British Columbia Standard Time"){
    Sys.setenv(TZ = "Etc/GMT+7")
    if(!isTRUE(.pacea_env$tz_message_shown)){
      packageStartupMessage("Timezone automatically set to Etc/GMT+7 for British Columbia Standard Time, see https://github.com/pbs-assess/pacea/issues/107")
      .pacea_env$tz_message_shown <- TRUE
    }
  }
}
