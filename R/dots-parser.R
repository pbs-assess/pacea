##' Allow a function to parse out the ... values into two separate functions,
##'  without giving a warning.
##'
##' Taken from
##'   https://stackoverflow.com/questions/4124900/is-there-a-way-to-use-two-statements-in-a-function-in-r. So
##' within a larger function that has `...`, use this to send the appropriate
##' values to two (or more) further functions. The values can go to both further
##' functions. Taken from sizeSpectraFit.
##' @param FUN function to be run with arguments from `...`
##' @param ... arguments to be passed onto `FUN`
##' @return appropriately evaluates `FUN`
##' @export
##' @author Mike Blazanin in the Stack Overflow post referenced above, written up here by Andrew Edwards
##' @examples
##' \dontrun{
##' # Code within a function would have previously looked like:
##' func1(a = myvaluea, b = myvalueb, ...)
##' But that returns an error because ... includes arguments not accepted by func1, this now works:
##' dots_parser(func1, a = myvaluea, b = myvalueb, ...)
##' }
dots_parser <- function(FUN, ...) {
  argnames <- names(formals(FUN))
  dots <- list(...)

  return(do.call(FUN,
                 dots[names(dots) %in% argnames]))
}
