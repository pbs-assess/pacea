##' Summarize a vector of months as a formatted string
##'
##' @param months numeric vector of month numbers (1-12)
##'
##' @return character string summarizing the months
##' @export
##'
##' @examples
##' summarise_months(4)
##' summarise_months(c(4, 5))
##' summarise_months(c(4, 5, 6, 7))
##' summarise_months(c(4, 5, 7))
##'
summarise_months <- function(months) {
  if (length(months) == 1) {
    return(month.abb[months])
  }

  # Check if months are sequential
  is_sequential <- all(diff(months) == 1)

  if (!is_sequential) {
    # Spell out all months: "Apr, May, and Jul"
    month_names <- month.abb[months]
    if (length(month_names) == 2) {
      return(paste(month_names[1],
                   "and",
                   month_names[2]))
    } else {
      return(paste0(paste(month_names[-length(month_names)],
                         collapse = ", "),
                   ", and ",
                   month_names[length(month_names)]))
    }
  }

  if (length(months) == 2) {
    return(paste(month.abb[months[1]], "and", month.abb[months[2]]))
  } else {
    return(paste(month.abb[months[1]], "to", month.abb[months[length(months)]]))
  }
}
