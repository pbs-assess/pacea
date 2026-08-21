##' Summarize a vector of months as a formatted string
##'
##' @param months numeric vector of month numbers (1-12)
##'
##' @param just_first_to_last force function to return just the first and last
##' months in of `months`; generally wouldn't have gaps anyway
##' @return character string summarizing the months
##' @export
##'
##' @examples
##' summarise_months(4)             # "Apr"
##' summarise_months(c(4, 5))       # "Apr" and "May"
##' summarise_months(4:6)           # "Apr to Jun"
##' summarise_months(c(12, 1:3))    # "Dec, Jan, Feb, and Mar"
##' summarise_months(c(12, 1:3),
##'                  just_first_to_last = TRUE)
##'                                 # "Dec to Mar"
##' Generally thinking we would have consecutive months, but can still spell
##' them out even if not:
##' summarise_months(c(12, 4:8))    #  "Dec, Apr, May, Jun, Jul, and Aug"
##'
summarise_months <- function(months,
                             just_first_to_last = FALSE) {
  if(length(months) == 1){
    return(month.abb[months])
  }

  # Check if months are sequential
  is_sequential <- all(diff(months) == 1)

  if(!is_sequential){
    # Spell out all months: "Apr, May, and Jul"
    month_names <- month.abb[months]
    if (length(month_names) == 2) {
      return(paste(month_names[1],
                   "and",
                   month_names[2]))
    } else {
      if(just_first_to_last){
        # Dec-Apr, say
        return(paste(month.abb[months[1]],
                     "to",
                     month.abb[months[length(months)]]))
      } else
      {
        return(paste0(paste(month_names[-length(month_names)],
                            collapse = ", "),
                      ", and ",
                      month_names[length(month_names)]))
      }
    }
  }

  # If sequential:
  if (length(months) == 2) {
    return(paste(month.abb[months[1]],
                 "and",
                 month.abb[months[2]]))
  } else {
    return(paste(month.abb[months[1]],
                 "to",
                 month.abb[months[length(months)]]))
  }
}
