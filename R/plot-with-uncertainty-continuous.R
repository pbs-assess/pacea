plot_with_uncertainty_continuous <- function(obj_lub,
                                             value,
                                             xlab,
                                             ylab,
                                             uncertainty_shade_col,
                                             uncertainty_line_col,
                                             uncertainty_line_lty,
                                             y_max,
                                             median_type,
                                             median_pch,
                                             median_line_col,
                                             median_line_lty,
                                             median_line_lwd,
                                             ...){

  if(is.null(y_max)){
    y_max = max(obj_lub$high)
  }

  plot(obj_lub$date,
       obj_lub[[value]], # [[]] returns a vector not a tibble
       xlab = xlab,
       ylab = ylab,
       ylim = c(0, y_max),    # specifying ylim in main plot call won't override
       # this
       type = median_type,
       pch = median_pch,
       col = median_line_col,
       lty = median_line_lty,
       lwd = median_line_lwd,
       ...)

  polygon(x = c(obj_lub$date,
                rev(obj_lub$date)),
          y = c(obj_lub[["low"]],
                rev(obj_lub[["high"]])),
          border = NA,
          col = uncertainty_shade_col)

  lines(obj_lub$date,
        obj_lub[["low"]],
        lty = uncertainty_line_lty,
        col = uncertainty_line_col)

    lines(obj_lub$date,
          obj_lub[["high"]],
        lty = uncertainty_line_lty,
        col = uncertainty_line_col)

  invisible()
}
