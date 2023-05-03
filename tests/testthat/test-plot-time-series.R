# These are on pacea_index and then pacea_recruitment and pacea_biomass objects.

test_that("the stopifnot commands are working", {
  expect_error(plot(oni,
                    value = "foo"))
  expect_error(plot(npi_monthly))
  expect_error(plot(npi_annual,
                    smooth_over_year = TRUE))
})

test_that("plotting works with various options", {
  expect_invisible(plot(npi_monthly,
                        smooth_over_year = TRUE,
                        value = "val"))
  expect_invisible(plot(npi_monthly,
                        value = "val"))
  expect_invisible(plot(npi_annual,
                        value = "val"))
  expect_invisible(plot(oni,
                        xlim = c(lubridate::dmy(01011950),
                                 lubridate::dmy(01012040)))) # to expand x-axis
  expect_invisible(plot(oni,
                       type = "o"))     # passed onto plot()
  expect_invisible(plot(oni,
                        main = "My nice plot"))  # passed onto plot()

  suppressWarnings(expect_warning(plot(oni,
                                       not_an_option = "My nice plot")))
                                       # nonsensical ... option

  expect_invisible(plot(oni,
                        style = "goa"))   # that isn't implemented yet

})

# TODO repeat some of above for hake_recruitment
