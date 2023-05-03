# These are on plotting functions for pacea_index and then pacea_recruitment and pacea_biomass objects.

test_that("the stopifnot commands are working", {
  expect_error(plot(oni,
                    value = "foo"))
  expect_error(plot(npi_monthly))
  expect_error(plot(npi_annual,
                    smooth_over_year = TRUE))
})

test_that("index plotting works with various options", {
  expect_invisible(plot(npi_monthly,
                        smooth_over_year = TRUE,
                        value = "value"))
  expect_invisible(plot(oni,
                        style = "red_blue"))

  expect_invisible(plot(npi_monthly,
                        value = "value"))
  expect_invisible(plot(npi_annual,
                        value = "value"))
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

# Adapting some of above for hake_recruitment

test_that("the stopifnot commands are working", {
  expect_error(plot(hake_recruitment,
                    value = "foo"))
  expect_error(plot.hake_recruitment(npi_monthly))
})

test_that("recruitment plotting works with various options", {
  expect_invisible(plot(hake_recruitment))
  expect_invisible(plot(hake_recruitment,
                        style = "foo"))
  expect_invisible(plot(dplyr::select(hake_recruitment,
                                      -c("low"))))
  expect_invisible(plot(hake_recruitment,
                        y_max = 100))
})
