# lubridate_pacea_series()
# Did not have specific tests before (function code was covered by other tests),
#  but Emily spotted that attributes weren't getting preserved (Issue #87).
test_that("lubridate_pacea_series() works and preserves attributes", {
  expect_equal(attr(oni,
                    "axis_name"),
               attr(lubridate_pacea_series(oni),
                    "axis_name"))

  expect_equal(attr(oni,
                    "axis_name"),
               attr(lubridate_pacea_series(oni,
                                      smooth_over_year=TRUE),
               "axis_name"))
})
