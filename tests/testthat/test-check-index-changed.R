test_that("a non-pacea_index object returns an error", {
  expect_error(check_index_changed(1, oni))
  expect_error(check_index_changed(oni, "abc"))
})

test_that("an unchanged index is correctly identified as not changed", {
  expect_false(check_index_changed(oni, oni))
})

test_that("a changed index is identified as changed", {
  expect_true(check_index_changed(oni,
                                  oni[-10, ]))

  # This passes but messes up code coverage calculations for some reason
  #  expect_true(check_index_changed(oni,
  #                                  oni[, -1]))

  oni2 <- oni
  names(oni2)[4] <- "anomaly_schmanomaly"
  expect_true(check_index_changed(oni,
                                  oni2))
  expect_true(check_index_changed(oni,
                                  dplyr::mutate(oni, value = 2 * value)))
  expect_true(check_index_changed(oni,
                                  rbind(oni, oni[1,])))

  oni3 <- oni
  class(oni3)[1] <- "pacea_biomass"
  expect_true(check_index_changed(oni,
                                  oni3))
})
