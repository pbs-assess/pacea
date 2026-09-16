# These are for Andy's new calculate_anomaly.pacea_buoy().

# buoy_sst data
test_that("create_index.pacea_buoy() gives expected errors", {
  expect_error(create_index(buoy_sst,
                            stn_id = 1:2))
  expect_error(create_index(buoy_sst,
                            index_statistic = "hello"))
  expect_error(create_index(buoy_sst,
                            stn_id = "made-up-buoy-name"))
  expect_error(create_index(buoy_sst,
                            months = c(1, 2, 4)))
  expect_error(create_index(buoy_sst,
                            months = c(11, 12, 2)))
  expect_error(create_index(buoy_sst,
                            require_requested_months = 2))

})


