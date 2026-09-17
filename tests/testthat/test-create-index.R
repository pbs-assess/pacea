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
                            stn_id = "C43210"))

  expect_error(create_index(buoy_sst,
                            months = c(1, 2, 4)))
  expect_error(create_index(buoy_sst,
                            months = c(11, 12, 2)))
  expect_error(create_index(buoy_sst,
                            require_requested_months = 2))

})


test_that("create_index.pacea_buoy() works with different options", {
  # Months variations
  expect_s3_class(create_index(buoy_sst,
                               months = 5),
                  "pacea_standardised_index")
  expect_s3_class(create_index(buoy_sst,
                               months = 5:6),
                  "pacea_standardised_index")
  expect_s3_class(create_index(buoy_sst,
                               months = c(11, 12, 1, 2, 3)),
                  "pacea_standardised_index")

  # Test different years
  expect_s3_class(create_index(buoy_sst,
                               months = 5:6,
                               years = 2000:2010),
                  "pacea_standardised_index")
  expect_s3_class(create_index(buoy_sst,
                               months = 5:6,
                               years = 2015:2020),
                  "pacea_standardised_index")

  # Test different stations (by code and by name)
  expect_s3_class(create_index(buoy_sst,
                               months = 5:6,
                               stn_id = "C46181"),
                  "pacea_standardised_index")
  expect_s3_class(create_index(buoy_sst,
                               months = 5:6,
                               stn_id = "Central Dixon Entrance"),
                  "pacea_standardised_index")

  # Test different index_statistic
  expect_s3_class(create_index(buoy_sst,
                               months = 5:6,
                               index_statistic = "mean"),
                  "pacea_standardised_index")

  # Test custom labels and names
  expect_s3_class(create_index(buoy_sst,
                               months = 5:6,
                               index_label = "Custom Label",
                               index_name = "custom_name"),
                  "pacea_standardised_index")

  # Test require_requested_months (partial months)
  expect_s3_class(create_index(buoy_sst,
                               months = 5:6,
                               require_requested_months = 1),
                  "pacea_standardised_index")

  # Test ... arguments passed to calculate_anomalies.pacea_buoy
  expect_s3_class(create_index(buoy_sst,
                               months = 5:6,
                               min_days_per_month = 10,
                               climatology_years = 2015:2025),
                  "pacea_standardised_index")
  expect_s3_class(create_index(buoy_sst,
                               months = 5:6,
                               max_consecutive_missing_days = 20),
                  "pacea_standardised_index")

  # Combination: multiple options
  expect_s3_class(create_index(buoy_sst,
                               months = 5:6,
                               years = 2010:2023,
                               stn_id = "C46181",
                               index_statistic = "mean",
                               require_requested_months = 1),
                  "pacea_standardised_index")
})
