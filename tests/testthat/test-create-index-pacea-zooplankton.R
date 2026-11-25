# Tests for create_index.pacea_zooplankton() function

test_that("create_index.pacea_zooplankton() gives expected errors", {
  # Invalid zooplankton_species_group
  expect_error(create_index(zooplankton_sog,
                            zooplankton_species_group = "made_up_species"),
               "zooplankton_species_group must be a column of the zooplankton_sog object")
})


test_that("create_index.pacea_zooplankton() works with default settings", {
  # Default parameters: total_biomass, anomaly statistic
  result <- create_index(zooplankton_sog)

  expect_s3_class(result,
                  "pacea_standardised_index")
  expect_s3_class(result,
                  "tbl_df")
  expect_true(nrow(result) > 0)
  expect_true(all(c("year", "value", "index") %in% names(result)))
  expect_equal(length(unique(result$index)), 1)
  expect_equal(unique(result$index), "total_biomass_sog")
})


test_that("create_index.pacea_zooplankton() works with different species groups", {
  # Test various zooplankton species groups
  expect_s3_class(create_index(zooplankton_sog,
                               zooplankton_species_group = "cladocera"),
                  "pacea_standardised_index")

  expect_s3_class(create_index(zooplankton_sog,
                               zooplankton_species_group = "euphausiids"),
                  "pacea_standardised_index")

  expect_s3_class(create_index(zooplankton_sog,
                               zooplankton_species_group = "calanoid_copepods_large"),
                  "pacea_standardised_index")

  expect_s3_class(create_index(zooplankton_sog,
                               zooplankton_species_group = "fish"),
                  "pacea_standardised_index")

  expect_s3_class(create_index(zooplankton_sog,
                               zooplankton_species_group = "medusae"),
                  "pacea_standardised_index")
})


test_that("create_index.pacea_zooplankton() works with different years", {
  # Test with specific year ranges
  result_2000_2010 <- create_index(zooplankton_sog,
                                   years = 2000:2010)
  expect_s3_class(result_2000_2010,
                  "pacea_standardised_index")
  expect_true(all(result_2000_2010$year %in% 2000:2010))

  result_2015_2025 <- create_index(zooplankton_sog,
                                   years = 2015:2025)
  expect_s3_class(result_2015_2025,
                  "pacea_standardised_index")

  # Test with single year
  result_2010 <- create_index(zooplankton_sog,
                              years = 2010)
  expect_s3_class(result_2010,
                  "pacea_standardised_index")
  expect_true(all(result_2010$year == 2010))

  # Test with years outside data range returns empty tibble with correct structure
  result_empty <- create_index(zooplankton_sog,
                               years = 1900:1950)
  expect_s3_class(result_empty,
                  "pacea_standardised_index")
  expect_equal(nrow(result_empty), 0)
})


test_that("create_index.pacea_zooplankton() works with different index_statistic", {
  # anomaly statistic (default)
  result_anomaly <- create_index(zooplankton_sog,
                                 index_statistic = "anomaly")
  expect_s3_class(result_anomaly,
                  "pacea_standardised_index")
  expect_true(!anyNA(result_anomaly$value))
})


test_that("create_index.pacea_zooplankton() works with custom labels and names", {
  # Test with custom index_label and index_name
  result <- create_index(zooplankton_sog,
                         index_label = "My Custom Zooplankton Label",
                         index_name = "my_custom_zoop_index")

  expect_s3_class(result,
                  "pacea_standardised_index")
  # The index_label should be in the data as a column
  expect_equal(unique(result$index_label),
               "My Custom Zooplankton Label")
  expect_equal(unique(result$index),
               "my_custom_zoop_index")
})


test_that("create_index.pacea_zooplankton() auto-generates appropriate labels", {
  # Test that labels are auto-generated from zooplankton_sog_axis_names when not provided
  result <- create_index(zooplankton_sog,
                         zooplankton_species_group = "cladocera")

  expect_s3_class(result,
                  "pacea_standardised_index")
  # index_label should be populated from zooplankton_sog_axis_names
  expect_true(length(unique(result$index_label)) == 1)
  expect_true(nchar(unique(result$index_label)) > 0)
  expect_true(grepl("cladocera",
                    unique(result$index),
                    ignore.case = TRUE))
  expect_true(grepl("sog",
                    unique(result$index)))
})


test_that("create_index.pacea_zooplankton() combinations of options work", {
  # Test multiple options together
  result1 <- create_index(zooplankton_sog,
                          zooplankton_species_group = "euphausiids",
                          years = 2010:2023,
                          index_statistic = "anomaly",
                          index_label = "Euphausid Test",
                          index_name = "euph_test_index")
  expect_s3_class(result1,
                  "pacea_standardised_index")
  expect_true(all(result1$year %in% 2010:2023))
  expect_equal(unique(result1$index),
               "euph_test_index")
  expect_equal(unique(result1$index_label),
               "Euphausid Test")

  # Another combination
  result2 <- create_index(zooplankton_sog,
                          zooplankton_species_group = "fish",
                          years = 2000:2015)
  expect_s3_class(result2,
                  "pacea_standardised_index")
  expect_true(all(result2$year %in% 2000:2015))
  expect_true(grepl("fish",
                    unique(result2$index)))
})
