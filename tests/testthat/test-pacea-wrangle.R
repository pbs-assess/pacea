test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


test_that("pacea_long function works", {
  # download subsetted small data
  pdata <- get_pacea_data("test_surftemp", force = TRUE)
  
  # convert to long format
  long_dat <- pacea_long(pdata)
  
  expect_equal(nrow(long_dat), nrow(pdata) * (ncol(pdata) - 1))
  expect_length(long_dat, 4)
})


test_that("pacea_wide function works", {
  # download subsetted small data
  pdata <- get_pacea_data("test_surftemp", force = TRUE)
  
  # conver to long then wide format
  long_dat <- pacea_long(pdata)
  wide_dat <- pacea_wide(long_dat)
  
  expect_equal(nrow(wide_dat), nrow(pdata))
  expect_length(wide_dat, 5)
})

test_that("pacea_wide errors work", {
  # download subsetted small data
  pdata <- get_pacea_data("test_surftemp", force = TRUE)
  
  # convert to long then wide format
  long_dat <- pacea_long(pdata)
  wide_dat <- pacea_wide(long_dat)
  
  # error for incorrect names_from values
  expect_error(pacea_wide(long_dat, names_from = c("week")))
  
  # incorrect legnth for and values for values_from
  expect_error(pacea_wide(long_dat, values_from = c("value", "valuefake")))
  expect_error(pacea_wide(long_dat, values_from = c("valuefake")))
})

