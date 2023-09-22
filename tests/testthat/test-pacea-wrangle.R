test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


test_that("pacea_long function works", {
  pdata <- get_pacea_data("test_surftemp", force = TRUE)
  
  long_dat <- pacea_long(pdata)
  
  expect_equal(nrow(long_dat), nrow(pdata) * (ncol(pdata) - 1))
  expect_length(long_dat, 4)
})


test_that("pacea_wide function works", {
  pdata <- get_pacea_data("test_surftemp", force = TRUE)
  
  long_dat <- pacea_long(pdata)
  wide_dat <- pacea_wide(long_dat)
  
  expect_equal(nrow(wide_dat), nrow(pdata))
  expect_length(wide_dat, 5)
})
