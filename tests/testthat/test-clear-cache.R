test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("clear cache works", {
  # clear cache of 'test_' files
  pacea_clear_cache() 
  
  # check for files
  file.list <- list.files(pacea_cache(), pattern = "test_")
  expect_equal(length(file.list), 0)
})


