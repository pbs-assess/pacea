test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("Data object name is a valid character string and available: stop error is working", {
  expect_error(get_pacea_data(1234))
  expect_error(get_pacea_data("1234"))
})

test_that("Test internet connection: stop error is working", {
  
  ## write list of interactive functions and responses here
  layer <- "test_data"
  
  cache_dir <- pacea_cache()
  
  file_list <- list.files(cache_dir)[grep(pattern = layer, list.files(cache_dir))]
  
  # remove test_data files
  if(length(file_list) >= 1) {
    for(i in 1:length(file_list)){
      unlink(paste0(cache_dir, "/", file_list[i]))
    }
  }
  
  expect_error(get_pacea_data("test_data_01", force = "testInternetError"))
  
  dat1 <- get_pacea_data("test_data_01", force = TRUE)
  
  expect_warning(get_pacea_data("test_data", update = TRUE, force = "testInternetError"))
})


test_that("Denying of downloading test data: successful", {
  
  ## write list of interactive functions and responses here
  layer <- "test_data"
  
  cache_dir <- pacea_cache()
  
  file_list <- list.files(cache_dir)[grep(pattern = layer, list.files(cache_dir))]
  
  # remove test_data files
  if(length(file_list) >= 1) {
    for(i in 1:length(file_list)){
      unlink(paste0(cache_dir, "/", file_list[i]))
    }
  }
  
  data1_dir <- paste0(pacea_cache(), "/", "test_data_01.rds")
  expect_error(data1 <- get_pacea_data("test_data_01", force = FALSE), "Exiting...")
  
  # data1 deleted with data2 downloaded
  expect_equal(file.exists(data1_dir), FALSE)
})


test_that("Download of test data 1, and loading of cached test data 1: successful", {
  
  ## write list of interactive functions and responses here
  layer <- "test_data"
  
  cache_dir <- pacea_cache()
  
  file_list <- list.files(cache_dir)[grep(pattern = layer, list.files(cache_dir))]
  
  # remove test_data files
  if(length(file_list) >= 1) {
    for(i in 1:length(file_list)){
      unlink(paste0(cache_dir, "/", file_list[i]))
    }
  }
  
  data1_dir <- paste0(pacea_cache(), "/", "test_data_01.rds")
  data1 <- get_pacea_data("test_data_01", force = TRUE)
  data1.1 <- get_pacea_data("test_data", force = TRUE)
  
  expect_equal(file.exists(data1_dir), TRUE)
  
  unlink(paste0(cache_dir, "/test_data_01.rds"))
  expect_length(data1, 100)
  expect_length(data1.1, 100)
})


test_that("Download and update of test data (from version 1 to 2) declined (ie. keep old data); force = FALSE", {
  
  ## write list of interactive functions and responses here
  layer <- "test_data"
  
  cache_dir <- pacea_cache()
  
  file_list <- list.files(cache_dir)[grep(pattern = layer, list.files(cache_dir))]
  
  # remove test_data files
  if(length(file_list) >= 1) {
    for(i in 1:length(file_list)){
      unlink(paste0(cache_dir, "/", file_list[i]))
    }
  }
  data1_dir <- paste0(cache_dir, "/", "test_data_01.rds")
  
  # download test_data_01
  data1 <- get_pacea_data("test_data_01", force = TRUE)
  expect_equal(file.exists(data1_dir), TRUE)
  
  # check for test_data update and update using default 'ask = true' when interactive message detected
  expect_warning(data2f <- get_pacea_data("test_data", update = TRUE, force = FALSE), "Returned local version of data.")
  
  # data_2 directory
  data2f_dir <- paste0(cache_dir, "/", "test_data_02.rds")
  
  # call test_data to ensure old version is loaded
  data1.1 <- get_pacea_data("test_data")
  
  # data1 deleted with data2 downloaded
  expect_equal(file.exists(data1_dir), TRUE)
  expect_equal(file.exists(data2_dir), FALSE)
  
  unlink(paste0(cache_dir, "/test_data_01.rds"))
  expect_length(data1, 100)
  expect_length(data2f, 100)
  expect_length(data1.1, 100)
})



test_that("Download and update of test data (from version 1 to 2) successful - force = TRUE", {
  
  ## write list of interactive functions and responses here
  layer <- "test_data"
  
  cache_dir <- pacea_cache()
  
  file_list <- list.files(cache_dir)[grep(pattern = layer, list.files(cache_dir))]
  
  # remove test_data files
  if(length(file_list) >= 1) {
    for(i in 1:length(file_list)){
      unlink(paste0(cache_dir, "/", file_list[i]))
    }
  }
  data1_dir <- paste0(cache_dir, "/", "test_data_01.rds")
  
  # download test_data_01
  data1 <- get_pacea_data("test_data_01", force = TRUE)
  expect_equal(file.exists(data1_dir), TRUE)
  
  # Update test_data using 'force = TRUE'
  data2 <- get_pacea_data("test_data", update = TRUE, force = TRUE)
  
  # data_2 directory
  data2_dir <- paste0(cache_dir, "/", "test_data_02.rds")
  
  # call test_data to ensure most recent version is loaded
  data2.1 <- get_pacea_data("test_data")
  
  expect_warning(get_pacea_data("test_data", update = T, force = T), "Most recent version of data already downloaded in cache folder!")
  
  expect_equal(file.exists(data1_dir), FALSE)
  expect_equal(file.exists(data2_dir), TRUE)
  
  unlink(paste0(cache_dir, "/test_data_02.rds"))
  expect_length(data1, 100)
  expect_length(data2, 200)
  expect_length(data2.1, 200)
})


test_that("'ask' function works and returns correct value", {
  expect_equal(ask("Return value = TRUE when skipping interactive with testthat"), TRUE)
})

