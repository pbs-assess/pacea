test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("Data object name is a valid character string and available: stop error is working", {
  expect_error(get_pacea_data(1234))
  expect_error(get_pacea_data("1234"))
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
  data1 <- get_pacea_data("test_data_01", force = T)
  data1.1 <- get_pacea_data("test_data", force = T)
  
  expect_equal(file.exists(data1_dir), TRUE)
  
  unlink(paste0(cache_dir, "/test_data_01.rds"))
  expect_length(data1, 100)
  expect_length(data1.1, 100)
})


test_that("Download and update of test data (from version 1 to 2) successful", {
  
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
  
  data1 <- get_pacea_data("test_data_01", force = T)
  expect_equal(file.exists(data1_dir), TRUE)
  
  data2_dir <- paste0(pacea_cache(), "/", "test_data_02.rds")
  data2 <- get_pacea_data("test_data", update = T, force = T)
  data2.1 <- get_pacea_data("test_data")
  
  expect_equal(file.exists(data1_dir), FALSE)
  expect_equal(file.exists(data2_dir), TRUE)
  
  unlink(paste0(cache_dir, "/test_data_02.rds"))
  expect_length(data1, 100)
  expect_length(data2, 200)
  expect_length(data2.1, 200)
})


## TESt errors and interactive questions

