test_that("create_data_hake() works", {
  x <- 1:10
  year <- 2048
  expect_invisible(create_data_hake(paste0("test_file_",
                                           year),
                                    x))
  file.remove(list = paste0(here::here(),
                            "/data/test_file_",
                            year,
                            ".rda"))
})
