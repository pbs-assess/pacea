test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("ggplot plot for pacea.st objects work with various functions", {
  library(ggplot2)
  
  pdata <- get_pacea_data("test_surftemp", force = TRUE)
  
  expect_true(is.ggplot(plot(pdata, 
                             months.plot = c(1),
                             years.plot = c(2000))))   
  expect_true(is.ggplot(plot(pdata, 
                             months.plot = c("June"),
                             years.plot = c(2000), 
                             bc = FALSE, eez = FALSE)))
  expect_true(is.ggplot(plot(pdata, 
                             months.plot = c(1, "June"),
                             years.plot = c(2000, 2019), 
                             bc = FALSE, eez = FALSE)))
})

test_that("geospatial plotting: test that stopifnot commands are working", {
  pdata <- get_pacea_data("test_surftemp", force = TRUE)
  
  expect_error(plot.pacea_st(npi_monthly))
  
  expect_error(plot.pacea_st(pdata, years.plot = c("January")))
  
  expect_error(plot.pacea_st(pdata, months.plot = c("asdf")))
  
  expect_error(plot.pacea_st(pdata, years.plot = c(1990)))
  
  # test subroutine function
  expect_error(subset_pacea_ym(pdata, years = 1991, months.plot = c("Jam")))
  
})

test_that("month_match function errors are working", {
  expect_error(month_match("Ju"))
  expect_error(month_match(13))
}) 
