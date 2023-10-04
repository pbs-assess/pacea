test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("ggplot plot for pacea.st objects work with various functions", {
  # require ggplot2 for is.ggplot() function
  library(ggplot2)
  
  # download subsetted small size data
  pdata <- get_pacea_data("test_surftemp", force = TRUE)
  
  # numeric month
  expect_true(is.ggplot(plot(pdata, 
                             months.plot = c(1),
                             years.plot = c(2000))))   
  
  # character month - no bc or eez
  expect_true(is.ggplot(plot(pdata, 
                             months.plot = c("June"),
                             years.plot = c(2000), 
                             bc = FALSE, eez = FALSE)))
  
  # numeric and character month - no bc or eez
  expect_true(is.ggplot(plot(pdata, 
                             months.plot = c(1, "June"),
                             years.plot = c(2000, 2019), 
                             bc = FALSE, eez = FALSE)))
  
  # numeric and abbr month - no bc or eez
  expect_true(is.ggplot(plot(pdata, 
                             months.plot = c(1, "Jun"),
                             years.plot = c(2000, 2019), 
                             bc = FALSE, eez = FALSE)))
})

test_that("geospatial plotting: test that stopifnot commands are working", {
  # download subsetted small size data
  pdata <- get_pacea_data("test_surftemp", force = TRUE)
  
  # wrong data type - not sf class (or pacea_st)
  expect_error(plot.pacea_st(npi_monthly))
  
  # incorrect years.plot value
  expect_error(plot.pacea_st(pdata, years.plot = c("January")))
  
  # incorrect months.plot value
  expect_error(plot.pacea_st(pdata, months.plot = c("asdf")))
  
  # year value specified not in data
  expect_error(plot.pacea_st(pdata, years.plot = c(1990)))
  
  # test subroutine function - months.plot value incorrect
  expect_error(subset_pacea_ym(pdata, years = 1991, months.plot = c("Jam")))
})

test_that("month_match function errors are working", {
  # too many matched values
  expect_error(month_match("Ju"))
  
  # month value does not exist
  expect_error(month_match(13))
}) 
