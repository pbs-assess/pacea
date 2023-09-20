test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("ggplot plot for pacea.st objects work with various functions", {
  pdata <- bccm_surface_temperature(force = T)
  
  expect_true(is.ggplot(plot(pdata)))
  expect_true(is.ggplot(plot(pdata, 
                             months.plot = c("June", "September"), 
                             years.plot = c(1995))))
  expect_true(is.ggplot(plot(pdata, 
                             months.plot = c("June", 1),
                             years.plot = c(1995))))
  expect_true(is.ggplot(plot(pdata, 
                             months.plot = c(1),
                             years.plot = c(1995))))   
  expect_true(is.ggplot(plot(pdata, 
                             months.plot = c("September"),
                             years.plot = c(2019), 
                             bc = FALSE, eez = FALSE)))
  expect_true(is.ggplot(plot(pdata, 
                             months.plot = c(1, "September"),
                             years.plot = c(2018, 2019), 
                             bc = FALSE, eez = FALSE)))
})

test_that("geospatial plotting: test that stopifnot commands are working", {
  pdata <- bccm_surface_temperature(force = T)
  
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
