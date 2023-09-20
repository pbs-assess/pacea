test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


test_that("ggplot plot for pacea.st objects work with various functions", {
  test.dat <- oisst_7day
  
  expect_true(is.ggplot(plot(test.dat)))
  expect_true(is.ggplot(plot(test.dat, 
                             weeks.plot = c(1),
                             years.plot = c(1995))))
  expect_true(is.ggplot(plot(test.dat, 
                             weeks.plot = c(1, 35),
                             years.plot = c(2018, 2019), 
                             bc = FALSE, eez = FALSE)))
  
  test.dat <- oisst_month
  
  expect_true(is.ggplot(plot(test.dat)))
  expect_true(is.ggplot(plot(test.dat, 
                             months.plot = c("June", "September"), 
                             years.plot = c(1995))))
  expect_true(is.ggplot(plot(test.dat, 
                             months.plot = c("June", 1),
                             years.plot = c(1995))))
  expect_true(is.ggplot(plot(test.dat, 
                             months.plot = c(1),
                             years.plot = c(1995))))   
  expect_true(is.ggplot(plot(test.dat, 
                             months.plot = c("September"),
                             years.plot = c(2019), 
                             bc = FALSE, eez = FALSE)))
  expect_true(is.ggplot(plot(test.dat, 
                             months.plot = c(1, "September"),
                             years.plot = c(2018, 2019), 
                             bc = FALSE, eez = FALSE)))
})




test_that("test that stopifnot commands are working", {
  test.dat <- oisst_7day
  
  expect_error(plot.pacea_oi(npi_annual))
  
  expect_error(plot.pacea_oi(test.dat, years.plot = c("January")))
  expect_error(plot.pacea_oi(test.dat, years.plot = c(1800)))
  
  expect_error(plot.pacea_oi(test.dat, weeks.plot = c("asdf")))
  
  expect_error(plot.pacea_oi(test.dat, years.plot = 1981, weeks.plot = 1))
  
  expect_warning(plot.pacea_oi(test.dat, years.plot = 1981, weeks.plot = c(1, 50)))
})


