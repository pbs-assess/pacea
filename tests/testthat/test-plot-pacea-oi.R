test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


test_that("ggplot plot for pacea.st objects work with various functions", {
  test.dat <- oisst_7day
  
  # default plot (most recent year and week/month available)
  expect_true(is.ggplot(plot(test.dat)))
  
  # specify week and year combination
  expect_true(is.ggplot(plot(test.dat, 
                             weeks.plot = c(1),
                             years.plot = c(1995))))
  
  # specify week and year to create facet grid - bc and eez equals false
  expect_true(is.ggplot(plot(test.dat, 
                             weeks.plot = c(1, 35),
                             years.plot = c(2018, 2019), 
                             bc = FALSE, eez = FALSE)))
  
  test.dat <- oisst_month
  
  # default plot (most recent year and week/month available)
  expect_true(is.ggplot(plot(test.dat)))
  
  # plot month and year combination with facet_wrap
  expect_true(is.ggplot(plot(test.dat, 
                             months.plot = c("June", "September"), 
                             years.plot = c(1995))))
  
  # inidcate months with character and numeric and abbr
  expect_true(is.ggplot(plot(test.dat, 
                             months.plot = c("June", 2, "Jan"),
                             years.plot = c(1995))))
  
  # indicate months with numeric, character, abbr, bc and eez = False, facet_grid
  expect_true(is.ggplot(plot(test.dat, 
                             months.plot = c(1, "September", "Oct"),
                             years.plot = c(2018, 2019), 
                             bc = FALSE, eez = FALSE)))
})


test_that("test that stopifnot commands are working", {
  test.dat <- oisst_7day
  
  # incorrect data, must have week or month as column name
  expect_error(plot.pacea_oi(npi_annual))
  
  # years.plot not valid
  expect_error(plot.pacea_oi(test.dat, years.plot = c("January")))
  expect_error(plot.pacea_oi(test.dat, years.plot = c(1800)))
  
  # incorrect weeks.plot values (warning for week values)
  expect_error(expect_warning(plot.pacea_oi(test.dat, weeks.plot = c("asdf"))))
  
  # date combinations do not exist in dataset
  expect_error(plot.pacea_oi(test.dat, years.plot = 1981, weeks.plot = 1))
  
  # not all date combinations available
  expect_warning(plot.pacea_oi(test.dat, years.plot = 1981, weeks.plot = c(1, 50)))
})


