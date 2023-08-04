test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})



test_that("Base plot for pacea.st objects work with various functions", {
  pdata <- roms_surface_temperature(force = T)
  
  expect_invisible(plot(pdata))
  expect_invisible(plot(pdata, 
                        months = c("June", "September"), 
                        years = c(1995)))
  expect_invisible(plot(pdata, 
                        months = c("June", 1),
                        years = c(1995)))
  expect_invisible(plot(pdata, 
                        months = c(1),
                        years = c(1995)))
  expect_invisible(plot(pdata, 
                        months = c("September"),
                        years = c(2019), 
                        bc = T, eez = T))
  
})



test_that("geospatial plotting: test that stopifnot commands are working", {
  pdata <- roms_surface_temperature(force = T)
  
  expect_error(plot.pacea_st(npi_monthly))
  
  expect_error(plot.pacea_st(pdata, years = c("January")))
  
  expect_error(plot.pacea_st(pdata, months = c("asdf")))
  
  expect_error(plot.pacea_st(pdata, years = c(1990)))
  
})
