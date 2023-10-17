test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# bccm
test_that("plot works for bccm anom data", {
  pdata <- get_pacea_data("test_surftemp", force = TRUE)
  expect_warning(clim1 <- calc_clim(pdata, time_period_return = c(1,6)))
  expect_warning(anom1 <- calc_anom(pdata, time_period_return = c(1,6), years_return = c(2000, 2019)))
  
  p1 <- plot(anom1)
  p2 <- plot(anom1, clim.dat = clim1)
  p3 <- plot(anom1, clim.dat = clim1, months.plot = 1, years.plot = c(2000,2019))
  p4 <- plot(anom1, clim.dat = clim1, months.plot = c(1, "June"), years.plot = c(2000,2019))
  
  library(ggplot2)
  expect_true(is.ggplot(p1))
  expect_true(is.ggplot(p2))
  expect_true(is.ggplot(p3))
  expect_true(is.ggplot(p4))
})

test_that("stop errors work for bccm anom data", {
  pdata <- get_pacea_data("test_surftemp", force = TRUE)
  expect_warning(clim1 <- calc_clim(pdata, time_period_return = c(1,6)))
  expect_warning(anom1 <- calc_anom(pdata, time_period_return = c(1,6), years_return = c(2000, 2019)))
  
  # error for input data
  expect_error(plot.pacea_stanom(buoy_sst))
  
  # errors for months and years entered
  expect_error(plot(anom1, months.plot = 2, years.plot = 2000))
  expect_error(plot(anom1, months.plot = 1, years.plot = "Jan"))
  expect_error(plot(anom1, months.plot = 1, years.plot = 1999))
  
  # error for incorrect clim.dat
  expect_error(plot(anom1, clim.dat = pdata, months.plot = 1, years.plot = 2000))
  
  # error for months,plot not matching what is available in climatology data
  clim.sub <- clim1 %>% dplyr::filter(month == 6)
  expect_warning(plot(anom1, clim.dat = clim.sub, months.plot = 1))
})


# oisst
test_that("plot works for oisst anom data", {
  clim_week <- calc_clim(oisst_7day, time_period_return = c(1,6))
  anom_week <- calc_anom(oisst_7day, time_period_return = c(1,6), years_return = c(2000, 2019))
  
  p1w <- plot(anom_week)
  p2w <- plot(anom_week, clim.dat = clim_week, weeks.plot = 1, years.plot = 2000)
  p3w <- plot(anom_week, clim.dat = clim_week, weeks.plot = c(1, 6), years.plot = c(2000, 2019))
  
  clim_month <- calc_clim(oisst_month, time_period_return = c(1,6))
  anom_month <- calc_anom(oisst_month, time_period_return = c(1,6), years_return = c(2000, 2019))
  
  p1m <- plot(anom_month)
  p2m <- plot(anom_month, clim.dat = clim_month, months.plot = 1, years.plot = 2000)
  p3m <- plot(anom_month, clim.dat = clim_month, months.plot = c(1, 6), years.plot = c(2000, 2019))
  
  library(ggplot2)
  expect_true(is.ggplot(p1w))
  expect_true(is.ggplot(p2w))
  expect_true(is.ggplot(p3w))
  
  expect_true(is.ggplot(p1m))
  expect_true(is.ggplot(p2m))
  expect_true(is.ggplot(p3m))
})

test_that("stop errors work for oisst anom data", {
  sub.dat <- oisst_7day %>% dplyr::filter(year %in% c(2000, 2019))
  expect_warning(clim_week <- calc_clim(sub.dat, time_period_return = c(1,6)))
  anom_week <- calc_anom(sub.dat, time_period_return = c(1,6), years_return = c(2000, 2019))
  
  sub.dat <- oisst_month %>% dplyr::filter(year %in% c(2000, 2019))
  expect_warning(clim_month <- calc_clim(sub.dat, time_period_return = c(1,6)))
  expect_warning(clim_month2 <- calc_clim(sub.dat, time_period_return = c(2,6)))
  anom_month <- calc_anom(sub.dat, time_period_return = c(1,6), years_return = c(2000, 2019))
  
  # error for input data
  expect_error(plot.pacea_oianom(buoy_sst))
  
  # errors for weeks/months and years entered
  expect_error(plot(anom_week, weeks.plot = 2, years.plot = 2000))
  expect_error(plot(anom_week, weeks.plot = 1, years.plot = "Jan"))
  expect_error(plot(anom_week, weeks.plot = 1, years.plot = 1999))
  
  expect_error(plot(anom_month, months.plot = 2, years.plot = 2000))
  
  # error for incorrect clim.dat
  expect_error(plot(anom_week, clim.dat = pdata, months.plot = 1, years.plot = 2000))
  expect_error(plot(anom_week, clim.dat = clim_month, months.plot = 1, years.plot = 2000))
  
  expect_warning(plot(anom_month, clim.dat = clim_month2, months.plot = c(1,6), years.plot = 2019))
})

test_that("Plotting anomaly with only one column (layer) of data", {
  pdata <- get_pacea_data("test_surftemp", force = TRUE)
  expect_warning(clim1 <- calc_clim(pdata, time_period_return = c(1)))
  expect_warning(anom1 <- calc_anom(pdata, time_period_return = c(1), years_return = c(2000)))
  
  head(clim1)
  head(anom1)
  
  p1 <- plot(anom1, clim.dat = clim1)
  
  library(ggplot2)
  expect_true(is.ggplot(p1))
})


test_that("Plotting anomaly with negative SD contour lines",{
  pdata <- get_pacea_data("test_surfsal", force = TRUE)
  expect_warning(clim1 <- calc_clim(pdata, time_period_return = c(1)))
  expect_warning(anom1 <- calc_anom(pdata, time_period_return = c(1), years_return = c(2000)))
  
  p1 <- plot(anom1, clim.dat = clim1)
  
  library(ggplot2)
  expect_true(is.ggplot(p1))
})





