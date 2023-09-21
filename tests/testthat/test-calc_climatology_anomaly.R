test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# calc_clim
test_that("calc_clim works for bccm data", {
  pdata <- get_pacea_data("test_surftemp")
  expect_warning(clim_bccm <- calc_clim(pdata, time_period_return = 6))
  expect_equal(class(clim_bccm)[1], "pacea_stclim")
})

test_that("calc_clim works for oisst data", {
  sub.dat <- oisst_7day %>% dplyr::filter(year %in% c(2000:2010))
  clim_oisst <- calc_clim(sub.dat)
  
  expect_equal(class(clim_oisst)[1], "pacea_oiclim")
  expect_equal(colnames(clim_oisst)[1], "week")
  
  sub.dat <- oisst_month %>% dplyr::filter(year %in% c(2000:2010))
  clim_oisst <- calc_clim(sub.dat)
  
  expect_equal(class(clim_oisst)[1], "pacea_oiclim")
  expect_equal(colnames(clim_oisst)[1], "month")
})

test_that("calc_clim works for pacea_buoy data", {
  clim1 <- calc_clim(buoy_sst, clim_time = "month")
  clim2 <- calc_clim(buoy_sst, clim_time = "week")
  clim3 <- calc_clim(buoy_sst, clim_years = c(1971:2000))
  
  expect_equal(class(clim1)[1], "pacea_buoyclim")
  expect_equal(colnames(clim1)[2], "month")
  
  expect_equal(class(clim2)[1], "pacea_buoyclim")
  expect_equal(colnames(clim2)[2], "week")
  
  expect_equal(class(clim3)[1], "pacea_buoyclim")
  expect_equal(colnames(clim3)[2], "month")
})

test_that("calc_clim stop errors work",{
  expect_error(clim1 <- calc_clim(buoy_sst, clim_time = "year"))
})


# calc_anom
test_that("calc_anom works for bccm data", {
  pdata <- get_pacea_data("test_surftemp")
  expect_warning(anom_bccm <- calc_anom(pdata, time_period_return = 6, years_return = 2000))
  expect_equal(class(anom_bccm)[1], "pacea_stanom")
})


test_that("calc_anom works for oisst data", {
  sub.dat <- oisst_7day %>% dplyr::filter(year %in% c(2000:2010))
  anom_oisst <- calc_anom(sub.dat, time_period_return = 6, years_return = 2000)
  
  expect_equal(class(anom_oisst)[1], "pacea_oianom")
  expect_equal(colnames(anom_oisst)[2], "week")
  
  sub.dat <- oisst_month %>% dplyr::filter(year %in% c(2000:2010))
  anom_oisst <- calc_anom(sub.dat, time_period_return = 6, years_return = 2000)
  
  expect_equal(class(anom_oisst)[1], "pacea_oianom")
  expect_equal(colnames(anom_oisst)[2], "month")
})

test_that("calc_anom works for pacea_buoy data", {
  anom1 <- calc_anom(buoy_sst, clim_time = "month", time_period_return = 9, years_return = 2000)
  anom2 <- calc_anom(buoy_sst, clim_time = "week", time_period_return = 9, years_return = 2000)
  anom3 <- calc_anom(buoy_sst, clim_years = c(1971:2000), time_period_return = 9, years_return = 2000)
  
  expect_equal(class(anom1)[1], "pacea_buoyclim")
  expect_equal(colnames(anom1)[5], "month")
  
  expect_equal(class(anom2)[1], "pacea_buoyclim")
  expect_equal(colnames(anom2)[5], "week")
  
  expect_equal(class(anom3)[1], "pacea_buoyclim")
  expect_equal(colnames(anom3)[5], "month")
})

test_that("calc_anom stop errors work",{
  expect_error(clim1 <- calc_anom(buoy_sst, clim_time = "year"))
})
