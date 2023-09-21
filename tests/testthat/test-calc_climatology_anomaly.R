test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# calc_clim
test_that("calc_clim works for bccm data", {
  pdata <- bccm_surface_temperature()
  expect_warning(clim_bccm <- calc_clim(pdata))
  expect_equal(class(clim_bccm)[1], "pacea_stclim")
})

test_that("calc_clim works for oisst data", {
  clim_oisst <- calc_clim(oisst_7day)
  
  expect_equal(class(clim_oisst)[1], "pacea_oiclim")
  expect_equal(colnames(clim_oisst)[1], "week")
  
  clim_oisst <- calc_clim(oisst_month)
  
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
  pdata <- bccm_surface_temperature()
  expect_warning(anom_bccm <- calc_anom(pdata))
  expect_equal(class(anom_bccm)[1], "pacea_stanom")
})


test_that("calc_anom works for oisst data", {
  anom_oisst <- calc_anom(oisst_7day)
  
  expect_equal(class(anom_oisst)[1], "pacea_oianom")
  expect_equal(colnames(anom_oisst)[2], "week")
  
  anom_oisst <- calc_anom(oisst_month)
  
  expect_equal(class(anom_oisst)[1], "pacea_oianom")
  expect_equal(colnames(anom_oisst)[2], "month")
})

test_that("calc_anom works for pacea_buoy data", {
  anom1 <- calc_anom(buoy_sst, clim_time = "month")
  anom2 <- calc_anom(buoy_sst, clim_time = "week")
  anom3 <- calc_anom(buoy_sst, clim_years = c(1971:2000))
  
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
