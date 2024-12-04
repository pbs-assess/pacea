test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


test_that("shortcuts work", {
  expect_error(bccm_bottom_oxygen(force = "testDataFunctions"))
  expect_error(bccm_bottom_ph(force = "testDataFunctions"))
  expect_error(bccm_bottom_salinity(force = "testDataFunctions"))
  expect_error(bccm_bottom_temperature(force = "testDataFunctions"))

  expect_error(bccm_surface_oxygen(force = "testDataFunctions"))
  expect_error(bccm_surface_ph(force = "testDataFunctions"))
  expect_error(bccm_surface_salinity(force = "testDataFunctions"))
  expect_error(bccm_surface_temperature(force = "testDataFunctions"))

  expect_error(bccm_avg0to40m_oxygen(force = "testDataFunctions"))
  expect_error(bccm_avg0to40m_ph(force = "testDataFunctions"))
  expect_error(bccm_avg0to40m_salinity(force = "testDataFunctions"))
  expect_error(bccm_avg0to40m_temperature(force = "testDataFunctions"))

  expect_error(bccm_avg40to100m_oxygen(force = "testDataFunctions"))
  expect_error(bccm_avg40to100m_ph(force = "testDataFunctions"))
  expect_error(bccm_avg40to100m_salinity(force = "testDataFunctions"))
  expect_error(bccm_avg40to100m_temperature(force = "testDataFunctions"))

  expect_error(bccm_avg100mtoBot_oxygen(force = "testDataFunctions"))
  expect_error(bccm_avg100mtoBot_ph(force = "testDataFunctions"))
  expect_error(bccm_avg100mtoBot_salinity(force = "testDataFunctions"))
  expect_error(bccm_avg100mtoBot_temperature(force = "testDataFunctions"))

  expect_error(bccm_phytoplankton(force = "testDataFunctions"))
  expect_error(bccm_primaryproduction(force = "testDataFunctions"))
})

# This failed locally for Andy (when doing test(), but not running
# individually). Think it's the timeout issue; see README. Works okay on GitHub.
test_that("Download all bccm data works", {
  bccm_all_variables()
  data1_dir <- paste0(pacea_cache(),"/test_data_01.rds")
  expect_equal(file.exists(data1_dir), TRUE)
  unlink(data1_dir)
})
