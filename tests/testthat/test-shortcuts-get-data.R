test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


test_that("shortcuts work", {
  expect_error(roms_bottom_oxygen(force = "testDataFunctions"))
  expect_error(roms_bottom_ph(force = "testDataFunctions"))
  expect_error(roms_bottom_salinity(force = "testDataFunctions"))
  expect_error(roms_bottom_temperature(force = "testDataFunctions"))

  expect_error(roms_surface_oxygen(force = "testDataFunctions"))
  expect_error(roms_surface_ph(force = "testDataFunctions"))
  expect_error(roms_surface_salinity(force = "testDataFunctions"))
  expect_error(roms_surface_temperature(force = "testDataFunctions"))

  expect_error(roms_avg0to40m_oxygen(force = "testDataFunctions"))
  expect_error(roms_avg0to40m_ph(force = "testDataFunctions"))
  expect_error(roms_avg0to40m_salinity(force = "testDataFunctions"))
  expect_error(roms_avg0to40m_temperature(force = "testDataFunctions"))

  expect_error(roms_avg40to100m_oxygen(force = "testDataFunctions"))
  expect_error(roms_avg40to100m_ph(force = "testDataFunctions"))
  expect_error(roms_avg40to100m_salinity(force = "testDataFunctions"))
  expect_error(roms_avg40to100m_temperature(force = "testDataFunctions"))

  expect_error(roms_avg100mtoBot_oxygen(force = "testDataFunctions"))
  expect_error(roms_avg100mtoBot_ph(force = "testDataFunctions"))
  expect_error(roms_avg100mtoBot_salinity(force = "testDataFunctions"))
  expect_error(roms_avg100mtoBot_temperature(force = "testDataFunctions"))

  expect_error(roms_phytoplankton(force = "testDataFunctions"))
  expect_error(roms_primaryproduction(force = "testDataFunctions"))
})
