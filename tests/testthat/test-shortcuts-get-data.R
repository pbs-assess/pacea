test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


test_that("shortcuts work", {
  expect_length(roms_bottom_oxygen(force = TRUE), 325)
  expect_length(roms_bottom_ph(force = TRUE), 325)
  expect_length(roms_bottom_salinity(force = TRUE), 325)
  expect_length(roms_bottom_temperature(force = TRUE), 325)
  
  expect_length(roms_surface_oxygen(force = TRUE), 325)
  expect_length(roms_surface_ph(force = TRUE), 325)
  expect_length(roms_surface_salinity(force = TRUE), 325)
  expect_length(roms_surface_temperature(force = TRUE), 325)
  
  expect_length(roms_avg0to40m_oxygen(force = TRUE), 325)
  expect_length(roms_avg0to40m_ph(force = TRUE), 325)
  expect_length(roms_avg0to40m_salinity(force = TRUE), 325)
  expect_length(roms_avg0to40m_temperature(force = TRUE), 325)
  
  expect_length(roms_avg40to100m_oxygen(force = TRUE), 325)
  expect_length(roms_avg40to100m_ph(force = TRUE), 325)
  expect_length(roms_avg40to100m_salinity(force = TRUE), 325)
  expect_length(roms_avg40to100m_temperature(force = TRUE), 325)
  
  expect_length(roms_avg100mtoBot_oxygen(force = TRUE), 325)
  expect_length(roms_avg100mtoBot_ph(force = TRUE), 325)
  expect_length(roms_avg100mtoBot_salinity(force = TRUE), 325)
  expect_length(roms_avg100mtoBot_temperature(force = TRUE), 325)
  
  expect_length(roms_phytoplankton(force = TRUE), 325)
  expect_length(roms_primaryproduction(force = TRUE), 325)
  
})
