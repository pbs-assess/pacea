# area_mean with bccm_roms data
test_that("area_mean function works with bccm roms data", {
  # download subsetted small data
  pdata <- get_pacea_data("test_surftemp", force = TRUE)
  
  # subsetting data to one date period
  tdat <- pdata[,1]
  
  # area_mean output
  area_output <- area_mean(tdat)
  
  expect_type(area_output, "list")
})

# area_mean with oisst data
test_that("area_mean function works with oisst data", {
  # subset data to single time period
  tdat <- oisst_7day %>%
    filter(year == 2000,
           week == 1) %>%
    dplyr::select(sst)
  
  # area_mean output
  area_output <- area_mean(tdat)
  
  expect_type(area_output, "list")
})

# check errors
test_that("stop errors work", {
  # data is an sf object
  expect_error(area_mean(pdo))
  
  # data has only one column of data (and one geometry column)
  expect_error(area_mean(oisst_7day))
})

# make_isoshape testing
test_that("function make_isoshape works for both inshore and offshore polygon", {
  ipoly <- make_isoshape(inshore = TRUE)
  opoly <- make_isoshape(inshore = FALSE)
  
  expect_equal(class(ipoly)[1], "sf")
  expect_equal(class(opoly)[1], "sf")
})

