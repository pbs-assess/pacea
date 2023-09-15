test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("Errors work", {
  
  library(sf)
  
  dat <- data.frame(x = runif(5, 0, 10), y = runif(5, 0, 10), var = rnorm(5))
  
  extent <- st_bbox(c(xmin = 0, ymin = 0, xmax= 10, ymax = 10),crs = NA)
  
  # cellsize value; 'as' argument; loc vector length must equal 2; loc name not in data
  expect_error(point2rast(dat, spatobj = extent))
  expect_error(point2rast(dat, spatobj = extent, cellsize = 0.5, as = "Spat"))
  expect_error(point2rast(dat, spatobj = extent, loc = c("x"), cellsize = 0.5))
  expect_error(point2rast(dat, spatobj = extent, loc = c("lon", "lat"), cellsize = 0.5))
  
  dat2 <- data.frame(var = rnorm(5))
  ll1 <- matrix(runif(10, 0, 10), ncol = 1)
  ll2 <- matrix(runif(8, 0, 10), ncol = 2)
  
  # loc must be 2 col; loc length must of same length as data
  expect_error(point2rast(dat2, spatobj = extent, loc = ll1, cellsize = 0.5))
  expect_error(point2rast(dat2, spatobj = extent, loc = ll2, cellsize = 0.5))
})


test_that("Warnings for loc coordinates outside extent of object to interpolate to.", {
  library(sf)
  
  dat <- data.frame(x = runif(5, 10, 20), y = runif(5, 10, 20), var = rnorm(5))
  
  extent <- st_bbox(c(xmin = 0, ymin = 0, xmax= 10, ymax = 10), crs = NA)
  
  expect_warning(point2rast(dat, extent, loc = c("x","y"), cellsize = 0.5, nnmax = 2, as = "SpatRast"))
})



test_that("Output of SpatialRaster and SpatialVector from dataframe successful.", {
  library(sf)
  
  dat <- data.frame(x = runif(5, 0, 10), y = runif(5, 0, 10), var = rnorm(5))
  
  extent <- st_bbox(c(xmin = 0, ymin = 0, xmax= 10, ymax = 10), crs = NA)
  
  output.rast <- point2rast(dat, extent, loc = c("x","y"), cellsize = 0.5, nnmax = 2, as = "SpatRast")
  output.vect <- point2rast(dat, extent, loc = c("x","y"), cellsize = 0.5, nnmax = 2, as = "SpatVect")
  
  expect_equal(class(output.rast)[1], "SpatRaster")
  expect_equal(nrow(output.rast), 20)
  
  expect_equal(class(output.vect)[1], "SpatVector")
  expect_equal(nrow(output.vect), 400)
})


test_that("Output from vector and matrix of data and coordinates successful.", {
  library(sf)
  
  dat <- rnorm(5)
  ll <- matrix(runif(10, 0, 10), ncol = 2)
  
  extent <- st_bbox(c(xmin = 0, ymin = 0, xmax= 10, ymax = 10), crs = NA)
  
  output.rast <- point2rast(dat, spatobj = extent, loc = ll, cellsize = 0.5, nnmax = 2, as = "SpatRast")
  
  expect_equal(class(output.rast)[1], "SpatRaster")
  expect_equal(nrow(output.rast), 20)
  
})


test_that("Output from sf object successful", {
  library(sf)
  
  dat <- data.frame(x = runif(5, 0, 10), y = runif(5, 0, 10), var = rnorm(5)) %>% 
    st_as_sf(coords = c("x", "y"))
  
  extent <- st_bbox(c(xmin = 0, ymin = 0, xmax= 10, ymax = 10), crs = NA)
  
  output.rast <- point2rast(dat, extent, cellsize = 0.5, nnmax = 2, as = "SpatRast")
  
  expect_equal(class(output.rast)[1], "SpatRaster")
  expect_equal(nrow(output.rast), 20)
})



test_that("Output from Spatial object successful", {
  library(sf)
  
  dat <- data.frame(x = runif(5, 0, 10), y = runif(5, 0, 10), var = rnorm(5)) %>% 
    st_as_sf(coords = c("x", "y")) %>% as_Spatial()
    
  extent <- st_bbox(c(xmin = 0, ymin = 0, xmax= 10, ymax = 10), crs = NA)
  
  output.rast <- point2rast(dat, extent, cellsize = 0.5, nnmax = 2, as = "SpatRast")
  
  expect_equal(class(output.rast)[1], "SpatRaster")
  expect_equal(nrow(output.rast), 20)
})


test_that("Including extent of SpatObj works", {
  library(sf)
  
  dat <- data.frame(x = runif(5, 0, 10), y = runif(5, 0, 10), var = rnorm(5)) %>% 
    st_as_sf(coords = c("x", "y"))
  
  outer <- matrix(c(0,0,10,0,10,10,0,10,0,0), ncol=2, byrow=TRUE)
  
  sf_poly <- st_polygon(x = list(outer)) %>% 
    st_sfc() %>% st_as_sf() %>%
    st_set_crs("EPSG: 4326")
  
  output.rast <- point2rast(dat, sf_poly, cellsize = 0.5, nnmax = 2, as = "SpatRast")
  
  expect_equal(class(output.rast)[1], "SpatRaster")
  expect_equal(nrow(output.rast), 20)
})






