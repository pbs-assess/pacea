test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("Errors work", {
  
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
