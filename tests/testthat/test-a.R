# Test helper function a()

test_that("a() works on a tibble", {
  expect_equal(oni %>% a(),
               as.data.frame(oni))
})
