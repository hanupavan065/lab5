test_that("get_earthquakes validates inputs before making a request", {
  expect_error(get_earthquakes(limit = 0), "positive integer")
  expect_error(get_earthquakes(limit = 1.5), "positive integer")
  expect_error(get_earthquakes(min_magnitude = 5, max_magnitude = 2),
               "less than or equal")
  expect_error(get_earthquakes(start_time = "bad"), "ISO-style")
})

test_that("a small API query returns the documented structure", {
  skip_on_cran()
  result <- get_earthquakes(limit = 5)
  expect_s3_class(result, "data.frame")
  expect_named(result, c("id", "magnitude", "place", "time",
                         "longitude", "latitude", "depth"))
  expect_lte(nrow(result), 5)
})
