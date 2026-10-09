test_that("nearby search validates coordinates and radius", {
  expect_error(get_nearby_earthquakes(91, 0), "latitude")
  expect_error(get_nearby_earthquakes(0, 181), "longitude")
  expect_error(get_nearby_earthquakes(0, 0, radius_km = 0), "positive")
  expect_error(get_nearby_earthquakes(0, 0, limit = -1), "positive integer")
})
