test_that("Haversine distances are correct to a useful tolerance", {
  events <- data.frame(latitude = c(0, 0), longitude = c(0, 1))
  result <- earthquake_distance(events, latitude = 0, longitude = 0)
  expect_equal(result$distance_km[1], 0, tolerance = 1e-8)
  expect_equal(result$distance_km[2], 111.2, tolerance = 1)
})

test_that("distance rejects missing coordinate columns", {
  expect_error(earthquake_distance(data.frame(x = 1), 0, 0),
               "latitude and longitude")
  expect_error(earthquake_distance(data.frame(latitude = 0, longitude = 0),
                                   91, 0), "latitude")
})
