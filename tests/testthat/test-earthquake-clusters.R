test_that("events close in both space and time receive the same cluster", {
  events <- data.frame(
    latitude = c(0, 0, 30),
    longitude = c(0, 0.1, 30),
    time = as.POSIXct(c("2026-01-01 00:00:00",
                        "2026-01-01 01:00:00",
                        "2026-01-05 00:00:00"), tz = "UTC")
  )
  result <- earthquake_clusters(events, distance_km = 20, time_window_hours = 3)
  expect_equal(result$cluster_id[1], result$cluster_id[2])
  expect_true(is.na(result$cluster_id[3]))
})

test_that("clustering validates columns and thresholds", {
  expect_error(earthquake_clusters(data.frame(latitude = 0)), "latitude")
  events <- data.frame(latitude = 0, longitude = 0,
                       time = as.POSIXct("2026-01-01", tz = "UTC"))
  expect_error(earthquake_clusters(events, distance_km = 0), "positive")
})
