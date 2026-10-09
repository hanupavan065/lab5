test_that("earthquake_map returns a ggplot object", {
  events <- data.frame(longitude = c(0, 1), latitude = c(0, 1),
                       magnitude = c(2, 4))
  expect_s3_class(earthquake_map(events), "ggplot")
})

test_that("earthquake_map validates input", {
  expect_error(earthquake_map(data.frame(x = 1)), "longitude, latitude, and magnitude")
  expect_error(earthquake_map(data.frame(longitude = numeric(),
    latitude = numeric(), magnitude = numeric())), "no rows")
})
