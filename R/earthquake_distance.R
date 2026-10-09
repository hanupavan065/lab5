#' Calculate distance from a reference location to each earthquake
#'
#' Uses the Haversine formula to calculate great-circle distance and adds the
#' result as a `distance_km` column.
#'
#' @param earthquakes Data frame containing `latitude` and `longitude` columns.
#' @param latitude Latitude of the reference point, between -90 and 90.
#' @param longitude Longitude of the reference point, between -180 and 180.
#' @return The input data frame with an added `distance_km` column.
#' @examples
#' events <- data.frame(latitude = c(0, 0), longitude = c(0, 1))
#' earthquake_distance(events, latitude = 0, longitude = 0)
#' @export
earthquake_distance <- function(earthquakes, latitude, longitude) {
  if (!is.data.frame(earthquakes) ||
      !all(c("latitude", "longitude") %in% names(earthquakes))) {
    stop("earthquakes must be a data frame with latitude and longitude columns.",
         call. = FALSE)
  }
  .validate_coordinate(latitude, "latitude", -90, 90)
  .validate_coordinate(longitude, "longitude", -180, 180)

  if (anyNA(earthquakes$latitude) || anyNA(earthquakes$longitude) ||
      any(!is.finite(earthquakes$latitude)) ||
      any(!is.finite(earthquakes$longitude)) ||
      any(earthquakes$latitude < -90 | earthquakes$latitude > 90) ||
      any(earthquakes$longitude < -180 | earthquakes$longitude > 180)) {
    stop("Earthquake coordinates must be complete and within valid ranges.",
         call. = FALSE)
  }

  lat1 <- latitude * pi / 180
  lat2 <- earthquakes$latitude * pi / 180
  delta_lat <- lat2 - lat1
  delta_lon <- (earthquakes$longitude - longitude) * pi / 180
  a <- sin(delta_lat / 2)^2 +
    cos(lat1) * cos(lat2) * sin(delta_lon / 2)^2
  a <- pmin(1, pmax(0, a))

  earthquakes$distance_km <- 6371.0088 * 2 * atan2(sqrt(a), sqrt(1 - a))
  earthquakes
}
