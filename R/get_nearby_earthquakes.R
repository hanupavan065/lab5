#' Query earthquakes near a geographic location
#'
#' Uses the USGS API's geographic search with a radius in kilometres.
#'
#' @param latitude Centre latitude, between -90 and 90.
#' @param longitude Centre longitude, between -180 and 180.
#' @param radius_km Search radius in kilometres; must be positive.
#' @param start_time Optional start date/time accepted by [earthquakes::get_earthquakes()].
#' @param end_time Optional end date/time accepted by [earthquakes::get_earthquakes()].
#' @param min_magnitude Optional minimum magnitude.
#' @param max_magnitude Optional maximum magnitude.
#' @param limit Maximum number of events to request; positive integer.
#' @return A data frame with the same columns as [earthquakes::get_earthquakes()].
#' @examples
#' \dontrun{
#' nearby <- get_nearby_earthquakes(59.3293, 18.0686, radius_km = 100)
#' }
#' @export
get_nearby_earthquakes <- function(latitude, longitude, radius_km = 100,
                                   start_time = NULL, end_time = NULL,
                                   min_magnitude = NULL, max_magnitude = NULL,
                                   limit = 1000) {
  .validate_coordinate(latitude, "latitude", -90, 90)
  .validate_coordinate(longitude, "longitude", -180, 180)
  .validate_limit(limit)

  if (!is.numeric(radius_km) || length(radius_km) != 1L ||
      is.na(radius_km) || !is.finite(radius_km) || radius_km <= 0) {
    stop("radius_km must be a single positive finite number.", call. = FALSE)
  }
  .validate_time(start_time, "start_time")
  .validate_time(end_time, "end_time")
  .validate_magnitude(min_magnitude, "min_magnitude")
  .validate_magnitude(max_magnitude, "max_magnitude")

  if (!is.null(min_magnitude) && !is.null(max_magnitude) &&
      min_magnitude > max_magnitude) {
    stop("min_magnitude must be less than or equal to max_magnitude.",
         call. = FALSE)
  }

  params <- list(
    format = "geojson", limit = as.integer(limit),
    latitude = latitude, longitude = longitude, maxradiuskm = radius_km
  )
  if (!is.null(start_time)) params$starttime <- .format_usgs_time(start_time)
  if (!is.null(end_time)) params$endtime <- .format_usgs_time(end_time)
  if (!is.null(min_magnitude)) params$minmagnitude <- min_magnitude
  if (!is.null(max_magnitude)) params$maxmagnitude <- max_magnitude

  .query_usgs(params)
}

.validate_coordinate <- function(x, name, lower, upper) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) ||
      !is.finite(x) || x < lower || x > upper) {
    stop(name, " must be a single finite number between ", lower, " and ",
         upper, ".", call. = FALSE)
  }
}
