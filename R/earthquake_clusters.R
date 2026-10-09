#' Identify exploratory spatiotemporal earthquake clusters
#'
#' Events are linked when they are within both the spatial and time thresholds.
#' Connected components are used, so two events can belong to the same group
#' through a chain of intermediate events.
#'
#' @param earthquakes Data frame containing `latitude`, `longitude`, and `time`.
#' @param distance_km Maximum distance for linking two events.
#' @param time_window_hours Maximum time difference for linking two events.
#' @return The input data frame with an integer `cluster_id` column. Events
#'   that do not belong to a multi-event cluster have `NA`.
#' @details This is an exploratory grouping method, not a formal aftershock
#'   sequence detector. Its straightforward implementation is best suited to
#'   small or moderate data sets.
#' @examples
#' \dontrun{grouped <- earthquake_clusters(events, 50, 24)}
#' @export
earthquake_clusters <- function(earthquakes, distance_km = 50,
                                time_window_hours = 24) {
  required <- c("latitude", "longitude", "time")
  if (!is.data.frame(earthquakes) || !all(required %in% names(earthquakes))) {
    stop("earthquakes must contain latitude, longitude, and time columns.",
         call. = FALSE)
  }
  if (!is.numeric(distance_km) || length(distance_km) != 1L ||
      is.na(distance_km) || !is.finite(distance_km) || distance_km <= 0) {
    stop("distance_km must be a positive finite number.", call. = FALSE)
  }
  if (!is.numeric(time_window_hours) || length(time_window_hours) != 1L ||
      is.na(time_window_hours) || !is.finite(time_window_hours) ||
      time_window_hours <= 0) {
    stop("time_window_hours must be a positive finite number.", call. = FALSE)
  }
  if (anyNA(earthquakes$latitude) || anyNA(earthquakes$longitude) ||
      any(!is.finite(earthquakes$latitude)) ||
      any(!is.finite(earthquakes$longitude)) ||
      any(earthquakes$latitude < -90 | earthquakes$latitude > 90) ||
      any(earthquakes$longitude < -180 | earthquakes$longitude > 180)) {
    stop("Coordinates must be complete and within valid ranges.", call. = FALSE)
  }

  event_time <- tryCatch(as.POSIXct(earthquakes$time, tz = "UTC"),
                         error = function(e) NULL)
  if (is.null(event_time) || anyNA(event_time)) {
    stop("time must contain valid, non-missing date/time values.", call. = FALSE)
  }

  n <- nrow(earthquakes)
  earthquakes$cluster_id <- rep(NA_integer_, n)
  if (n < 2L) return(earthquakes)

  lat <- earthquakes$latitude * pi / 180
  lon <- earthquakes$longitude * pi / 180
  time_seconds <- as.numeric(event_time)
  visited <- rep(FALSE, n)
  cluster_number <- 0L

  for (i in seq_len(n)) {
    if (visited[i]) next
    queue <- i
    visited[i] <- TRUE
    component <- integer(0)

    while (length(queue) > 0L) {
      current <- queue[1L]
      queue <- queue[-1L]
      component <- c(component, current)
      candidates <- which(!visited)
      if (!length(candidates)) next

      dlat <- lat[candidates] - lat[current]
      dlon <- lon[candidates] - lon[current]
      a <- sin(dlat / 2)^2 +
        cos(lat[current]) * cos(lat[candidates]) * sin(dlon / 2)^2
      a <- pmin(1, pmax(0, a))
      distance <- 6371.0088 * 2 * atan2(sqrt(a), sqrt(1 - a))
      hours <- abs(time_seconds[candidates] - time_seconds[current]) / 3600
      linked <- candidates[distance <= distance_km & hours <= time_window_hours]

      if (length(linked)) {
        visited[linked] <- TRUE
        queue <- c(queue, linked)
      }
    }

    if (length(component) >= 2L) {
      cluster_number <- cluster_number + 1L
      earthquakes$cluster_id[component] <- cluster_number
    }
  }
  earthquakes
}
