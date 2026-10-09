#' Query earthquake events from the USGS API
#'
#' Retrieve earthquake events, optionally filtering by time and magnitude.
#' Queries are sent to the USGS FDSN event service.
#'
#' @param start_time Optional start date/time. Accepts a `Date`, `POSIXct`, or
#'   ISO-style character value such as `"2026-01-01"`.
#' @param end_time Optional end date/time in the same formats as `start_time`.
#' @param min_magnitude Optional minimum magnitude.
#' @param max_magnitude Optional maximum magnitude.
#' @param limit Maximum number of events to request; must be a positive integer.
#'
#' @return A data frame with `id`, `magnitude`, `place`, `time`, `longitude`,
#'   `latitude`, and `depth` columns. `time` is in UTC.
#' @examples
#' \dontrun{
#' events <- get_earthquakes(start_time = "2026-01-01",
#'                           min_magnitude = 3, limit = 100)
#' }
#' @export
get_earthquakes <- function(start_time = NULL, end_time = NULL,
                            min_magnitude = NULL, max_magnitude = NULL,
                            limit = 100) {
  .validate_limit(limit)
  .validate_time(start_time, "start_time")
  .validate_time(end_time, "end_time")
  .validate_magnitude(min_magnitude, "min_magnitude")
  .validate_magnitude(max_magnitude, "max_magnitude")

  if (!is.null(min_magnitude) && !is.null(max_magnitude) &&
      min_magnitude > max_magnitude) {
    stop("min_magnitude must be less than or equal to max_magnitude.",
         call. = FALSE)
  }

  params <- list(format = "geojson", limit = as.integer(limit))
  if (!is.null(start_time)) params$starttime <- .format_usgs_time(start_time)
  if (!is.null(end_time)) params$endtime <- .format_usgs_time(end_time)
  if (!is.null(min_magnitude)) params$minmagnitude <- min_magnitude
  if (!is.null(max_magnitude)) params$maxmagnitude <- max_magnitude

  .query_usgs(params)
}

.validate_limit <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) ||
      !is.finite(x) || x < 1 || x != floor(x)) {
    stop("limit must be a single positive integer.", call. = FALSE)
  }
}

.validate_magnitude <- function(x, name) {
  if (!is.null(x) && (!is.numeric(x) || length(x) != 1L ||
                      is.na(x) || !is.finite(x))) {
    stop(name, " must be NULL or a single finite number.", call. = FALSE)
  }
}

.validate_time <- function(x, name) {
  if (is.null(x)) return(invisible(NULL))
  if (length(x) != 1L || is.na(x)) {
    stop(name, " must be NULL or a single date/time value.", call. = FALSE)
  }
  if (inherits(x, "Date") || inherits(x, "POSIXt")) return(invisible(NULL))
  if (!is.character(x) ||
      !grepl("^\\d{4}-\\d{2}-\\d{2}([T ][0-9]{2}:[0-9]{2}(:[0-9]{2})?(Z|[+-][0-9:]+)?)?$", x)) {
    stop(name, " must be a Date, POSIXct, or ISO-style date/time string.",
         call. = FALSE)
  }
  invisible(NULL)
}

.format_usgs_time <- function(x) {
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if (inherits(x, "POSIXt")) {
    return(format(x, "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
  }
  x
}


.query_usgs <- function(params) {
  req <- httr2::request(
    "https://earthquake.usgs.gov/fdsnws/event/1/query"
  )
  
  # Add all query parameters to the HTTP request.
  req <- do.call(
    httr2::req_url_query,
    c(list(.req = req), params)
  )
  
  response <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      stop(
        "Unable to retrieve earthquake data from USGS: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  
  data <- httr2::resp_body_json(response, simplifyVector = FALSE)
  
  features <- data$features
  
  if (is.null(features) || length(features) == 0) {
    return(.empty_earthquakes())
  }
  
  rows <- lapply(features, .extract_earthquake)
  
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}



.extract_earthquake <- function(feature) {
  coords <- feature$geometry$coordinates
  props <- feature$properties

  data.frame(
    id = as.character(feature$id),
    magnitude = if (is.null(props$mag)) NA_real_ else as.numeric(props$mag),
    place = if (is.null(props$place)) NA_character_ else as.character(props$place),
    time = as.POSIXct(as.numeric(props$time) / 1000,
                      origin = "1970-01-01", tz = "UTC"),
    longitude = as.numeric(coords[[1L]]),
    latitude = as.numeric(coords[[2L]]),
    depth = as.numeric(coords[[3L]]),
    stringsAsFactors = FALSE
  )
}

.empty_earthquakes <- function() {
  data.frame(
    id = character(), magnitude = numeric(), place = character(),
    time = as.POSIXct(character(), tz = "UTC"),
    longitude = numeric(), latitude = numeric(), depth = numeric(),
    stringsAsFactors = FALSE
  )
}
