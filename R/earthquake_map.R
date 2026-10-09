utils::globalVariables(c("longitude", "latitude", "magnitude"))
#' Plot earthquake locations
#'
#' Creates a static longitude-latitude plot with point size and colour mapped
#' to earthquake magnitude.
#'
#' @param earthquakes Data frame containing `longitude`, `latitude`, and
#'   `magnitude` columns, such as the output of [earthquakes::get_earthquakes()].
#' @return A ggplot object.
#' @examples
#' \dontrun{earthquake_map(get_earthquakes(limit = 100))}
#' @export
earthquake_map <- function(earthquakes) {
  required <- c("longitude", "latitude", "magnitude")
  if (!is.data.frame(earthquakes) || !all(required %in% names(earthquakes))) {
    stop("earthquakes must contain longitude, latitude, and magnitude columns.",
         call. = FALSE)
  }
  if (nrow(earthquakes) == 0L) {
    stop("earthquakes contains no rows to plot.", call. = FALSE)
  }

  ggplot2::ggplot(
    earthquakes,
    ggplot2::aes(x = longitude, y = latitude,
                 size = magnitude, colour = magnitude)
  ) +
    ggplot2::geom_point(alpha = 0.75, na.rm = TRUE) +
    ggplot2::scale_colour_gradient(low = "blue", high = "red", na.value = "grey70") +
    ggplot2::coord_quickmap() +
    ggplot2::labs(
      title = "Earthquake Locations",
      x = "Longitude", y = "Latitude",
      size = "Magnitude", colour = "Magnitude"
    ) +
    ggplot2::theme_minimal()
}
