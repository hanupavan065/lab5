# earthquakes

An R package to query and explore earthquake events from the USGS event API.

## Features

- Query events by time and magnitude.
- Search within a geographic radius measured in kilometres.
- Calculate great-circle distances using the Haversine formula.
- Identify exploratory spatiotemporal clusters.
- Plot earthquake locations with ggplot2.
- Explore the data with a Shiny application.

## Install for development

Clone the repository, open it in RStudio, then run:

```r
install.packages(c("devtools", "httr2", "ggplot2", "testthat"))
devtools::document()
devtools::install()
library(earthquakes)
```

## Examples

```r
events <- get_earthquakes(min_magnitude = 3, limit = 100)
nearby <- get_nearby_earthquakes(59.3293, 18.0686, radius_km = 100)
nearby <- earthquake_distance(nearby, 59.3293, 18.0686)
grouped <- earthquake_clusters(nearby, distance_km = 50,
                               time_window_hours = 24)
earthquake_map(events)
```

## Shiny

From the repository root, run `shiny::runApp("inst/shiny")`.

## Important limitation

The clustering function is an exploratory connected-components method. It is
not a formal seismological aftershock-sequence detector.
