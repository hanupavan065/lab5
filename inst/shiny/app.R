library(shiny)
library(leaflet)
library(DT)
library(ggplot2)
library(earthquakes)

mag_colour <- function(x) {
  ifelse(is.na(x), "#777777",
    ifelse(x < 2, "#2c7bb6",
      ifelse(x < 4, "#abd9e9",
        ifelse(x < 5, "#ffffbf",
          ifelse(x < 6, "#fdae61", "#d7191c")))))
}

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background: #f4f6f8; color: #202a35; }
    .topbar { background: #243447; color: white; padding: 18px 22px; margin: -15px -15px 20px -15px; }
    .metric { background: white; border-radius: 8px; padding: 14px; margin-bottom: 12px; }
  "))),
  div(class = "topbar", h2("Earthquake Explorer"),
      p("Explore earthquake events from the USGS")),
  sidebarLayout(
    sidebarPanel(
      numericInput("limit", "Maximum events", value = 500, min = 1, max = 2000),
      sliderInput("mag", "Magnitude range", min = 0, max = 8,
                  value = c(0, 8), step = 0.1),
      dateRangeInput("dates", "Date range",
                     start = Sys.Date() - 30, end = Sys.Date()),
      actionButton("load", "Load / refresh data", class = "btn-primary"),
      hr(),
      h4("Nearby search"),
      numericInput("lat", "Latitude", value = 59.3293, min = -90, max = 90),
      numericInput("lon", "Longitude", value = 18.0686, min = -180, max = 180),
      numericInput("radius", "Radius (km)", value = 100, min = 1, max = 20000),
      actionButton("nearby", "Search nearby"),
      hr(),
      downloadButton("download", "Download filtered CSV")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("🌍 Interactive Map", leafletOutput("map", height = 540)),
        tabPanel("📍 Nearby Earthquakes",
                 leafletOutput("near_map", height = 420), DTOutput("near_table")),
        tabPanel("📊 Summary & Analysis",
          fluidRow(
            column(4, div(class = "metric", h4("Events"), h2(textOutput("n")))),
            column(4, div(class = "metric", h4("Mean magnitude"), h2(textOutput("mean")))),
            column(4, div(class = "metric", h4("Maximum magnitude"), h2(textOutput("max"))))
          ),
          plotOutput("hist"), plotOutput("depth"), verbatimTextOutput("summary")
        ),
        tabPanel("🗺 Static Map", plotOutput("static", height = 500)),
        tabPanel("📋 Earthquake Data", DTOutput("table"))
      )
    )
  )
)

server <- function(input, output, session) {
  events <- eventReactive(input$load, {
    get_earthquakes(
      start_time = as.character(input$dates[1]),
      end_time = as.character(input$dates[2] + 1),
      min_magnitude = input$mag[1], max_magnitude = input$mag[2],
      limit = input$limit
    )
  }, ignoreInit = FALSE)

  filtered <- reactive({
    x <- events()
    x[!is.na(x$magnitude) & x$magnitude >= input$mag[1] &
        x$magnitude <= input$mag[2], , drop = FALSE]
  })

  nearby <- eventReactive(input$nearby, {
    get_nearby_earthquakes(
      latitude = input$lat, longitude = input$lon,
      radius_km = input$radius, min_magnitude = input$mag[1],
      max_magnitude = input$mag[2], limit = input$limit
    )
  }, ignoreInit = TRUE)

  output$map <- renderLeaflet({
    x <- filtered()
    leaflet(x) |>
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
               attribution = "&copy; OpenStreetMap contributors") |>
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        radius = ~pmax(3, pmin(12, 2 + magnitude * 1.5)),
        color = ~mag_colour(magnitude), fillOpacity = 0.75, stroke = FALSE,
        popup = ~paste0("<b>Magnitude:</b> ", magnitude,
          "<br><b>Place:</b> ", place, "<br><b>Depth:</b> ", depth, " km",
          "<br><b>Time (UTC):</b> ", time)
      ) |>
      addLegend("bottomright",
        colors = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"),
        labels = c("< 2", "2–4", "4–5", "5–6", "> 6"), title = "Magnitude")
  })

  output$near_map <- renderLeaflet({
    req(input$nearby > 0)
    x <- nearby()
    leaflet(x) |>
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
               attribution = "&copy; OpenStreetMap contributors") |>
      addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 6,
                       color = ~mag_colour(magnitude), fillOpacity = 0.75,
                       stroke = FALSE, popup = ~paste(magnitude, place, sep = " — "))
  })
  output$near_table <- renderDT({
    req(input$nearby > 0)
    datatable(nearby(), options = list(pageLength = 10, scrollX = TRUE))
  })

  output$n <- renderText(nrow(filtered()))
  output$mean <- renderText({
    x <- filtered()
    if (!nrow(x)) "N/A" else round(mean(x$magnitude, na.rm = TRUE), 2)
  })
  output$max <- renderText({
    x <- filtered()
    if (!nrow(x)) "N/A" else max(x$magnitude, na.rm = TRUE)
  })
  output$summary <- renderPrint({
    x <- filtered()
    list(events = nrow(x),
         median_magnitude = if (nrow(x)) median(x$magnitude, na.rm = TRUE) else NA,
         mean_depth_km = if (nrow(x)) mean(x$depth, na.rm = TRUE) else NA)
  })
  output$hist <- renderPlot({
    x <- filtered()
    validate(need(nrow(x) > 0, "No events match these filters."))
    ggplot(x, aes(magnitude)) + geom_histogram(binwidth = 0.25,
      fill = "#3977a8", colour = "white") + theme_minimal() +
      labs(title = "Magnitude distribution", x = "Magnitude", y = "Events")
  })
  output$depth <- renderPlot({
    x <- filtered()
    validate(need(nrow(x) > 0, "No events match these filters."))
    ggplot(x, aes(magnitude, depth)) + geom_point(alpha = 0.6,
      colour = "#3977a8") + theme_minimal() +
      labs(title = "Magnitude versus depth", x = "Magnitude", y = "Depth (km)")
  })
  output$static <- renderPlot({
    x <- filtered()
    validate(need(nrow(x) > 0, "No events match these filters."))
    earthquake_map(x)
  })
  output$table <- renderDT({
    datatable(filtered(), filter = "top",
      options = list(pageLength = 15, scrollX = TRUE,
                     lengthMenu = c(10, 15, 25, 50, 100)))
  })
  output$download <- downloadHandler(
    filename = function() paste0("earthquakes_", Sys.Date(), ".csv"),
    content = function(file) write.csv(filtered(), file, row.names = FALSE)
  )
}
shinyApp(ui, server)
