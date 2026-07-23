options(shiny.host = "0.0.0.0", shiny.port = 8080)
shiny::runApp(
  shiny::shinyAppFile("shiny_aggregation.R"),
  host = "0.0.0.0", port = 8080, launch.browser = FALSE
)
