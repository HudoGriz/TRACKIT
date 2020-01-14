require(shiny)
shiny::runApp(
  "./app",
  launch.browser = FALSE,
  port = 5555,
  host = "0.0.0.0"
  )  