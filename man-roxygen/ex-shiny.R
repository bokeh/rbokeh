\dontrun{
library("shiny")
library("rbokeh")

ui <- fluidPage(
  rbokehOutput("rbokeh")
)

server <- function(input, output, session) {
  output$rbokeh <- renderRbokeh({
    # Use invalidateLater() and jitter() to add some motion
    invalidateLater(1000, session)
    figure() %>%
      ly_points(jitter(cars$speed), jitter(cars$dist))
  })
}

shinyApp(ui, server)


library("shiny")
library("rbokeh")

ui <- fluidPage(
  rbokehOutput("rbokeh", width = 500, height = 540),
  textOutput("x_range_text")
)

server <- function(input, output, session) {
  output$rbokeh <- renderRbokeh({
    figure() %>% ly_points(1:10) %>%
      x_range(callback = shiny_callback("x_range"))
  })

  output$x_range_text <- reactive({
    xrng <- input$x_range
    if(!is.null(xrng)) {
      paste0("factors: ", xrng$factors, ", start: ", xrng$start,
        ", end: ", xrng$end)
    } else {
      "waiting for axis event..."
    }
  })
}

shinyApp(ui, server)
}
