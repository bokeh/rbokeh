\donttest{
# courtesy of Joe Cheng from RStudio
library("shiny")
library("rbokeh")
library("htmlwidgets")

ui <- fluidPage(
  rbokehOutput("rbokeh")
)

server <- function(input, output, session) {
  output$rbokeh <- renderRbokeh({
    # Use invalidateLater() and jitter() to add some motion
    invalidateLater(1000, session)
    p <- figure() %>%
      ly_points(jitter(cars$speed), jitter(cars$dist))
    rbokeh:::plot.BokehFigure(p)
  })
}

shinyApp(ui, server)
}
