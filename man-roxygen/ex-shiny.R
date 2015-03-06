\donttest{
# courtesy of Joe Cheng from RStudio
library("shiny")
library("rbokeh")
library("htmlwidgets")


#' Widget output function for use in Shiny
#'
#' @export
rbokehOutput <- function(outputId, width = '100%', height = '400px'){
  shinyWidgetOutput(outputId, 'rbokeh', width, height, package = 'rbokeh')
}

#' Widget render function for use in Shiny
#'
#' @export
renderRbokeh <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, rbokehOutput, env, quoted = TRUE)
}


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
