# nolint start
#' Widget output function for use in Shiny.
#' @param outputId Output variable to read from.
#' @param width A valid CSS unit for the width or a number, which will be coerced to a string and have "px" appended.
#' @param height a valid CSS unit for the height or a number, which will be coerced to a string and have "px" appended.
#'
#' @export
#' @examples
#' \dontrun{
#' library("shiny")
#' library("rbokeh")
#'
#' ui <- fluidPage(rbokehOutput("rbokeh"))
#'
#' server <- function(input, output, session) {
#'   output$rbokeh <- renderRbokeh({
#'     # Use invalidateLater() and jitter() to add some motion
#'     invalidateLater(1000, session)
#'     figure() %>%
#'       ly_points(jitter(cars$speed), jitter(cars$dist))
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
rbokehOutput <- function(outputId, width = "100%", height = "400px") {
  htmlwidgets::shinyWidgetOutput(outputId, "rbokeh", width, height, package = "rbokeh")
}

#' Widget render function for use in Shiny
#' @param expr an expression that generates a rbokeh figure
#' @param env the environment in which to evaluate expr.
#' @param quoted is expr a quoted expression (with quote())? This is useful if you want to save an expression in a variable.
#'
#' @seealso \code{\link{rbokehOutput}} for an example.
#'
#' @export
renderRbokeh <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr <- substitute(expr)
  htmlwidgets::shinyRenderWidget(expr, rbokehOutput, env, quoted = TRUE)
}
# nolint end
