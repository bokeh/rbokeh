#' Widget output function for use in Shiny
#' @param outputId output variable to read from
#' @param width a valid CSS unit for the width or a number, which will be coerced to a string and have "px" appended.
#' @param height a valid CSS unit for the height or a number, which will be coerced to a string and have "px" appended.
#' 
#' @example man-roxygen/ex-shiny.R
#'
#' @export
rbokehOutput <- function(outputId, width = '100%', height = '400px'){
  shinyWidgetOutput(outputId, 'rbokeh', width, height, package = 'rbokeh')
}

#' Widget render function for use in Shiny
#' @param expr an expression that generates a rbokeh figure
#' @param env the environment in which to evaluate expr.
#' @param quoted is expr a quoted expression (with quote())? This is useful if you want to save an expression in a variable.
#'
#' @seealso \code{\link{rbokehOutput}} for an example in Shiny
#'
#' @export
renderRbokeh <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr <- substitute(expr)
  shinyRenderWidget(expr, rbokehOutput, env, quoted = TRUE)
}