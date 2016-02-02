# http://bokeh.pydata.org/en/latest/docs/reference/models.html

#' Add "pan" tool to a Bokeh figure
#' @param dimensions a vector specifying whether the pan tool should pan with respect to the x axis ("width") and the y axis ("height") or both (c("width", "height"))
#' @template tools
#' @examples
#' \donttest{
#' # only pan on x axis
#' figure() %>% ly_points(1:10) %>%
#'  tool_pan(dimensions = "height")
#' }
#' @export
tool_pan <- function(fig, dimensions = c("width", "height")) {
  update_tool(fig, which = "pan", args = list(dimensions = dimensions, plot_ref = fig$x$spec$ref))
}

#' Add "wheel_zoom" tool to a Bokeh figure
#' @param dimensions a vector specifying whether the wheel_zoom tool should zoom with respect to the x axis ("width") and the y axis ("height") or both (c("width", "height"))
#' @template tools
#' @examples
#' \donttest{
#' # only zoom on x axis
#' figure() %>% ly_points(1:10) %>%
#'  tool_wheel_zoom(dimensions = "height")
#' }
#' @export
tool_wheel_zoom <- function(fig, dimensions = c("width", "height")) {
  update_tool(fig, which = "wheel_zoom", args = list(dimensions = dimensions, plot_ref = fig$x$spec$ref))
}

#' Add "box_zoom" tool to a Bokeh figure
#' @template tools
#' @examples
#' \donttest{
#' figure() %>% ly_points(1:10) %>%
#'  tool_box_zoom()
#' }
#' @export
tool_box_zoom <- function(fig) {
  update_tool(fig, which = "box_zoom", args = list(plot_ref = fig$x$spec$ref))
}

#' Add "save" tool to a Bokeh figure
#' @template tools
#' @examples
#' \donttest{
#' figure() %>% ly_points(1:10) %>%
#'  tool_save()
#' }
#' @export
tool_save <- function(fig) {
  update_tool(fig, which = "preview_save", args = list(plot_ref = fig$x$spec$ref))
}

#' Add "crosshair" tool to a Bokeh figure
#' @template tools
#' @examples
#' \donttest{
#' figure() %>% ly_points(1:10) %>%
#'  tool_crosshair()
#' }
#' @export
tool_crosshair <- function(fig) {
  update_tool(fig, which = "crosshair", args = list(plot_ref = fig$x$spec$ref))
}

#' Add "resize" tool to a Bokeh figure
#' @template tools
#' @examples
#' \donttest{
#' figure() %>% ly_points(1:10) %>%
#'  tool_resize()
#' }
#' @export
tool_resize <- function(fig) {
  update_tool(fig, which = "resize", args = list(plot_ref = fig$x$spec$ref))
}

#' Add "reset" tool to a Bokeh figure
#' @template tools
#' @examples
#' \donttest{
#' figure() %>% ly_points(1:10) %>%
#'  tool_reset()
#' }
#' @export
tool_reset <- function(fig) {
  update_tool(fig, which = "reset", args = list(plot_ref = fig$x$spec$ref))
}

#' Add "box_select" tool to a Bokeh figure
#' @template tools
#' @param select_every_mousemove logical - should the tool's callback be triggered on every mouse move?
#' @examples
#' \donttest{
#' figure() %>% ly_points(1:10) %>%
#'  tool_box_select()
#' }
#' @export
tool_box_select <- function(fig, select_every_mousemove = TRUE) {
  update_tool(fig, which = "box_select", args = list(plot_ref = fig$x$spec$ref,
    select_every_mousemove = select_every_mousemove))
}

#' Add "lasso_select" tool to a Bokeh figure
#' @template tools
#' @param select_every_mousemove logical - should the tool's callback be triggered on every mouse move?
#' @examples
#' \donttest{
#' figure() %>% ly_points(1:10) %>%
#'  tool_lasso_select()
#' }
#' @export
tool_lasso_select <- function(fig, select_every_mousemove = TRUE) {
  update_tool(fig, which = "lasso_select", args = list(plot_ref = fig$x$spec$ref,
    select_every_mousemove = select_every_mousemove))
}

#' @export
tool_hover <- function(fig, callback) {
  fig %>% add_hover_callback(callback, renderer_ref = NULL)
}

tool_help <- function(fig, redirect = "http://hafen.github.io/rbokeh",
  help_tooltip = "Click to learn more about rbokeh.") {
  update_tool(fig, which = "help",
    args = list(plot_ref = fig$x$spec$ref, redirect = redirect,
      help_tooltip = help_tooltip))
}

## internal methods

update_tool <- function(fig, which, args) {
  id <- gen_id(fig, which)
  args$id <- id
  args$tool_name <- get_tool_name(which)
  model <- do.call(tool_model, args)

  fig$x$spec$model$plot$attributes$tools[[model$ref$id]] <- model$ref
  fig$x$spec$model[[id]] <- model$model

  fig <- update_tool_events(fig)

  fig
}

get_tool_name <- function(x) {
  paste(underscore2camel(x), "Tool", sep = "")
}

tool_model <- function(id, tool_name, plot_ref, ...) {
  res <- base_model_object(tool_name, id)
  res$model$attributes$plot <- plot_ref
  dots <- list(...)
  dotnms <- names(dots)
  for(nm in dotnms) {
    res$model$attributes[[nm]] <- I(dots[[nm]])
  }

  res
}

tool_events <- function(id) {
  res <- base_model_object("ToolEvents", id)
  res$model$geometries <- I(NULL)
  res
}

update_tool_events <- function(fig) {
  id <- gen_id(fig, "ToolEvents")
  model <- tool_events(id)

  fig$x$spec$model$plot$attributes$tool_events <- model$ref
  fig$x$spec$model[[id]] <- model$model

  fig
}
