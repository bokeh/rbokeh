# http://bokeh.pydata.org/en/latest/docs/reference/models.html

#' @export
tool_pan <- function(obj, dimensions = c("width", "height")) {
  update_tool(obj, which = "pan", args = list(dimensions = dimensions, plot_ref = obj$ref))
}

#' @export
tool_wheel_zoom <- function(obj, dimensions = c("width", "height")) {
  update_tool(obj, which = "wheel_zoom", args = list(dimensions = dimensions, plot_ref = obj$ref))
}

#' @export
tool_box_zoom <- function(obj) {
  update_tool(obj, which = "box_zoom", args = list(plot_ref = obj$ref))
}

#' @export
tool_save <- function(obj) {
  update_tool(obj, which = "preview_save", args = list(plot_ref = obj$ref))
}

#' @export
tool_crosshair <- function(obj) {
  update_tool(obj, which = "crosshair", args = list(plot_ref = obj$ref))
}

#' @export
tool_tap <- function(obj) {
  update_tool(obj, which = "tap", args = list(plot_ref = obj$ref))
}

#' @export
tool_resize <- function(obj) {
  update_tool(obj, which = "resize", args = list(plot_ref = obj$ref))
}

#' @export
tool_reset <- function(obj) {
  update_tool(obj, which = "reset", args = list(plot_ref = obj$ref))
}

#' @export
tool_box_select <- function(obj, select_every_mousemove = TRUE) {
  update_tool(obj, which = "box_select", args = list(plot_ref = obj$ref,
    select_every_mousemove = select_every_mousemove))
}

#' @export
tool_lasso_select <- function(obj, select_every_mousemove = TRUE) {
  update_tool(obj, which = "lasso_select", args = list(plot_ref = obj$ref,
    select_every_mousemove = select_every_mousemove))
}

update_tool <- function(obj, which, args) {
  id <- gen_id(obj, which)
  args$id <- id
  args$tool_name <- get_tool_name(which)
  model <- do.call(tool_model, args)

  obj$model$plot$attributes$tools[[model$ref$id]] <- model$ref
  obj$model[[id]] <- model$model

  obj <- update_tool_events(obj)

  obj
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

update_tool_events <- function(obj) {
  id <- gen_id(obj, "ToolEvents")
  model <- tool_events(id)

  obj$model$plot$attributes$tool_events <- model$ref
  obj$model[[id]] <- model$model

  obj
}
