# http://bokeh.pydata.org/en/latest/docs/reference/models.html

#' @export
tool_pan <- function(obj, dimensions = c("width", "height")) {
  updateTool(obj, which = "pan", args = list(dimensions = dimensions, plotRef = obj$ref))
}

#' @export
tool_wheel_zoom <- function(obj, dimensions = c("width", "height")) {
  updateTool(obj, which = "wheel_zoom", args = list(dimensions = dimensions, plotRef = obj$ref))
}

#' @export
tool_box_zoom <- function(obj) {
  updateTool(obj, which = "box_zoom", args = list(plotRef = obj$ref))
}

#' @export
tool_save <- function(obj) {
  updateTool(obj, which = "preview_save", args = list(plotRef = obj$ref))
}

#' @export
tool_crosshair <- function(obj) {
  updateTool(obj, which = "crosshair", args = list(plotRef = obj$ref))
}

#' @export
tool_tap <- function(obj) {
  updateTool(obj, which = "tap", args = list(plotRef = obj$ref))
}

#' @export
tool_resize <- function(obj) {
  updateTool(obj, which = "resize", args = list(plotRef = obj$ref))
}

#' @export
tool_reset <- function(obj) {
  updateTool(obj, which = "reset", args = list(plotRef = obj$ref))
}

#' @export
tool_box_select <- function(obj, select_every_mousemove = TRUE) {
  updateTool(obj, which = "box_select", args = list(plotRef = obj$ref,
    select_every_mousemove = select_every_mousemove))
}

#' @export
tool_lasso_select <- function(obj, select_every_mousemove = TRUE) {
  updateTool(obj, which = "lasso_select", args = list(plotRef = obj$ref,
    select_every_mousemove = select_every_mousemove))
}

updateTool <- function(obj, which, args) {
  id <- genId(obj, which)
  args$id <- id
  args$toolName <- getToolName(which)
  model <- do.call(toolModel, args)

  obj$model$plot$attributes$tools[[model$ref$id]] <- model$ref
  obj$model[[id]] <- model$model

  obj <- updateToolEvents(obj)

  obj
}

getToolName <- function(x) {
  paste(underscore2camel(x), "Tool", sep = "")
}

toolModel <- function(id, toolName, plotRef, ...) {
  res <- base_model_object(toolName, id)
  res$model$attributes$plot <- plotRef
  dots <- list(...)
  dotnms <- names(dots)
  for(nm in dotnms) {
    res$model$attributes[[nm]] <- I(dots[[nm]])
  }

  res
}

toolEvents <- function(id) {
  res <- base_model_object("ToolEvents", id)
  res$model$geometries <- I(NULL)
  res
}

updateToolEvents <- function(obj) {
  id <- genId(obj, "ToolEvents")
  model <- toolEvents(id)

  obj$model$plot$attributes$tool_events <- model$ref
  obj$model[[id]] <- model$model

  obj
}
