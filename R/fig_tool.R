#' @export
tool_pan <- function(obj, dimensions = c("width", "height")) {
  updateTool(obj, which = "pan", args = list(dimensions = dimensions, plotRef = obj$ref))
}

#' @export
tool_wheel_zoom <- function(obj, dimensions = c("width", "height")) {
  updateTool(obj, which = "wheelZoom", args = list(dimensions = dimensions, plotRef = obj$ref))
}

#' @export
tool_box_zoom <- function(obj) {
  updateTool(obj, which = "boxZoom", args = list(plotRef = obj$ref))
}

#' @export
tool_save <- function(obj) {
  updateTool(obj, which = "previewSave", args = list(plotRef = obj$ref))
}

#' @export
tool_resize <- function(obj) {
  updateTool(obj, which = "resize", args = list(plotRef = obj$ref))
}

#' @export
tool_reset <- function(obj) {
  updateTool(obj, which = "reset", args = list(plotRef = obj$ref))
}

updateTool <- function(obj, which, args) {
  id <- genId(obj, which)
  args$id <- id
  model <- do.call(toolModels()[[which]], args)

  obj$model$plot$attributes$tools[[model$ref$id]] <- model$ref
  obj$model[[id]] <- model$model

  obj <- updateToolEvents(obj)

  obj
}

toolModels <- function() {
  list(
    pan = function(id, dimensions, plotRef) {
      res <- baseModelObject("PanTool", id)
      res$model$attributes$plot <- plotRef
      res$model$attributes$dimensions <- I(dimensions)
      res
    },
    wheelZoom = function(id, dimensions, plotRef) {
      res <- baseModelObject("WheelZoomTool", id)
      res$model$attributes$plot <- plotRef
      res$model$attributes$dimensions <- I(dimensions)
      res
    },
    boxZoom = function(id, dimensions, plotRef) {
      res <- baseModelObject("BoxZoomTool", id)
      res$model$attributes$plot <- plotRef
      res
    },
    previewSave = function(id, dimensions, plotRef) {
      res <- baseModelObject("PreviewSaveTool", id)
      res$model$attributes$plot <- plotRef
      res
    },
    resize = function(id, dimensions, plotRef) {
      res <- baseModelObject("ResizeTool", id)
      res$model$attributes$plot <- plotRef
      res
    },
    reset = function(id, dimensions, plotRef) {
      res <- baseModelObject("ResetTool", id)
      res$model$attributes$plot <- plotRef
      res
    }
  )
}


toolEvents <- function(id) {
  res <- baseModelObject("ToolEvents", id)
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