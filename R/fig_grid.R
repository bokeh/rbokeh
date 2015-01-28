
addGridPlot <- function(obj) {

  id <- genId(obj, "GridPlot")
  grd <- gridPlotModel(id, "a", "b")

  obj$model$plot$attributes$tools[[id]] <- hov$ref
  obj$model[[id]] <- hov$model

  obj
}

gridPlotModel <- function(id, plotRefs, toolEvents) {
  res <- base_model_object("GridPlot", id)

  res$model$attributes$children <- list(plotRefs)
  res$model$attributes$tool_events <- toolEvents

  res$model$attributes$plot_width <- width
  res$model$attributes$plot_height <- height
  res$model$attributes$x_range <- NULL
  res$model$attributes$y_range <- NULL
  res$model$attributes$left <- list()
  res$model$attributes$below <- list()
  res$model$attributes$right <- list()
  res$model$attributes$above <- list()
  res$model$attributes$renderers <- list()
  res$model$attributes$extra_y_ranges <- structure(list(), .Names = character(0))
  res$model$attributes$extra_x_ranges <- structure(list(), .Names = character(0))

  res
}

