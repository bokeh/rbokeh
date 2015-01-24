#' @export
x_axis <- function(obj, label, position = "below", grid = TRUE, num_minor_ticks = 5) {
  if(is.null(position))
    position <- "below"
  if(!position %in% c("below", "above")) {
    message("x axis position must be either below or above - setting to 'below'")
    position <- "below"
  }

  if(missing(label))
    label <- obj$xlab
  obj$xlab <- label
  updateAxis(obj, position = position, label = label, grid = grid, num_minor_ticks = num_minor_ticks)
}

#' @export
y_axis <- function(obj, label, position = "left", grid = TRUE, num_minor_ticks = 5) {
  if(is.null(position))
    position <- "left"
  if(!position %in% c("left", "right")) {
    message("y axis position must be either left or right - setting to 'left'")
    position <- "left"
  }

  if(missing(label))
    label <- obj$ylab
  obj$ylab <- label
  updateAxis(obj, position = position, label = label, grid = grid, num_minor_ticks = num_minor_ticks)
}

# axis ref needs to be added to plot attributes as "above", "below", "left", or "right"
# axis ref needs to be added to plot attributes -> renderers as well
# then axis model added to object
# axis model also depends on the following references:
# - plot (already have that)
# - formatter
# - ticker

# formatter model added to object
# ticker model added to object, also referred to in grid
# also create grid

updateAxis <- function(obj, position, label, grid = TRUE, num_minor_ticks = 5) {
  fId <- genId(obj, c(position, "formatter"))
  tId <- genId(obj, c(position, "ticker"))
  aId <- genId(obj, position)

  isY <- position %in% c("left", "right")

  axisType <- ifelse(isY, obj$yAxisType, obj$xAxisType)
  if(axisType == "numeric") {
    typeList <- list(format = "BasicTickFormatter", tick = "BasicTicker", axis = "LinearAxis")
  } else if(axisType == "datetime") {
    typeList <- list(format = "DatetimeTickFormatter", tick = "DatetimeTicker", axis = "DatetimeAxis")
  } else {
    typeList <- list(format = "CategoricalTickFormatter", tick = "CategoricalTicker", axis = "CategoricalAxis")
  }

  formatter <- formatterModel(typeList$format, fId)
  ticker <- tickerModel(typeList$tick, tId, num_minor_ticks)
  axis <- axisModel(type = typeList$axis, label = label, id = aId, plotRef = obj$ref, formatterRef = formatter$ref, tickerRef = ticker$ref)

  obj$model$plot$attributes[[position]][[1]] <- axis$ref
  obj$model$plot$attributes$renderers[[axis$ref$id]] <- axis$ref

  obj$model[[aId]] <- axis$model
  obj$model[[fId]] <- formatter$model
  obj$model[[tId]] <- ticker$model

  if(grid) {
    gId <- genId(obj, c(position, "grid"))
    grid <- gridModel(gId, plotRef = obj$ref, tickerRef = ticker$ref, dimension = as.integer(isY))
    obj$model$plot$attributes$renderers[[grid$ref$id]] <- grid$ref
    obj$model[[gId]] <- grid$model
  }

  obj
}

axisModel <- function(type = "LinearAxis", label = NULL, id, plotRef, formatterRef, tickerRef) {

  res <- base_model_object(type, id)
  res$model$attributes$plot <- plotRef
  res$model$attributes$axis_label <- label
  res$model$attributes$formatter <- formatterRef
  res$model$attributes$ticker <- tickerRef

  res
}

formatterModel <- function(type = "BasicTickFormatter", id) {
  base_model_object(type, id)
}

tickerModel <- function(type = "BasicTicker", id, num_minor_ticks = 5) {
  res <- base_model_object(type, id)
  res$model$attributes$num_minor_ticks = num_minor_ticks
  res
}

gridModel <- function(id, dimension = 0, plotRef, tickerRef) {
  res <- base_model_object("Grid", id)
  res$model$attributes$dimension = dimension
  res$model$attributes$plot = plotRef
  res$model$attributes$ticker = tickerRef

  res
}


