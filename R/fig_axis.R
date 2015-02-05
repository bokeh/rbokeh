#' @export
x_axis <- function(obj, label, position = "below", log = FALSE, grid = TRUE, num_minor_ticks = 5) {
  if(is.null(position))
    position <- "below"
  if(!position %in% c("below", "above")) {
    message("x axis position must be either below or above - setting to 'below'")
    position <- "below"
  }

  if(missing(label))
    label <- obj$xlab
  obj$xlab <- label
  update_axis(obj, position = position, label = label, grid = grid, num_minor_ticks = num_minor_ticks, log = log)
}

#' @export
y_axis <- function(obj, label, position = "left", log = FALSE, grid = TRUE, num_minor_ticks = 5) {
  if(is.null(position))
    position <- "left"
  if(!position %in% c("left", "right")) {
    message("y axis position must be either left or right - setting to 'left'")
    position <- "left"
  }

  if(missing(label))
    label <- obj$ylab
  obj$ylab <- label
  update_axis(obj, position = position, label = label, grid = grid, num_minor_ticks = num_minor_ticks, log = log)
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

update_axis <- function(obj, position, label, grid = TRUE, num_minor_ticks = 5, log = FALSE) {
  f_id <- gen_id(obj, c(position, "formatter"))
  t_id <- gen_id(obj, c(position, "ticker"))
  a_id <- gen_id(obj, position)

  is_y <- position %in% c("left", "right")

  axis_type <- ifelse(is_y, obj$y_axis_type, obj$x_axis_type)
  if(axis_type == "numeric") {
    if(log) {
      type_list <- list(format = "LogTickFormatter", tick = "LogTicker", axis = "LogAxis")
      if(is_y) {
        obj$model$plot$attributes$y_mapper_type <- "log"
      } else {
        obj$model$plot$attributes$x_mapper_type <- "log"
      }
    } else {
      type_list <- list(format = "BasicTickFormatter", tick = "BasicTicker", axis = "LinearAxis")
    }
  } else if(axis_type == "datetime") {
    type_list <- list(format = "DatetimeTickFormatter", tick = "DatetimeTicker", axis = "DatetimeAxis")
  } else {
    type_list <- list(format = "CategoricalTickFormatter", tick = "CategoricalTicker", axis = "CategoricalAxis")
  }

  formatter <- formatter_model(type_list$format, f_id)
  ticker <- ticker_model(type_list$tick, t_id, num_minor_ticks, log)
  axis <- axis_model(type = type_list$axis, label = label, id = a_id, plot_ref = obj$ref, formatter_ref = formatter$ref, ticker_ref = ticker$ref)

  obj$model$plot$attributes[[position]][[1]] <- axis$ref
  obj$model$plot$attributes$renderers[[axis$ref$id]] <- axis$ref

  obj$model[[a_id]] <- axis$model
  obj$model[[f_id]] <- formatter$model
  obj$model[[t_id]] <- ticker$model

  if(grid) {
    g_id <- gen_id(obj, c(position, "grid"))
    grid <- grid_model(g_id, plot_ref = obj$ref, ticker_ref = ticker$ref, dimension = as.integer(is_y))
    obj$model$plot$attributes$renderers[[grid$ref$id]] <- grid$ref
    obj$model[[g_id]] <- grid$model
  }

  if(is_y) {
    obj$has_y_axis <- TRUE
  } else {
    obj$has_x_axis <- TRUE
  }

  obj
}

axis_model <- function(type = "LinearAxis", label = NULL, id, plot_ref, formatter_ref, ticker_ref) {

  res <- base_model_object(type, id)
  res$model$attributes$plot <- plot_ref
  res$model$attributes$axis_label <- label
  res$model$attributes$formatter <- formatter_ref
  res$model$attributes$ticker <- ticker_ref

  res
}

formatter_model <- function(type = "BasicTickFormatter", id) {
  base_model_object(type, id)
}

ticker_model <- function(type = "BasicTicker", id, num_minor_ticks = 5, log = FALSE) {
  res <- base_model_object(type, id)
  res$model$attributes$num_minor_ticks = num_minor_ticks

  res
}


grid_model <- function(id, dimension = 0, plot_ref, ticker_ref) {
  res <- base_model_object("Grid", id)
  res$model$attributes$dimension = dimension
  res$model$attributes$plot = plot_ref
  res$model$attributes$ticker = ticker_ref

  res
}


