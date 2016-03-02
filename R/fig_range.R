#' Update x axis range in a Bokeh figure
#' @param fig figure to modify
#' @param dat either a vector (min, max) if the axis is numeric, or a vector of values if the axis is categorical.  In the latter case, the order in which the values are supplied is how they will be arranged on the axis.
#' @param callback TODO
#' @family ranges
#' @example man-roxygen/ex-range.R
#' @export
x_range <- function(fig, dat = NULL, callback = NULL) {
  update_range(fig, "x", dat, callback)
}

#' Update y axis range in a Bokeh figure
#' @inheritParams x_range
#' @family ranges
#' @example man-roxygen/ex-range.R
#' @export
y_range <- function(fig, dat = NULL, callback = NULL) {
  update_range(fig, "y", dat, callback)
}

# range ref needs to be added to plot attributes as "x_range" or "y_range"
# then range model added to object
update_range <- function(fig, axis = "x", dat = NULL, callback = NULL) {

  range_name <- paste0(axis, "_range")
  id <- gen_id(fig, range_name)
  axis_name <- paste0(axis, "_axis_type")
  type <- ifelse(fig$x$spec[[axis_name]] == "categorical", "FactorRange", "Range1d")
  model <- base_model_object(type, id)

  # first get the model if it exists
  if(!is.null(fig$x$spec$model[[id]]))
    model$model <- fig$x$spec$model[[id]]

  if(!is.null(dat)) {
    if(inherits(dat, c("Date", "POSIXt")))
      dat <- to_epoch(dat)

    if(type == "Range1d") {
      # preserve backward xlim/ylim specification
      if(!(length(dat) == 2 && dat[1] > dat[2]))
        dat <- range(dat, na.rm = TRUE)
      model$model$attributes$start <- dat[1]
      model$model$attributes$end <- dat[2]
    } else if(type == "FactorRange") {
      model$model$attributes$factors <- I(dat)
    }

    # this way we know a range was alredy specified
    # so we don't need to do it at the render time
    fig$x$spec[[paste0("has_", axis, "_range")]] <- TRUE
  }

  if(!is.null(callback)) {
    if(is.character(callback))
      callback <- structure(list(code = callback, args = NULL, lnames = NULL),
        class = "customCallback")
    callback$args <- c(callback$args, list(range = model$ref))
    callback <- handle_range_callback(callback, fig$x$spec$callback$layers)
    if(!is.null(callback)) {
      cb_id <- gen_id(fig, c(paste0(range_name, "_callback"),
        callback$args, callback$lname))
      cb_model <- customjs_model(id = cb_id,
        code = callback$code, args = callback$args)
      fig$x$spec$model[[cb_id]] <- cb_model$model
      model$model$attributes$callback <- cb_model$ref
    }
  }

  fig$x$spec$model$plot$attributes[[range_name]] <- model$ref
  fig$x$spec$model[[id]] <- model$model

  fig
}

range_model <- function(type = "Range1d", id, dat) {
  res <- base_model_object(type, id)

  if(type == "Range1d") {
    res$model$attributes$start <- dat[1]
    res$model$attributes$end <- dat[2]
  } else if(type == "FactorRange") {
    res$model$attributes$factors <- I(dat)
  }

  res
}
