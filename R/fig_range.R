#' Update x axis range in a Bokeh figure
#' @param fig figure to modify
#' @param dat either a vector (min, max) if the axis is numeric, or a vector of values if the axis is categorical.  In the latter case, the order in which the values are supplied is how they will be arranged on the axis.
#' @family ranges
#' @example man-roxygen/ex-range.R
#' @export
x_range <- function(fig, dat) {
  update_range(fig, "x", dat)
}

#' Update y axis range in a Bokeh figure
#' @inheritParams x_range
#' @family ranges
#' @example man-roxygen/ex-range.R
#' @export
y_range <- function(fig, dat) {
  update_range(fig, "y", dat)
}

range_model <- function(type = "Range1d", id, dat) {
  res <- base_model_object(type, id)

  if(type == "Range1d") {
    res$model$attributes$start <- dat[1]
    res$model$attributes$end <- dat[2]
  } else if(type == "FactorRange") {
    res$model$attributes$factors <- dat
  }

  res
}

# range ref needs to be added to plot attributes as "x_range" or "y_range"
# then range model added to object
update_range <- function(fig, axis = "x", dat) {
  if(inherits(dat, c("Date", "POSIXct")))
    dat <- to_epoch(dat)

  if(is.numeric(dat)) {
    type <- "Range1d"
    dat <- range(dat, na.rm = TRUE)
  } else {
    type <- "FactorRange"
  }

  range_name <- paste0(axis, "_range")

  id <- gen_id(fig, range_name)
  model <- range_model(type, id, dat)

  fig$x$spec$model$plot$attributes[[range_name]] <- model$ref
  fig$x$spec$model[[id]] <- model$model
  fig$x$spec[[paste0("has_", axis, "_range")]] <- TRUE

  fig
}

