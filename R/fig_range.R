#' @export
x_range <- function(obj, dat) {
  update_range(obj, "x", dat)
}

#' @export
y_range <- function(obj, dat) {
  update_range(obj, "y", dat)
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
update_range <- function(obj, axis = "x", dat) {
  if(is.numeric(dat)) {
    type <- "Range1d"
    dat <- range(dat, na.rm = TRUE)
  } else {
    type <- "FactorRange"
  }

  range_name <- paste0(axis, "_range")

  id <- gen_id(obj, range_name)
  model <- range_model(type, id, dat)

  obj$model$plot$attributes[[range_name]] <- model$ref
  obj$model[[id]] <- model$model
  obj[[paste0("has_", axis, "_range")]] <- TRUE

  obj
}

