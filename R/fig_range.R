#' @export
x_range <- function(obj, dat) {
  updateRange(obj, "x", dat)
}

#' @export
y_range <- function(obj, dat) {
  updateRange(obj, "y", dat)
}

rangeModel <- function(type = "Range1d", id, dat) {
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
updateRange <- function(obj, axis = "x", dat) {
  if(is.numeric(dat)) {
    type <- "Range1d"
    dat <- range(dat, na.rm = TRUE)
  } else {
    type <- "FactorRange"
  }

  rangeName <- paste(axis, "_range", sep = "")

  id <- genId(obj, rangeName)
  model <- rangeModel(type, id, dat)

  obj$model$plot$attributes[[rangeName]] <- model$ref
  obj$model[[id]] <- model$model

  obj
}

