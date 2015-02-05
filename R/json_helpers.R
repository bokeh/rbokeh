base_model_object <- function(type, id) {
  list(
    model = list(
      type = type,
      id = id,
      attributes = list(
        id = id,
        tags = list(),
        doc = NULL
      )
    ),
    ref = list(
      type = type,
      id = id
    )
  )
}

#' @importFrom digest digest
gen_id <- function(obj, name = NULL) {
  digest(c(name, obj$time))
}

remove_model_names <- function(obj) {
  names(obj$plot$attributes$tools) <- NULL
  names(obj$plot$attributes$renderers) <- NULL
  names(obj) <- NULL
  obj
}

#' @importFrom RJSONIO toJSON
#' @export
print_model_json <- function(obj, prepare = TRUE, pretty = TRUE, pbcopy = FALSE) {
  if(prepare) {
    if(inherits(obj, "BokehFigure")) {
      obj <- prepare_figure(obj)
    } else if(inherits(obj, "BokehGridPlot")) {
      obj <- prepare_gridplot(obj)
    }
  }

  file <- ""
  if(pbcopy)
    file <- pipe("pbcopy")
  cat(toJSON(remove_model_names(obj$model), digits = 50, pretty = pretty), file = file)
}

underscore2camel <- function(x) {
  x <- gsub("^([a-zA-Z])", "\\U\\1", x, perl = TRUE)
  gsub("_([a-zA-Z])", "\\U\\1", x, perl = TRUE)
}
