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
genId <- function(obj, name = NULL) {
  digest(c(name, obj$time))
}

get_json <- function(obj) {
  names(obj$plot$attributes$tools) <- NULL
  names(obj$plot$attributes$renderers) <- NULL
  names(obj) <- NULL
  obj
}

#' @importFrom RJSONIO toJSON
#' @export
print_model_json <- function(obj, prepare = TRUE, pretty = TRUE) {
  if(prepare)
    obj <- prepare_figure(obj)
  cat(toJSON(get_json(obj$model), digits = 50, pretty = pretty))
}

underscore2camel <- function(x) {
  x <- gsub("^([a-zA-Z])", "\\U\\1", x, perl = TRUE)
  gsub("_([a-zA-Z])", "\\U\\1", x, perl = TRUE)
}
