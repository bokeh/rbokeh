#' Instantiate an rbokeh figure
#'
#' @param data default dataset to use
#' @export
figure <- function(data = NULL) {
  plt <- Plot$new()
  obj <- htmlwidgets::createWidget(
    name = "rbokeh",
    x = list(
      mods = list(plot = plt),
      debug = FALSE
    ),
    sizingPolicy = htmlwidgets::sizingPolicy(defaultWidth = 500, defaultHeight = 500),
    preRenderHook = rbokeh_prerender,
    width = 600,
    height = 600,
    package = "rbokeh"
  )

  obj
}

rbokeh_prerender <- function(obj) {
  mods <- prepare_figure(obj$x$mods)

  mod_list <- unname(unlist(mods))
  mod_list <- mod_list[sapply(mod_list, function(a) inherits(a, "Model"))]

  fig <- list(list(
    version = "0.12.5",
    title = "Bokeh Figure",
    roots = list(
      root_ids = list(mods$plot$get_prop("id")),
      references = lapply(mod_list, function(a) a$get_all_props())
    )
  ))
  docid <- digest::digest(Sys.time())
  names(fig) <- docid

  obj$x$docs_json <- fig

  obj$x$elementid <- digest::digest(Sys.time())
  obj$x$modelid <- mods$plot$get_prop("id")
  obj$x$docid <- docid

  obj$x$mods <- NULL
  obj$preRenderHook <- NULL # nolint

  attr(obj$x, "TOJSON_ARGS") <- list(auto_unbox = TRUE, null = "null", na = "null")

  obj
}
