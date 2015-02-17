
## this is only used internally
## users cannot manually add a tap to open url tool
## it must be done through the url argument to the layer functions
add_tap_url <- function(fig, url, renderer_ref) {

  u_id <- gen_id(fig, c(renderer_ref$id, "url"))
  act <- open_url_model(u_id, url)

  id <- gen_id(fig, c(renderer_ref$id, "TapTool"))
  tap <- tap_model(id, fig$ref, renderer_ref, act$ref)

  fig$model$plot$attributes$tools[[id]] <- tap$ref
  fig$model[[id]] <- tap$model

  fig$model[[u_id]] <- act$model

  fig
}

tap_model <- function(id, plot_ref, renderer_ref, action_ref) {
  res <- base_model_object("TapTool", id)
  res$model$attributes$plot <- plot_ref
  res$model$attributes$renderers <- list(renderer_ref)
  res$model$attributes$names <- list()
  res$model$attributes$action <- action_ref
  res
}

open_url_model <- function(id, url) {
  res <- base_model_object("OpenURL", id)
  res$model$attributes$url <- url

  res
}
