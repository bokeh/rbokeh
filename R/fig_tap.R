
add_url <- function(fig, url, renderer_ref) {

  tap_id <- gen_id(fig, c(renderer_ref$id, "TapTool"))
  tap_model <- tap_model(tap_id, fig$x$spec$ref, renderer_ref)

  url_id <- gen_id(fig, c(renderer_ref$id, "url"))
  url_model <- open_url_model(url_id, url)

  tap_model$model$attributes$callback <- url_model$ref

  fig$x$spec$model[[url_id]] <- url_model$model
  fig$x$spec$model$plot$attributes$tools[[tap_id]] <- tap_model$ref
  fig$x$spec$model[[tap_id]] <- tap_model$model

  fig
}

add_tap_callback <- function(fig, callback, renderer_ref) {

  tap_id <- gen_id(fig, c(renderer_ref$id, "tap_callback"))
  tap_model <- tap_model(tap_id, fig$x$spec$ref, renderer_ref)

  callback <- handle_tap_callback(callback, tap_model)

  cb_id <- gen_id(fig, c(renderer_ref$id, "TapCallback"))

  cb_model <- customjs_model(id = cb_id,
    code = callback$code, args = callback$args)

  tap_model$model$attributes$callback <- cb_model$ref

  fig$x$spec$model[[cb_id]] <- cb_model$model
  fig$x$spec$model$plot$attributes$tools[[tap_id]] <- tap_model$ref
  fig$x$spec$model[[tap_id]] <- tap_model$model

  fig
}

tap_model <- function(id, plot_ref, renderer_ref) {
  res <- base_model_object("TapTool", id)
  res$model$attributes$plot <- plot_ref
  res$model$attributes$renderers <- list(renderer_ref)
  res$model$attributes$names <- list()
  res
}

open_url_model <- function(id, url) {
  res <- base_model_object("OpenURL", id)
  res$model$attributes$url <- url
  res
}
