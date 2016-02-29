# to add a HoverTool:
# - create a HoverTool model
# - need to point to glyph
# add ref to plot$attributes$tools
# add model to object

## this is only used internally
## users cannot manually add a hover tool
## it must be done through the hover argument to the layer functions

add_hover <- function(fig, tooltips, renderer_ref) {
  id <- gen_id(fig, c(renderer_ref$id, "hover"))
  hov_model <- hover_model(id, fig$x$spec$ref, renderer_ref, tooltips)
  fig$x$spec$model$plot$attributes$tools[[id]] <- hov_model$ref
  fig$x$spec$model[[id]] <- hov_model$model

  fig
}

add_hover_callback <- function(fig, callback, ref_layer) {

  hov_id <- gen_id(fig, c(callback, ref_layer, "hov_callback"))

  nm <- paste(ref_layer, "glyph_rend", sep = "_")
  renderer_ref <- fig$x$spec$callback$layers[[ref_layer]][[nm]]

  hov_model <- hover_model(hov_id, fig$x$spec$ref, renderer_ref, tooltips = NA)

  callback <- handle_hover_callback(callback, fig$x$spec$callback$layers)

  cb_id <- gen_id(fig, c(renderer_ref$id, "HoverCallback",
    callback$args, callback$lname))
  cb_model <- customjs_model(id = cb_id,
    code = callback$code, args = callback$args)
  hov_model$model$attributes$callback <- cb_model$ref
  fig$x$spec$model[[cb_id]] <- cb_model$model
  fig$x$spec$model$plot$attributes$tools[[hov_id]] <- hov_model$ref
  fig$x$spec$model[[hov_id]] <- hov_model$model

  fig
}

hover_model <- function(id, plot_ref, renderer_ref, tooltips) {
  res <- base_model_object("HoverTool", id)
  res$model$attributes$plot <- plot_ref
  if(is.null(renderer_ref)) {
    res$model$attributes$renderers <- NULL
  } else {
    res$model$attributes$renderers <- list(renderer_ref)
  }
  res$model$attributes$names <- list()
  res$model$attributes$always_active <- TRUE
  res$model$attributes$tooltips <- tooltips
  res
}
