# to add a HoverTool:
# - create a HoverTool model
# - need to point to glyph
# add ref to toolbar$attributes$tools
# add model to object

## this is only used internally
## users cannot manually add a hover tool
## it must be done through the hover argument to the layer functions

add_hover <- function(fig, tooltips, renderer_ref) {
  id <- gen_id(fig, c(renderer_ref$id, "hover"))
  hov_model <- hover_model(id, fig$x$spec$ref, renderer_ref, tooltips)

  if (is.null(fig$x$spec$model$toolbar)) {
    tbid <- gen_id(fig, "Toolbar")
    tbmodel <- toolbar_model(tbid)
    tbmodel$model$attributes["logo"] <- list(fig$x$spec$logo)
    fig$x$spec$model$plot$attributes$toolbar <- tbmodel$ref
    fig$x$spec$model$toolbar <- tbmodel$model
    fig$x$spec$model$plot$attributes$tool_events <- list()
    fig <- update_tool_events(fig)
  }

  fig$x$spec$model$toolbar

  fig$x$spec$model$toolbar$attributes$tools[[id]] <- hov_model$ref
  fig$x$spec$model[[id]] <- hov_model$model

  fig
}

add_hover_callback <- function(fig, callback, ref_layer) {

  if (is.null(fig$x$spec$model$toolbar)) {
    tbid <- gen_id(fig, "Toolbar")
    tbmodel <- toolbar_model(tbid)
    tbmodel$model$attributes["logo"] <- list(fig$x$spec$logo)
    fig$x$spec$model$plot$attributes$toolbar <- tbmodel$ref
    fig$x$spec$model$toolbar <- tbmodel$model
    fig$x$spec$model$plot$attributes$tool_events <- list()
    fig <- update_tool_events(fig)
  }

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
  fig$x$spec$model$toolbar$attributes$tools[[hov_id]] <- hov_model$ref
  fig$x$spec$model[[hov_id]] <- hov_model$model

  fig
}

hover_model <- function(id, plot_ref, renderer_ref, tooltips) {
  res <- base_model_object("HoverTool", id)
  res$model$attributes$plot <- plot_ref
  if (is.null(renderer_ref)) {
    res$model$attributes$renderers <- NULL
  } else {
    res$model$attributes$renderers <- list(renderer_ref)
  }
  res$model$attributes$names <- list()

  # TODO: expose any of these to user?
  res$model$attributes$anchor <- "center"
  res$model$attributes$attachment <- "horizontal"
  res$model$attributes$line_policy <- "prev"
  res$model$attributes$mode <- "mouse"
  res$model$attributes$point_policy <- "snap_to_data"

  res$model$attributes$tooltips <- tooltips
  res
}
