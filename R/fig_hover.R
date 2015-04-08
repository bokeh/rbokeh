

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
  hov <- hover_model(id, fig$x$spec$ref, renderer_ref, tooltips)

  fig$x$spec$model$plot$attributes$tools[[id]] <- hov$ref
  fig$x$spec$model[[id]] <- hov$model

  fig
}

hover_model <- function(id, plot_ref, renderer_ref, tooltips) {
  res <- base_model_object("HoverTool", id)
  res$model$attributes$plot <- plot_ref
  res$model$attributes$renderers <- list(renderer_ref)
  res$model$attributes$names <- list()
  res$model$attributes$always_active <- TRUE
  res$model$attributes$tooltips <- tooltips
  res
}

