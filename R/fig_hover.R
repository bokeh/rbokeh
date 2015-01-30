

# to add a HoverTool:
# - create a HoverTool model
# - need to point to glyph
# add ref to plot$attributes$tools
# add model to object

addHover <- function(obj, tooltips, rendererRef) {

  id <- genId(obj, c(rendererRef$id, "hover"))
  hov <- hoverModel(id, obj$ref, rendererRef, tooltips)

  obj$model$plot$attributes$tools[[id]] <- hov$ref
  obj$model[[id]] <- hov$model

  obj
}

hoverModel <- function(id, plotRef, rendererRef, tooltips) {
  res <- base_model_object("HoverTool", id)
  res$model$attributes$plot <- plotRef
  res$model$attributes$renderers <- list(rendererRef)
  res$model$attributes$names <- list()
  res$model$attributes$always_active <- TRUE
  res$model$attributes$tooltips <- tooltips
  res
}

