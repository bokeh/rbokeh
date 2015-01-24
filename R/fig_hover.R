

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

# rendererID <- "ec33f1d5819832fe90976dc15dbcf6e6"
# rendererRef <- list(
#   type = obj$model[[rendererID]]$type,
#   id = obj$model[[rendererID]]$id
# )
# addHover(p, tooltips, rendererRef)

# tooltips <- list(list("x", "@x"), list("y", "@y"))

# p1 <- figure() %>%
#   lay_points(rnorm(10), rnorm(10)) %>%
#   lay_points(rnorm(10), rnorm(10))

# idx <- which(sapply(p1$model, function(x) x$type) == "GlyphRenderer")

# for(ii in idx) {
#   rendererID <- names(p1$model)[ii]
#   rendererRef <- list(
#     type = obj$model[[rendererID]]$type,
#     id = obj$model[[rendererID]]$id
#   )
#   p1 <- addHover(p1, tooltips, rendererRef)
# }

# p1 %>% addLegend()

# cat(toJSON(p1$model, pretty = TRUE))

