# to add a legend:
# - create a legend model
#   - add a model reference
#   - add a list of legend items

# individual legend items are lists with a label
# and a list of glyphRenderers that the legend
# entry refers to

addLegend <- function(obj, legends) {
  id <- genId(obj, "legend")

  leg <- legendModel(id, obj$ref, legends)

  obj$model$plot$attributes$renderers[[id]] <- leg$ref
  obj$model[[id]] <- leg$model

  obj
}

legendModel <- function(id, plotRef, legends) {
  res <- base_model_object("Legend", id)
  res$model$attributes$plot <- plotRef
  res$model$attributes$legends <- legends
  res
}

