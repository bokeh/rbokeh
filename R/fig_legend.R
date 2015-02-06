# to add a legend:
# - create a legend model
#   - add a model reference
#   - add a list of legend items

# individual legend items are lists with a label
# and a list of glyph_renderers that the legend
# entry refers to

## these are all internal functions called from make_glyph
## legends are specified through the various layer functions

add_legend <- function(obj, legends) {
  id <- gen_id(obj, "legend")

  leg <- legend_model(id, obj$ref, legends)

  obj$model$plot$attributes$renderers[[id]] <- leg$ref
  obj$model[[id]] <- leg$model

  obj
}

legend_model <- function(id, plot_ref, legends) {
  res <- base_model_object("Legend", id)
  res$model$attributes$plot <- plot_ref
  res$model$attributes$legends <- legends
  res
}

