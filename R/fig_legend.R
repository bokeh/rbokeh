# to add a legend:
# - create a legend model
#   - add a model reference
#   - add a list of legend items

# individual legend items are lists with a label
# and a list of glyph_renderers that the legend
# entry refers to

## these are all internal functions called from make_glyph
## legends are specified through the various layer functions

add_legend <- function(fig, legends) {
  id <- gen_id(fig, "legend")

  leg <- legend_model(id, fig$x$spec$ref, legends)

  fig$x$spec$model$plot$attributes$renderers[[id]] <- leg$ref
  fig$x$spec$model[[id]] <- leg$model

  fig
}

legend_model <- function(id, plot_ref, legends) {
  res <- base_model_object("Legend", id)
  res$model$attributes$plot <- plot_ref
  res$model$attributes$legends <- legends
  res
}

