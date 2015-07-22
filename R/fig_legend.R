#' Finer control over legend
#'
#' @param fig figure to modify
#' @param orientation ('top_right', 'top_left', 'bottom_left', 'bottom_right') The location where the legend should draw itself.
#' @template dots-legend
#' @examples
#' figure() %>%
#'   ly_points(Sepal.Length, Sepal.Width, color = Species, data = iris) %>%
#'   legend_control(orientation = "top_left", label_width = 2,
#'     border_line_alpha = 0.5, border_line_cap = "butt")
#' @export
legend_control <- function(fig, orientation = "top_right", ...) {
  extra_pars <- handle_extra_pars(list(orientation = orientation, ...), legend_par_validator_map)
  fig$spec$legend_attrs <- extra_pars
  fig
}

# to add a legend:
# - create a legend model
#   - add a model reference
#   - add a list of legend items

# individual legend items are lists with a label
# and a list of glyph_renderers that the legend
# entry refers to

## these are all internal functions called from make_glyph
## legends are specified through the various layer functions

add_legend <- function(fig, legends, extra_pars) {
  id <- gen_id(fig, "legend")

  leg <- legend_model(id, fig$x$spec$ref, legends, extra_pars)

  fig$x$spec$model$plot$attributes$renderers[[id]] <- leg$ref
  fig$x$spec$model[[id]] <- leg$model

  fig
}

legend_model <- function(id, plot_ref, legends, extra_pars) {
  res <- base_model_object("Legend", id)
  res$model$attributes$plot <- plot_ref
  res$model$attributes$legends <- legends

  res$model$attributes <- c(res$model$attributes, extra_pars)

  res
}

legend_par_validator_map <- list(
  "orientation" = "legend_orientation",
  "border_line_alpha" = "num_data_spec",
  "border_line_cap" = "line_cap",
  "border_line_color" = "color",
  "border_line_dash" = "line_dash",
  "border_line_dash_offset" = "int",
  "border_line_join" = "line_join",
  "border_line_width" = "num_data_spec",
  "glyph_height" = "int",
  "glyph_width" = "int",
  "label_height" = "int",
  "label_standoff" = "int",
  "label_text_align" = "text_align",
  "label_text_alpha" = "num_data_spec",
  "label_text_baseline" = "text_baseline",
  "label_text_color" = "color",
  "label_text_font" = "string",
  "label_text_font_size" = "font_size_string",
  "label_text_font_style" = "font_style",
  "label_width" = "int",
  "legend_padding" = "int",
  "legend_spacing" = "int"
)
