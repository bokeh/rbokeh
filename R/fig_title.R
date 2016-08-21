# to add a title:
# - create a title model
#   - add a model reference
#   - add a list of title items

add_title <- function(fig, title, extra_pars = NULL) {
  title_id <- gen_id(fig, "title")
  titlemod <- title_model(title_id, fig$x$spec$ref, title, extra_pars)

  fig$x$spec$model$plot$attributes$title <- titlemod$ref
  fig$x$spec$model$title <- titlemod$model

  fig
}

title_model <- function(id, plot_ref, ttext, extra_pars) {
  res <- base_model_object("Title", id)
  res$model$attributes["plot"] <- list(NULL)
  res$model$attributes$text <- ttext

  res$model$attributes <- c(res$model$attributes, extra_pars)

  res
}

title_par_validator_map <- list(
  "align" = "align",
  "background_fill_alpha" = "num_data_spec",
  "background_fill_color" = "color",
  "border_line_alpha" = "num_data_spec",
  "border_line_cap" = "line_cap",
  "border_line_color" = "color",
  "border_line_dash" = "line_dash",
  "border_line_dash_offset" = "int",
  "border_line_dash_join" = "line_join",
  "border_line_width" = "num_data_spec",
  "offset" = "num_data_spec",
  "text_alpha" = "num_data_spec",
  "text_color" = "color",
  "text_font" = "string",
  "text_font_size" = "font_size_string",
  "text_font_style" = "font_style"
)
