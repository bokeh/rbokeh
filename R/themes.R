#' Set the theme for a figure
#'
#' @param fig a figure to set the theme for
#' @param theme theme
#' @export
set_theme <- function(fig, theme) {
  if(is.function(theme))
    theme <- theme()
  fig$x$spec$theme <- theme
  fig
}

#' Themes
#' @rdname themes
#' @export
bk_default_theme <- function() {
  list(
    discrete = list(
      glyph = pal_bk_glyph(),
      fill_color = pal_tableau("Tableau10"),
      line_color = pal_tableau("Tableau10"),
      text_color = pal_tableau("Tableau10"),
      fill_alpha = 1,
      line_alpha = 1,
      text_alpha = 1,
      line_dash = pal_bk_line_dash(),
      line_width = pal_bk_line_width(),
      size = pal_size()
    ),
    continuous = list(
      glyph = pal_bk_glyph(),
      fill_color = pal_gradient(),
      line_color = pal_gradient(),
      text_color = pal_gradient(),
      fill_alpha = 1,
      line_alpha = 1,
      text_alpha = 1,
      line_dash = pal_bk_line_dash(),
      line_width = pal_bk_line_width(),
      size = pal_size()
    ),
    ungrouped = list(fill_color = "black", line_color = "black",
      text_color = "black", fill_alpha = 0.5, line_alpha = 1,
      size = 20, glyph = 1, line_dash = NULL, line_width = 1),
    plot = NULL,
    axis = list(axis_label_text_font_size = "12pt"),
    grid = NULL,
    legend = NULL
  )
}

#' Themes
#' @rdname themes
#' @export
#' @importFrom scales shape_pal hue_pal seq_gradient_pal
#' @importFrom ggplot2 scale_size_discrete scale_size_continuous
bk_ggplot_theme <- function() {
  gg_shape_pal <- function() {
    function(n) {
      unname(unlist(lapply(marker_dict[as.character(scales::shape_pal()(n))], function(x) x$glyph)))
    }
  }

  list(
    discrete = list(
      glyph = gg_shape_pal(),
      fill_color = scales::hue_pal(),
      line_color = scales::hue_pal(),
      text_color = scales::hue_pal(),
      fill_alpha = 1,
      line_alpha = 1,
      text_alpha = 1,
      # line_dash = ,
      # line_width = ,
      size = ggplot2::scale_size_discrete()$palette
    ),
    continuous = list(
      fill_color = scales::seq_gradient_pal(low = "#132B43", high = "#56B1F7", space = "Lab"),
      line_color = scales::seq_gradient_pal(low = "#132B43", high = "#56B1F7", space = "Lab"),
      text_color = scales::seq_gradient_pal(low = "#132B43", high = "#56B1F7", space = "Lab"),
      fill_alpha = 1,
      line_alpha = 1,
      text_alpha = 1,
      # line_dash = ,
      # line_width = ,
      size = ggplot2::scale_size_continuous()$palette
    ),
    gradient = scales::seq_gradient_pal(low = "#132B43", high = "#56B1F7", space = "Lab"),
    ungrouped = list(fill_color = "black", line_color = "black",
      text_color = "black", fill_alpha = 1, line_alpha = 1,
      size = 10, glyph = 16, line_dash = NULL, line_width = 1),
    plot = list(background_fill = "#E6E6E6",
      outline_line_color = "white"),
    grid = list(grid_line_color = "white",
      minor_grid_line_color = "white",
      minor_grid_line_alpha = 0.4),
    axis = list(axis_line_color = "white",
      major_label_text_color = "#7F7F7F",
      major_tick_line_color = "#7F7F7F",
      minor_tick_line_alpha = 0, num_minor_ticks = 2)
  )
}
