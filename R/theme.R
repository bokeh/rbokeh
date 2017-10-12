#' Set the theme for a figure
#'
#' @param fig a figure to set the theme for
#' @param theme theme
# @example man-roxygen/ex-theme.R
#' @export
set_theme <- function(fig, theme) {
  if (is.function(theme))
    theme <- theme()

  fig$x$theme <- theme

  fig
}

# TODO: add lists for all possible tickers
# TODO: add TickFormatter options

#' Themes
#' @rdname themes
#' @export
bk_default_theme <- function() {
  list(
    discrete = list(
      glyph = pal_bk_glyph(),
      color = pal_tableau("Tableau10"),
      color_unknown = "lightgray",
      text_color = pal_tableau("Tableau10"),
      alpha = 1,
      text_alpha = 1,
      line_dash = pal_bk_line_dash(),
      line_width = list(exponent = 1, log = FALSE, min = 5, max = 8),
      size = list(exponent = 0.5, log = FALSE, min = 5, max = 30)
    ),
    continuous = list(
      glyph = pal_bk_glyph(),
      color = pal_gradient(),
      color_unknown = "lightgray",
      color_n_intervals = 4,
      text_color = pal_gradient(),
      alpha = 1,
      text_alpha = 1,
      line_dash = pal_bk_line_dash(),
      line_width = list(exponent = 1, log = FALSE, min = 5, max = 8),
      size = list(exponent = 0.5, log = FALSE, min = 5, max = 30)
    ),
    ungrouped = list(fill_color = "black", line_color = "black",
      text_color = "black", fill_alpha = 0.5, line_alpha = 1,
      size = 20, glyph = 1, line_dash = NULL, line_width = 1),
    plot = list(),
    title = NULL,
    axis = list(
      x = list(axis_label_text_font_size = "12pt"),
      y = list(axis_label_text_font_size = "12pt")
    ),
    ticker = list(x = list(), y = list()),
    grid = list(x = list(), y = list()),
    legend = NULL
  )
}

#' Themes
#' @rdname themes
#' @export
#' @importFrom scales shape_pal hue_pal
bk_ggplot_theme <- function() {
  gg_shape_pal <- function() {
    function(n) {
      unname(unlist(lapply(marker_dict[as.character(scales::shape_pal()(n))],
        function(x) x$glyph)))
    }
  }

  list(
    discrete = list(
      glyph = gg_shape_pal(),
      color = scales::hue_pal(),
      color_unknown = "lightgray",
      text_color = scales::hue_pal(),
      alpha = 1,
      text_alpha = 1,
      # line_dash = ,
      line_width = list(exponent = 1, log = FALSE, min = 5, max = 8),
      size = list(exponent = 0.5, log = FALSE, min = 5, max = 30)
    ),
    continuous = list(
      color = pal_gradient(c("#132B43", "#56B1F7"), space = "Lab"),
      color_unknown = "lightgray",
      text_color = pal_gradient(c("#132B43", "#56B1F7"), space = "Lab"),
      alpha = 1,
      text_alpha = 1,
      # line_dash = ,
      line_width = list(exponent = 1, log = FALSE, min = 5, max = 8),
      size = list(exponent = 0.5, log = FALSE, min = 5, max = 30)
    ),
    gradient = pal_gradient(c("#132B43", "#56B1F7"), space = "Lab"),
    ungrouped = list(fill_color = "black", line_color = "black",
      text_color = "black", fill_alpha = 1, line_alpha = 1,
      size = 10, glyph = 16, line_dash = NULL, line_width = 1),
    plot = list(
      background_fill_color = "#E6E6E6",
      outline_line_color = "white"
    ),
    grid = list(
      x = list(
        grid_line_color = "white",
        minor_grid_line_color = "white",
        minor_grid_line_alpha = 0.4
      ),
      y = list(
        grid_line_color = "white",
        minor_grid_line_color = "white",
        minor_grid_line_alpha = 0.4
      )
    ),
    axis = list(
      x = list(
        axis_line_color = "white",
        major_label_text_color = "#7F7F7F",
        major_tick_line_color = "#7F7F7F",
        minor_tick_line_alpha = 0,
        axis_label_text_font_style = "normal"
      ),
      y = list(
        axis_line_color = "white",
        major_label_text_color = "#7F7F7F",
        major_tick_line_color = "#7F7F7F",
        minor_tick_line_alpha = 0,
        axis_label_text_font_style = "normal"
      )
    ),
    ticker = list(
      x = list(
        num_minor_ticks = 2
      ),
      y = list(
        num_minor_ticks = 2
      )
    )
  )
}

#' Override theme parameters for general plot attributes
#'
#' @param fig figure to modify
#' @param background_fill_color (color) background color of plot
#' @param background_fill_alpha (numeric) background color alpha of plot
#' @param border_fill_color (color) fill color of border area of plot
#' @param border_fill_alpha (numeric) fill color alpha of border area of plot
#' @param align ('left', 'right', 'center') The text align for the plot title.
#' @param text_alpha The text alpha for the plot title.
#' @param text_baseline ('top', 'middle', 'bottom', 'alphabetic', 'hanging') The text baseline for the plot title.
#' @param text_color (color) The text color for the plot title.
#' @param text_font (string) The text font for the plot title.
#' @param text_font_size (string - e.g. '12pt') The text font size for the plot title.
#' @param text_font_style ('normal', 'italic', 'bold') The text font style for the plot title.
#' @param pars optionally specify a named list of all parameters - useful when dealing with theme lists
# @examples
# figure(title = "asdf") %>%
#   ly_points(1:10) %>%
#   theme_title(text_color = "red")
#' @export
theme_title <- function(fig,
  pars = NULL,
  background_fill_color = "white",
  background_fill_alpha = 1,
  border_fill_color = "white",
  border_fill_alpha = 1,
  align = "left",
  text_alpha = 1,
  text_baseline = "bottom",
  text_color = "#444444",
  text_font = "Helvetica",
  text_font_size = "12pt",
  text_font_style = "normal"
) {

  # this will provide a list of all user-specified arguments
  # (can ignore the defaults for the ones they don't specify
  # because they are defaults if not specified in bokeh)
  args <- get_specified_args(nnms = "fig")

  fig$x$theme$title <- args

  fig
}

#' Override theme parameters for axis attributes
#'
#' @param fig figure to modify
#' @param which which grids to apply attributes to ("x" and/or "y")
#' @param num_minor_ticks number of minor ticks
#' @param axis_label_standoff (integer) The distance in pixels that the axis labels should be offset from the tick labels.
#' @param axis_label_text_align ('left', 'right', 'center') The text align of the axis label.
#' @param axis_label_text_alpha (numeric) The text alpha of the axis label.
#' @param axis_label_text_baseline ('top', 'middle', 'bottom', 'alphabetic', 'hanging') The text baseline of the axis label.
#' @param axis_label_text_color (color) The text color of the axis label.
#' @param axis_label_text_font (string) The text font of the axis label.
#' @param axis_label_text_font_size (string - e.g. '12pt') The text font size of the axis label.
#' @param axis_label_text_font_style ('normal', 'italic', 'bold') The text font style of the axis label.
#' @param axis_line_alpha (numeric) The line alpha of the axis line.
#' @param axis_line_cap ('butt', 'round', 'square') The line cap of the axis line.
#' @param axis_line_color (color) The line color of the axis line.
#' @param axis_line_dash The line dash of the axis line.
#' @param axis_line_dash_offset (integer) The line dash offset of the axis line.
#' @param axis_line_join ('miter', 'round', 'bevel') The line join of the axis line.
#' @param axis_line_width (integer) The line width of the axis line.
#' @param major_label_orientation ('horizontal', 'vertical', or angle in degrees) What direction the major label text should be oriented. If a number is supplied, the angle of the text is measured from horizontal.
#' @param major_label_standoff (integer) The distance in pixels that the major tick labels should be offset from the associated ticks.
#' @param major_label_text_align ('left', 'right', 'center') The text align of the major tick labels.
#' @param major_label_text_alpha (numeric) The text alpha of the major tick labels.
#' @param major_label_text_baseline ('top', 'middle', 'bottom', 'alphabetic', 'hanging') The text baseline of the major tick labels.
#' @param major_label_text_color (color) The text color of the major tick labels.
#' @param major_label_text_font (string - 'Helvetica') The text font of the major tick labels.
#' @param major_label_text_font_size (string - e.g. '12pt') The text font size of the major tick labels.
#' @param major_label_text_font_style ('normal', 'italic', 'bold') The text font style of the major tick labels.
#' @param major_tick_in (integer) The distance in pixels that major ticks should extend into the main plot area.
#' @param major_tick_line_alpha (numeric) The line alpha of the major ticks.
#' @param major_tick_line_cap ('butt', 'round', 'square') The line cap of the major ticks.
#' @param major_tick_line_color (color) The line color of the major ticks.
#' @param major_tick_line_dash The line dash of the major ticks.
#' @param major_tick_line_dash_offset (integer) The line dash offset of the major ticks.
#' @param major_tick_line_join ('miter', 'round', 'bevel') The line join of the major ticks.
#' @param major_tick_line_width (integer) The line width of the major ticks.
#' @param major_tick_out (integer) The distance in pixels that major ticks should extend out of the main plot area.
#' @param minor_tick_in (integer) The distance in pixels that minor ticks should extend into the main plot area.
#' @param minor_tick_line_alpha (numeric) The line alpha of the minor ticks.
#' @param minor_tick_line_cap ('butt', 'round', 'square') The line cap of the minor ticks.
#' @param minor_tick_line_color (color) The line color of the minor ticks.
#' @param minor_tick_line_dash The line dash of the minor ticks.
#' @param minor_tick_line_dash_offset (integer) The line dash offset of the minor ticks.
#' @param minor_tick_line_join ('miter', 'round', 'bevel') The line join of the minor ticks.
#' @param minor_tick_line_width (integer) The line width of the minor ticks.
#' @param minor_tick_out (integer) The distance in pixels that major ticks should extend out of the main plot area.
#' @param pars optionally specify a named list of all parameters - useful when dealing with theme lists
# @example man-roxygen/ex-theme.R
#' @export
theme_axis <- function(fig,
  which = c("x", "y"),
  num_minor_ticks = 5,
  axis_label_standoff = NULL,
  axis_label_text_align = "left",
  axis_label_text_alpha = 1,
  axis_label_text_baseline = "bottom",
  axis_label_text_color = "#444444",
  axis_label_text_font = "Helvetica",
  axis_label_text_font_size = "12pt",
  axis_label_text_font_style = "normal",
  axis_line_alpha = 1,
  axis_line_cap = "butt",
  axis_line_color = "black",
  axis_line_dash = NULL,
  axis_line_dash_offset = 0,
  axis_line_join = "miter",
  axis_line_width = 1,
  major_label_orientation = "horizontal",
  major_label_standoff = NULL,
  major_label_text_align = "left",
  major_label_text_alpha = 1,
  major_label_text_baseline = "bottom",
  major_label_text_color = "#444444",
  major_label_text_font = "Helvetica",
  major_label_text_font_size = "12pt",
  major_label_text_font_style = "normal",
  major_tick_in = NULL,
  major_tick_line_alpha = 1,
  major_tick_line_cap = "butt",
  major_tick_line_color = "black",
  major_tick_line_dash = NULL,
  major_tick_line_dash_offset = 0,
  major_tick_line_join = "miter",
  major_tick_line_width = 1,
  major_tick_out = NULL,
  minor_tick_in = NULL,
  minor_tick_line_alpha = 1,
  minor_tick_line_cap = "butt",
  minor_tick_line_color = "black",
  minor_tick_line_dash = NULL,
  minor_tick_line_dash_offset = 0,
  minor_tick_line_join = "miter",
  minor_tick_line_width = 1,
  minor_tick_out = NULL
) {
  # this will provide a list of all user-specified arguments
  # (can ignore the defaults for the ones they don't specify
  # because they are defaults if not specified in bokeh)
  args <- get_specified_args(nnms = c("fig", "which"))

  for (whch in which)
    fig$x$theme$axis[[whch]] <- args

  fig
}

#' Override theme parameters for grid attributes
#'
#' @param fig figure to modify
#' @param which which grids to apply attributes to ("x" and/or "y")
#' @param band_fill_alpha The fill alpha of alternating bands between Grid lines.
#' @param band_fill_color The fill color of alternating bands between Grid lines.
#' @param grid_line_alpha The line alpha of the Grid lines.
#' @param grid_line_cap ('butt', 'round', 'square') The line cap of the Grid lines.
#' @param grid_line_color The line color of the Grid lines.
#' @param grid_line_dash The line dash of the Grid lines.
#' @param grid_line_dash_offset The line dash offset of the Grid lines.
#' @param grid_line_join ('miter', 'round', 'bevel') The line join of the Grid lines.
#' @param grid_line_width The line width of the Grid lines.
#' @param minor_grid_line_alpha The line alpha of the minor Grid lines.
#' @param minor_grid_line_cap ('butt', 'round', 'square') The line cap of the minor Grid lines.
#' @param minor_grid_line_color The line color of the minor Grid lines.
#' @param minor_grid_line_dash The line dash of the minor Grid lines.
#' @param minor_grid_line_dash_offset The line dash offset of the minor Grid lines.
#' @param minor_grid_line_join ('miter', 'round', 'bevel') The line join of the minor Grid lines.
#' @param minor_grid_line_width The line width of the minor Grid lines.
#' @param pars optionally specify a named list of all parameters - useful when dealing with theme lists
# @example man-roxygen/ex-theme.R
#' @export
theme_grid <- function(fig,
  which = c("x", "y"),
  band_fill_alpha = 1,
  band_fill_color = "gray",
  grid_line_alpha = 1,
  grid_line_cap = "butt",
  grid_line_color = "black",
  grid_line_dash = NULL,
  grid_line_dash_offset = 0,
  grid_line_join = "miter",
  grid_line_width = 1,
  minor_grid_line_alpha = 1,
  minor_grid_line_cap = "butt",
  minor_grid_line_color = "black",
  minor_grid_line_dash = NULL,
  minor_grid_line_dash_offset = 0,
  minor_grid_line_join = "miter",
  minor_grid_line_width = 1,
  pars = NULL
) {
  # this will provide a list of all user-specified arguments
  # (can ignore the defaults for the ones they don't specify
  # because they are defaults if not specified in bokeh)
  args <- get_specified_args(nnms = c("fig", "which"))

  for (whch in which)
    fig$x$theme$grid[[whch]] <- args

  fig
}

#' Override theme parameters for legend attributes
#'
#' @param fig figure to modify
#' @param background_fill_color (color) background color of plot
#' @param background_fill_alpha (numeric) background color alpha of plot
#' @param border_line_alpha The line alpha for the legend border outline.
#' @param border_line_cap ('butt', 'round', 'square') The line cap for the legend border outline.
#' @param border_line_color The line color for the legend border outline.
#' @param border_line_dash The line dash for the legend border outline.
#' @param border_line_dash_offset The line dash offset for the legend border outline.
#' @param border_line_join ('miter', 'round', 'bevel') The line join for the legend border outline.
#' @param border_line_width The line width for the legend border outline.
#' @param glyph_height The height (in pixels) that the rendered legend glyph should occupy.
#' @param glyph_width The width (in pixels) that the rendered legend glyph should occupy.
#' @param label_height The height (in pixels) of the area that legend labels should occupy.
#' @param label_standoff The distance (in pixels) to separate the label from its associated glyph.
#' @param label_text_align ('left', 'right', 'center') The text align for the legend labels.
#' @param label_text_alpha The text alpha for the legend labels.
#' @param label_text_baseline ('top', 'middle', 'bottom', 'alphabetic', 'hanging') The text baseline for the legend labels.
#' @param label_text_color The text color for the legend labels.
#' @param label_text_font The text font for the legend labels.
#' @param label_text_font_size The text font size for the legend labels.
#' @param label_text_font_style ('normal', 'italic', 'bold') The text font style for the legend labels.
#' @param label_width The width (in pixels) of the area that legend labels should occupy.
#' @param legend_padding Amount of padding around the legend.
#' @param legend_spacing Amount of spacing between legend entries.
#' @param pars optionally specify a named list of all parameters - useful when dealing with theme lists
# @examples
# figure(legend_location = "top_left") %>%
#   ly_points(1:10, legend = "a") %>%
#   theme_legend(border_line_width = 2)
#' @export
theme_legend <- function(fig,
  background_fill_alpha = 0.95,
  background_fill_color = "#fff",
  border_line_alpha = 0.5,
  border_line_cap = "butt",
  border_line_color = "black",
  border_line_dash = NULL,
  border_line_dash_offset = 0,
  border_line_join = "miter",
  border_line_width = 1,
  glyph_height = 20,
  glyph_width = 20,
  label_height = 20,
  label_standoff = 15,
  label_text_align = "left",
  label_text_alpha = 1,
  label_text_baseline = "bottom",
  label_text_color ="#444444",
  label_text_font = "Helvetica",
  label_text_font_size = "12pt",
  label_text_font_style = "normal",
  label_width = 50,
  legend_padding = 10,
  legend_spacing = 3,
  pars = NULL
) {
  # this will provide a list of all user-specified arguments
  # (can ignore the defaults for the ones they don't specify
  # because they are defaults if not specified in bokeh)
  if (is.null(pars)) {
    specified <- names(as.list(match.call())[-1])
    pars <- as.list(environment())[specified]
  }
  # pars <- pars[names(pars) %in% names(legend_par_validator_map)]

  fig$x$theme$legend <- pars

  fig
}

#' Override theme parameters for general plot attributes
#'
#' @param fig figure to modify
#' @param background_fill_color (color) background color of plot
#' @param background_fill_alpha (numeric) background color alpha of plot
#' @param border_fill_color (color) fill color of border area of plot
#' @param border_fill_alpha (numeric) fill color alpha of border area of plot
#' @param outline_line_alpha (numeric) The line alpha for the plot border outline.
#' @param outline_line_cap ('butt', 'round', 'square') The line cap for the plot border outline.
#' @param outline_line_color (color) The line color for the plot border outline.
#' @param outline_line_dash The line dash for the plot border outline.
#' @param outline_line_dash_offset (integer) The line dash offset for the plot border outline.
#' @param outline_line_join ('miter', 'round', 'bevel') The line join for the plot border outline.
#' @param outline_line_width (integer) The line width for the plot border outline.
#' @param min_border (integer) A convenience property to set all all the min_X_border properties to the same value. If an individual border property is explicitly set, it will override min_border.
#' @param min_border_bottom (integer) Minimum size in pixels of the padding region below the bottom of the central plot region. This is a minimum. The padding region may expand as needed to accommodate titles or axes, etc.
#' @param min_border_left (integer) Minimum size in pixels of the padding region to the left of the central plot region. This is a minimum. The padding region may expand as needed to accommodate titles or axes, etc.
#' @param min_border_right (integer) Minimum size in pixels of the padding region to the right of the central plot region. This is a minimum. The padding region may expand as needed to accommodate titles or axes, etc.
#' @param min_border_top (integer) Minimum size in pixels of the padding region above the top of the central plot region. This is a minimum. The padding region may expand as needed to accommodate titles or axes, etc.
#' @param pars optionally specify a named list of all parameters - useful when dealing with theme lists
# @example man-roxygen/ex-theme.R
#' @export
theme_plot <- function(fig,
  background_fill_color = "white",
  background_fill_alpha = 1,
  border_fill_color = "white",
  border_fill_alpha = 1,
  outline_line_alpha = 1,
  outline_line_cap = "butt",
  outline_line_color = "black",
  outline_line_dash = NULL,
  outline_line_dash_offset = 0,
  outline_line_join = "miter",
  outline_line_width = 1,
  min_border = 50,
  min_border_bottom = 50,
  min_border_left = 50,
  min_border_right = 50,
  min_border_top = 50
) {
  # this will provide a list of all user-specified arguments
  # (can ignore the defaults for the ones they don't specify
  # because they are defaults if not specified in bokeh)
  args <- get_specified_args(nnms = "fig")

  fig$x$theme$plot <- args

  fig
}


