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
#' @example man-roxygen/ex-theme.R
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
  minor_tick_out = NULL,
  pars = NULL
) {
  # this will provide a list of all user-specified arguments
  # (can ignore the defaults for the ones they don't specify
  # because they are defaults if not specified in bokeh)
  if(is.null(pars)) {
    specified <- names(as.list(match.call())[-1])
    pars <- as.list(environment())[specified]
  }
  pars <- pars[names(pars) %in% names(axis_par_validator_map)]

  pars <- handle_extra_pars(pars, axis_par_validator_map)
  parnames <- names(pars)

  ## if an axis hasn't been created yet (usually done in prepare_figure)
  ## then create it here and apply attributes
  ## could alternatively save attributes and apply in prepare_figure
  if("x" %in% which && fig$x$spec$xaxes != FALSE) {
    if(is.null(fig$x$spec$model[["x_axis"]]))
      fig <- fig %>% x_axis()
    for(nm in parnames)
      fig$x$spec$model[["x_axis"]]$attributes[[nm]] <- pars[[nm]]
    if(!is.null(pars$num_minor_ticks))
      fig$x$spec$model$x_tickformatter$attributes$num_minor_ticks <- pars$num_minor_ticks
  }
  if("y" %in% which && fig$x$spec$yaxes != FALSE) {
    if(is.null(fig$x$spec$model[["y_axis"]]))
      fig <- fig %>% y_axis()
    for(nm in parnames)
      fig$x$spec$model[["y_axis"]]$attributes[[nm]] <- pars[[nm]]
    if(!is.null(pars$num_minor_ticks))
      fig$x$spec$model$y_tickformatter$attributes$num_minor_ticks <- pars$num_minor_ticks
  }

  fig
}


