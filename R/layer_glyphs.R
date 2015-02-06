
#' Add an "annular_wedge" layer to a Bokeh figure
#' @param x the x-coordinates of the centers of the annular wedges
#' @param y the y-coordinates of the centers of the annular wedges
#' @param data an optional data frame, providing the source for inputs x, y, and other glyph properties
#' @param inner_radius the inner radii of the annular wedges
#' @param outer_radius The outer radii of the annular wedges
#' @param start_angle the angles to start the annular wedges, in radians, as measured from the horizontal
#' @param end_angle the angles to end the annular wedges, in radians, as measured from the horizontal
#' @param direction which direction to stroke between the start and end angles ("clock", "anticlock")
#' @template par-coloralpha
#' @template par-fill
#' @template par-line
#' @template par-hover
#' @template par-legend
#' @template par-lnamegroup
#' @param \ldots additional parameters
#' @family layer functions
#' @export
ly_annular_wedge <- function(fig, x, y = NULL, data = NULL,
  inner_radius = 0.1, outer_radius = 0.3,
  start_angle = 0, end_angle = 2*pi, direction = "anticlock",
  color = NULL, alpha = NULL,
  fill_color = NULL, fill_alpha = 0.75,
  line_color = NULL, line_width = 1, line_alpha = NULL,
  hover = NULL, legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validate_fig(fig, "ly_annular_wedge")
  ## see if any options won't be used and give a message
  check_opts(list(...), "annular_wedge")

  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    x          <- v_eval(substitute(x), data)
    y          <- v_eval(substitute(y), data)
    color      <- v_eval(substitute(color), data)
    fill_color <- v_eval(substitute(fill_color), data)
    line_color <- v_eval(substitute(line_color), data)
    inner_radius <- v_eval(substitute(inner_radius), data)
    outer_radius <- v_eval(substitute(outer_radius), data)
    start_angle <- v_eval(substitute(start_angle), data)
    end_angle <- v_eval(substitute(end_angle), data)
  }

  hover <- get_hover(substitute(hover), data)
  xy_names <- get_xy_names(x, y, xname, yname, list(...))
  ## translate different x, y types to vectors
  xy <- get_xy_data(x, y)
  lgroup <- get_lgroup(lgroup, fig)

  args <- list(glyph = "annular_wedge", color = color, alpha = alpha,
    fill_color = fill_color, fill_alpha = fill_alpha,
    line_color = line_color, line_width = line_width, line_alpha = line_alpha,
    inner_radius = inner_radius, outer_radius = outer_radius,
    start_angle = start_angle, end_angle = end_angle,
    direction = direction, ...)

  args <- resolve_color_alpha(args, has_line = TRUE, has_fill = TRUE, fig$layers[[lgroup]])

  check_arc_direction(direction)

  axis_type_range <- get_glyph_axis_type_range(x, y, assert_x = "numeric", assert_y = "numeric")

  make_glyph(fig, type = "annular_wedge", lname = lname, lgroup = lgroup,
    data = xy, data_sig = ifelse(is.null(data), NA, digest(data)),
    args = args, axis_type_range = axis_type_range,
    hover = hover, legend = legend,
    xname = xy_names$x, yname = xy_names$y)
}

#' @export
ly_annulus <- function(fig, x, y = NULL, data = NULL,
  inner_radius = 0.1, outer_radius = 0.2,
  color = NULL, alpha = NULL,
  line_color = NULL, line_alpha = NULL,
  fill_color = NULL, fill_alpha = 0.75,
  hover = NULL, legend = NULL,
  lname = NULL, lgroup = NULL, ...) {

  validate_fig(fig, "ly_annulus")
  ## see if any options won't be used and give a message
  check_opts(list(...), "annulus")

  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    x            <- v_eval(substitute(x), data)
    y            <- v_eval(substitute(y), data)
    color        <- v_eval(substitute(color), data)
    line_color   <- v_eval(substitute(line_color), data)
    fill_color   <- v_eval(substitute(fill_color), data)
    inner_radius <- v_eval(substitute(inner_radius), data)
    outer_radius <- v_eval(substitute(outer_radius), data)
  }

  hover <- get_hover(substitute(hover), data)
  xy_names <- get_xy_names(x, y, xname, yname, list(...))
  ## translate different x, y types to vectors
  xy <- get_xy_data(x, y)
  lgroup <- get_lgroup(lgroup, fig)

  args <- list(glyph = "annulus", color = color, alpha = alpha,
    fill_color = fill_color, fill_alpha = fill_alpha,
    line_color = line_color, line_alpha = line_alpha,
    inner_radius = inner_radius, outer_radius = outer_radius, ...)

  args <- resolve_color_alpha(args, has_line = TRUE, has_fill = TRUE, fig$layers[[lgroup]])

  axis_type_range <- get_glyph_axis_type_range(x, y, assert_x = "numeric", assert_y = "numeric")

  make_glyph(fig, type = "annulus", lname = lname, lgroup = lgroup,
    data = xy, data_sig = ifelse(is.null(data), NA, digest(data)),
    args = args, axis_type_range = axis_type_range,
    hover = hover, legend = legend,
    xname = xy_names$x, yname = xy_names$y)
}

# doesn't seem to have hover...
#' @export
ly_arc <- function(fig, x, y = NULL, data = NULL,
  color = NULL, alpha = NULL, line_width = 2,
  radius = 0.2,
  start_angle = 0, end_angle = 2*pi, direction = "anticlock",
  hover = NULL, legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validate_fig(fig, "ly_arc")
  ## see if any options won't be used and give a message
  check_opts(list(...), "arc")

  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    x           <- v_eval(substitute(x), data)
    y           <- v_eval(substitute(y), data)
    color       <- v_eval(substitute(color), data)
    radius      <- v_eval(substitute(radius), data)
    start_angle <- v_eval(substitute(start_angle), data)
    end_angle   <- v_eval(substitute(end_angle), data)
    line_width  <- v_eval(substitute(line_width), data)
  }

  hover <- get_hover(substitute(hover), data)
  xy_names <- get_xy_names(x, y, xname, yname, list(...))
  ## translate different x, y types to vectors
  xy <- get_xy_data(x, y)
  lgroup <- get_lgroup(lgroup, fig)

  args <- list(glyph = "arc", color = color, alpha = alpha,
    radius = radius, start_angle = start_angle, end_angle = end_angle,
    direction = direction,
    line_width = line_width, start_angle = start_angle,
    end_angle = end_angle, ...)

  args <- resolve_color_alpha(args, has_line = TRUE, has_fill = FALSE, fig$layers[[lgroup]])

  check_arc_direction(direction)

  axis_type_range <- get_glyph_axis_type_range(x, y, assert_x = "numeric", assert_y = "numeric")

  make_glyph(fig, type = "arc", lname = lname, lgroup = lgroup,
    data = xy, data_sig = ifelse(is.null(data), NA, digest(data)),
    args = args, axis_type_range = axis_type_range,
    hover = hover, legend = legend,
    xname = xy_names$x, yname = xy_names$y)
}

# doesn't seem to have hover...
#' @export
ly_oval <- function(fig, x, y = NULL, data = NULL,
  width = 0.1, height = 0.1, angle = 0,
  color = NULL, alpha = NULL, fill_color = NULL, fill_alpha = 0.75,
  line_color = NULL, line_alpha = NULL,
  hover = NULL, legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validate_fig(fig, "ly_oval")
  ## see if any options won't be used and give a message
  check_opts(list(...), "oval")

  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    x          <- v_eval(substitute(x), data)
    y          <- v_eval(substitute(y), data)
    color      <- v_eval(substitute(color), data)
    fill_color <- v_eval(substitute(fill_color), data)
    line_color <- v_eval(substitute(line_color), data)
    width      <- v_eval(substitute(width), data)
    height     <- v_eval(substitute(height), data)
    angle      <- v_eval(substitute(angle), data)
  }

  hover <- get_hover(substitute(hover), data)
  xy_names <- get_xy_names(x, y, xname, yname, list(...))
  ## translate different x, y types to vectors
  xy <- get_xy_data(x, y)
  lgroup <- get_lgroup(lgroup, fig)

  args <- list(glyph = "oval", color = color, alpha = alpha,
    width = width, height = height, angle = angle,
    fill_color = fill_color, fill_alpha = fill_alpha,
    line_color = line_color, line_alpha = line_alpha, ...)

  args <- resolve_color_alpha(args, has_line = TRUE, has_fill = TRUE, fig$layers[[lgroup]])

  axis_type_range <- get_glyph_axis_type_range(x, y)

  make_glyph(fig, type = "oval", lname = lname, lgroup = lgroup,
    data = xy, data_sig = ifelse(is.null(data), NA, digest(data)),
    args = args, axis_type_range = axis_type_range,
    hover = hover, legend = legend,
    xname = xy_names$x, yname = xy_names$y)
}

#' @export
ly_wedge <- function(fig, x, y = NULL, data = NULL, radius = 0.3,
  start_angle = 0, end_angle = 2*pi, direction = "anticlock",
  color = NULL, alpha = NULL, fill_color = NULL, fill_alpha = 0.75,
  line_color = NULL, line_alpha = NULL,
  hover = NULL, legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validate_fig(fig, "ly_wedge")
  ## see if any options won't be used and give a message
  check_opts(list(...), "wedge")

  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    x           <- v_eval(substitute(x), data)
    y           <- v_eval(substitute(y), data)
    color       <- v_eval(substitute(color), data)
    fill_color  <- v_eval(substitute(fill_color), data)
    line_color  <- v_eval(substitute(line_color), data)
    radius      <- v_eval(substitute(radius), data)
    start_angle <- v_eval(substitute(start_angle), data)
    end_angle   <- v_eval(substitute(end_angle), data)
  }

  hover <- get_hover(substitute(hover), data)
  xy_names <- get_xy_names(x, y, xname, yname, list(...))
  ## translate different x, y types to vectors
  xy <- get_xy_data(x, y)
  lgroup <- get_lgroup(lgroup, fig)

  args <- list(glyph = "wedge", color = color, alpha = alpha,
    radius = radius, start_angle = start_angle, end_angle = end_angle,
    direction = direction,
    fill_color = fill_color, fill_alpha = fill_alpha, line_color = line_color,
    line_alpha = line_alpha, radius = radius, start_angle = start_angle,
    end_angle = end_angle, direction = direction, ...)

  args <- resolve_color_alpha(args, has_line = TRUE, has_fill = TRUE, fig$layers[[lgroup]])

  check_arc_direction(direction)

  axis_type_range <- get_glyph_axis_type_range(x, y, assert_x = "numeric", assert_y = "numeric")

  make_glyph(fig, type = "wedge", lname = lname, lgroup = lgroup,
    data = xy, data_sig = ifelse(is.null(data), NA, digest(data)),
    args = args, axis_type_range = axis_type_range,
    hover = hover, legend = legend,
    xname = xy_names$x, yname = xy_names$y)
}


