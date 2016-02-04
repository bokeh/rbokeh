
#' Add an "annular_wedge" layer to a Bokeh figure
#' @param fig figure to modify
#' @param x values or field name of center x coordinates
#' @param y values or field name of center y coordinates
#' @param data an optional data frame, providing the source for inputs x, y, and other glyph properties
#' @param inner_radius values or field name of inner radii
#' @param outer_radius values or field name of outer radii
#' @param start_angle the angles to start the annular wedges, in radians, as measured from the horizontal
#' @param end_angle the angles to end the annular wedges, in radians, as measured from the horizontal
#' @param direction direction to turn between starting and ending angles ("anticlock", "clock")
#' @template par-coloralpha
#' @template par-hover
#' @template par-url
#' @template par-legend
#' @template par-lnamegroup
#' @template dots-fillline
#' @example man-roxygen/ex-annwedge.R
#' @family layer functions
#' @export
ly_annular_wedge <- function(
  fig, x, y = NULL, data = figure_data(fig),
  inner_radius = 0.1, outer_radius = 0.3,
  start_angle = 0, end_angle = 2*pi, direction = "anticlock",
  color = NULL, alpha = 1,
  hover = NULL, url = NULL, legend = NULL,
  lname = NULL, lgroup = NULL, visible = TRUE, ...
) {

  validate_fig(fig, "ly_annular_wedge")

  args <- sub_names(fig, data,
    grab(
      x,
      y,
      inner_radius,
      outer_radius,
      start_angle,
      end_angle,
      direction,
      color,
      alpha,
      hover,
      url,
      legend,
      lname,
      lgroup,
      visible,
      dots = lazy_dots(...)
    )
  )
  args$params$glyph <- "annular_wedge"

  if(missing(alpha))
    args$params$alpha <- NULL

  args$params <- resolve_color_alpha(args$params, has_line = TRUE, has_fill = TRUE, fig$x$spec$layers[[args$info$lgroup]], theme = fig$x$spec$theme)

  ## see if any options won't be used and give a message
  check_opts(args$params, "annular_wedge", names(formals(ly_annular_wedge)))

  axis_type_range <- get_glyph_axis_type_range(args$data$x, args$data$y, assert_x = "numeric", assert_y = "numeric")

  mc <- lapply(match.call(), deparse)

  make_glyph(fig, type = "annular_wedge", lname = args$info$lname,
    lgroup = args$info$lgroup,
    data = args$data, data_sig = ifelse(is.null(data), NA, digest(data)),
    args = args$params, axis_type_range = axis_type_range,
    hover = args$info$hover, url = args$info$url, legend = args$info$legend,
    xname = args$info$x_name, yname = args$info$y_name, ly_call = mc)
}

#' Add an "annulus" layer to a Bokeh figure
#' @param fig figure to modify
#' @param x values or field name of center x coordinates
#' @param y values or field name of center y coordinates
#' @param data an optional data frame, providing the source for inputs x, y, and other glyph properties
#' @param inner_radius values or field name of inner radii
#' @param outer_radius values or field name of outer radii
#' @template par-coloralpha
#' @template par-hover
#' @template par-url
#' @template par-legend
#' @template par-lnamegroup
#' @template dots-fillline
#' @example man-roxygen/ex-annwedge.R
#' @family layer functions
#' @export
ly_annulus <- function(
  fig, x, y = NULL, data = figure_data(fig),
  inner_radius = 0.1, outer_radius = 0.2,
  color = NULL, alpha = 1,
  hover = NULL, url = NULL, legend = NULL,
  lname = NULL, lgroup = NULL, visible = TRUE,
  ...
) {

  validate_fig(fig, "ly_annulus")

  args <- sub_names(fig, data,
    grab(
      x,
      y,
      inner_radius,
      outer_radius,
      color,
      alpha,
      hover,
      url,
      legend,
      lname,
      lgroup,
      visible,
      dots = lazy_dots(...)
    )
  )
  args$params$glyph <- "annulus"

  if (missing(alpha)) {
    args$params$alpha <- NULL
  }

  args$params <- resolve_color_alpha(args$params, has_line = TRUE, has_fill = TRUE, fig$x$spec$layers[[args$info$lgroup]], theme = fig$x$spec$theme)

  ## see if any options won't be used and give a message
  check_opts(args$params, "annulus", formals = names(formals(ly_annulus)))

  axis_type_range <- get_glyph_axis_type_range(args$data$x, args$data$y, assert_x = "numeric", assert_y = "numeric")

  mc <- lapply(match.call(), deparse)

  make_glyph(fig, type = "annulus", lname = args$info$lname, lgroup = args$info$lgroup,
    data = args$data, data_sig = ifelse(is.null(data), NA, digest(data)),
    args = args$params, axis_type_range = axis_type_range,
    hover = args$info$hover, url = args$info$url, legend = args$info$legend,
    xname = args$info$x_name, yname = args$info$y_name,
    ly_call = mc)
}

#' Add an "arc" layer to a Bokeh figure
#' @param fig figure to modify
#' @param x values or field name of center x coordinates
#' @param y values or field name of center y coordinates
#' @param data an optional data frame, providing the source for inputs x, y, and other glyph properties
#' @template par-lineprops
#' @param radius values or field name of arc radii
#' @param start_angle values or field name of starting angles
#' @param end_angle values or field name of ending angles
#' @param direction direction to turn between starting and ending angles ("anticlock", "clock")
#' @template par-legend
#' @template par-lnamegroup
#' @template dots-line
#' @example man-roxygen/ex-annwedge.R
#' @family layer functions
#' @export
ly_arc <- function(
  fig, x, y = NULL, data = figure_data(fig),
  color = NULL, alpha = 1, width = 2, type = 1,
  radius = 0.2,
  start_angle = 0, end_angle = 2*pi, direction = "anticlock",
  legend = NULL, lname = NULL, lgroup = NULL, visible = TRUE,
  ...
) {

  validate_fig(fig, "ly_arc")

  args <- sub_names(fig, data,
    grab(
      x,
      y,
      color,
      alpha,
      width,
      type,
      radius,
      start_angle,
      end_angle,
      direction,
      # hover,
      # url,
      legend,
      lname,
      lgroup,
      visible,
      dots = lazy_dots(...)
    )
  )
  args$params$glyph <- "arc"
  args$params <- resolve_line_args(fig, args$params)

  ## see if any options won't be used and give a message
  check_opts(args$params, "arc", formals = names(formals(ly_arc)))

  axis_type_range <- get_glyph_axis_type_range(args$data$x, args$data$y, assert_x = "numeric", assert_y = "numeric")

  mc <- lapply(match.call(), deparse)

  make_glyph(
    fig, type = "arc", lname = args$info$lname, lgroup = args$info$lgroup,
    data = args$data, data_sig = ifelse(is.null(data), NA, digest(data)),
    args = args$params, axis_type_range = axis_type_range,
    hover = args$info$hover, url = args$info$url,
    legend = args$info$legend, xname = args$info$x_name,
    yname = args$info$y_name, ly_call = mc)
}

#' Add a "wedge" layer to a Bokeh figure
#' @param fig figure to modify
#' @param x values or field name of center x coordinates
#' @param y values or field name of center y coordinates
#' @param data an optional data frame, providing the source for inputs x, y, and other glyph properties
#' @param radius values or field name of wedge radii
#' @param start_angle the angles to start the wedges, in radians, as measured from the horizontal
#' @param end_angle the angles to end the wedges, in radians, as measured from the horizontal
#' @param direction direction to turn between starting and ending angles ("anticlock", "clock")
#' @template par-coloralpha
#' @template par-hover
#' @template par-url
#' @template par-legend
#' @template par-lnamegroup
#' @template dots-fillline
#' @example man-roxygen/ex-annwedge.R
#' @family layer functions
#' @export
ly_wedge <- function(
  fig, x, y = NULL, data = figure_data(fig),
  radius = 0.3, start_angle = 0, end_angle = 2*pi, direction = "anticlock",
  color = NULL, alpha = 1,
  hover = NULL, url = NULL, legend = NULL,
  lname = NULL, lgroup = NULL, visible = TRUE,
  ...
) {

  validate_fig(fig, "ly_wedge")

  args <- sub_names(fig, data,
    grab(
      x,
      y,
      color,
      alpha,
      radius,
      start_angle,
      end_angle,
      direction,
      hover,
      url,
      legend,
      lname,
      lgroup,
      visible,
      dots = lazy_dots(...)
    )
  )
  args$params$glyph <- "wedge"

  if (missing(alpha)) {
    args$params$alpha <- NULL
  }

  args$params <- resolve_color_alpha(args$params, has_line = TRUE, has_fill = TRUE, fig$x$spec$layers[[args$info$lgroup]], theme = fig$x$spec$theme)

  ## see if any options won't be used and give a message
  check_opts(args$params, "wedge", formals = names(formals(ly_wedge)))

  check_arc_direction(direction)

  axis_type_range <- get_glyph_axis_type_range(args$data$x, args$data$y, assert_x = "numeric", assert_y = "numeric")

  mc <- lapply(match.call(), deparse)

  make_glyph(
    fig, type = "wedge", lname = args$info$lname, lgroup = args$info$lgroup,
    data = args$data, data_sig = ifelse(is.null(data), NA, digest(data)),
    args = args$params, axis_type_range = axis_type_range,
    hover = args$info$hover, url = args$info$url, legend = args$info$legend,
    xname = args$info$x_name, yname = args$info$y_name, ly_call = mc
  )
}
