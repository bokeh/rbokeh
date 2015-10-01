
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
  fig, x, y = NULL, data = NULL,
  inner_radius = 0.1, outer_radius = 0.3,
  start_angle = 0, end_angle = 2*pi, direction = "anticlock",
  color = NULL, alpha = 1,
  hover = NULL, url = NULL, legend = NULL, lname = NULL, lgroup = NULL, ...
) {

  validate_fig(fig, "ly_annular_wedge")

  glyph = "annular_wedge"
  dots <- substitute(list(...))
  args <- sub_names(fig, data,
    grab(
      sb(x),
      sb(y),
      p_sb(glyph),
      p_sb(inner_radius),
      p_sb(outer_radius),
      p_sb(start_angle),
      p_sb(end_angle),
      p_sb(direction),
      p_sb(color),
      p_sb(alpha),
      sb(hover),
      sb(url),
      sb(legend),
      sb(lname),
      sb(lgroup),
      dots
    )
  )

  # args <- c(args, list(glyph = "annular_wedge", color = color, alpha = alpha,
  #   inner_radius = inner_radius, outer_radius = outer_radius,
  #   start_angle = start_angle, end_angle = end_angle,
  #   direction = direction))
  if(missing(alpha))
    args$params$alpha <- NULL

  args$params <- resolve_color_alpha(args$params, has_line = TRUE, has_fill = TRUE, fig$x$spec$layers[[args$lgroup]], theme = fig$x$spec$theme)

  ## see if any options won't be used and give a message
  check_opts(args$params, "annular_wedge", names(formals(ly_annular_wedge)))

  axis_type_range <- get_glyph_axis_type_range(args$x, args$y, assert_x = "numeric", assert_y = "numeric")

  mc <- lapply(match.call(), deparse)

  make_glyph(fig, type = "annular_wedge", lname = args$lname, lgroup = args$lgroup,
    data = args[c("x", "y")], data_sig = ifelse(is.null(data), NA, digest(data)),
    args = args$params, axis_type_range = axis_type_range,
    hover = args$hover, url = args$url, legend = args$legend,
    xname = args$xName, yname = args$yName, ly_call = mc)
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
  fig, x, y = NULL, data = NULL,
  inner_radius = 0.1, outer_radius = 0.2,
  color = NULL, alpha = 1,
  hover = NULL, url = NULL, legend = NULL,
  lname = NULL, lgroup = NULL, ...
) {

  validate_fig(fig, "ly_annulus")

  glyph = "annulus"
  dots <- substitute(list(...))
  args <- sub_names(fig, data,
    grab(
      sb(x),
      sb(y),
      p_sb(glyph),
      p_sb(inner_radius),
      p_sb(outer_radius),
      p_sb(color),
      p_sb(alpha),
      sb(hover),
      sb(url),
      sb(legend),
      sb(lname),
      sb(lgroup),
      dots
    )
  )

  if(missing(alpha))
    args$params$alpha <- NULL

  args$params <- resolve_color_alpha(args$params, has_line = TRUE, has_fill = TRUE, fig$x$spec$layers[[args$lgroup]], theme = fig$x$spec$theme)

  ## see if any options won't be used and give a message
  check_opts(args$params, "annulus", formals = names(formals(ly_annulus)))

  axis_type_range <- get_glyph_axis_type_range(args$x, args$y, assert_x = "numeric", assert_y = "numeric")

  mc <- lapply(match.call(), deparse)

  make_glyph(fig, type = "annulus", lname = args$lname, lgroup = args$lgroup,
    data = args[c("x","y")], data_sig = ifelse(is.null(data), NA, digest(data)),
    args = args$params, axis_type_range = axis_type_range,
    hover = args$hover, url = args$url, legend = args$legend,
    xname = args$xName, yname = args$yName, ly_call = mc)
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
  fig, x, y = NULL, data = NULL,
  color = NULL, alpha = 1, width = 2, type = 1,
  radius = 0.2,
  start_angle = 0, end_angle = 2*pi, direction = "anticlock",
  legend = NULL, lname = NULL, lgroup = NULL, ...
) {

  validate_fig(fig, "ly_arc")

  glyph = "arc"
  dots <- substitute(list(...))
  args <- sub_names(fig, data,
    grab(
      sb(x),
      sb(y),
      p_sb(glyph),
      p_sb(color),
      p_sb(alpha),
      p_sb(width),
      p_sb(type),
      p_sb(radius),
      p_sb(start_angle),
      p_sb(end_angle),
      p_sb(direction),
      # sb(hover), # no hover?
      sb(url),
      sb(legend),
      sb(lname),
      sb(lgroup),
      dots
    )
  )

  args$params <- resolve_line_args(fig, args$params)

  ## see if any options won't be used and give a message
  check_opts(args$params, "arc", formals = names(formals(ly_arc)))

  axis_type_range <- get_glyph_axis_type_range(args$x, args$y, assert_x = "numeric", assert_y = "numeric")

  mc <- lapply(match.call(), deparse)

  make_glyph(
    fig, type = "arc", lname = args$lname, lgroup = args$lgroup,
    data = args[c("x", "y")], data_sig = ifelse(is.null(data), NA, digest(data)),
    args = args$params, axis_type_range = axis_type_range,
    legend = args$legend, xname = args$xName, yname = args$yName,
    ly_call = mc
  )
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
ly_wedge <- function(fig, x, y = NULL, data = NULL, radius = 0.3,
  start_angle = 0, end_angle = 2*pi, direction = "anticlock",
  color = NULL, alpha = 1,
  hover = NULL, url = NULL, legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validate_fig(fig, "ly_wedge")

  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    dots        <- substitute(list(...))[-1]
    args        <- lapply(dots, function(x) v_eval(x, data))
    x           <- v_eval(substitute(x), data)
    y           <- v_eval(substitute(y), data)
    color       <- v_eval(substitute(color), data)
    radius      <- v_eval(substitute(radius), data)
    start_angle <- v_eval(substitute(start_angle), data)
    end_angle   <- v_eval(substitute(end_angle), data)
  } else {
    args <- list(...)
  }

  hover <- get_hover(substitute(hover), data, parent.frame())
  url <- get_url(url, data)
  xy_names <- get_xy_names(x, y, xname, yname, args)
  ## translate different x, y types to vectors
  xy <- get_xy_data(x, y)
  lgroup <- get_lgroup(lgroup, fig)

  args <- c(args, list(glyph = "wedge", color = color, alpha = alpha,
    radius = radius, start_angle = start_angle, end_angle = end_angle,
    direction = direction, radius = radius, start_angle = start_angle,
    end_angle = end_angle, direction = direction))
  if(missing(alpha))
    args$alpha <- NULL

  args <- resolve_color_alpha(args, has_line = TRUE, has_fill = TRUE, fig$x$spec$layers[[lgroup]], theme = fig$x$spec$theme)

  ## see if any options won't be used and give a message
  check_opts(args, "wedge", formals = names(formals(ly_wedge)))

  check_arc_direction(direction)

  axis_type_range <- get_glyph_axis_type_range(x, y, assert_x = "numeric", assert_y = "numeric")

  mc <- lapply(match.call(), deparse)

  make_glyph(fig, type = "wedge", lname = lname, lgroup = lgroup,
    data = xy, data_sig = ifelse(is.null(data), NA, digest(data)),
    args = args, axis_type_range = axis_type_range,
    hover = hover, url = url, legend = legend,
    xname = xy_names$x, yname = xy_names$y, ly_call = mc)
}
