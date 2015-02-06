## "second-class" layer functions
## main purpose of these is to provide a direct interface to bokeh glyphs
## but where either there is a more natural way to do it with another glyph
## or there is not a natural way to have a "data" argument, etc.
## or it is not planned to be commonly used

#' @export
ly_multi_line <- function(fig, xs, ys, lname = NULL, lgroup = NULL, ...) {

  validate_fig(fig, "ly_multi_line")
  ## see if any options won't be used and give a message
  check_opts(list(...), "multi_line")

  axis_type_range <- get_glyph_axis_type_range(unlist(xs), unlist(ys))
  make_glyph(fig, type = "multi_line", lname = lname, lgroup = lgroup,
    data = list(xs = xs, ys = ys), args = list(...), axis_type_range = axis_type_range)
}

#' @export
ly_bezier <- function(fig, x0, y0, x1, y1, cx0, cy0, cx1, cy1, data = NULL, hover = NULL, legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validate_fig(fig, "ly_bezier")
  ## see if any options won't be used and give a message
  check_opts(list(...), "bezier")

  axis_type_range <- get_glyph_axis_type_range(c(x0, x1), c(y0, y1), assert_x = "numeric", assert_y = "numeric")
  make_glyph(fig, type = "bezier", lname = lname, lgroup = lgroup,
    data = list(x0 = x0, y0 = y0, x1 = x1, y1 = y1, cx0 = cx0, cy0 = cy0, cx1 = cx1, cy1 = cy1),
    args = list(...), axis_type_range = axis_type_range)
}


#' @export
ly_quadratic <- function(fig, x0, y0, x1, y1, cx, cy, data = NULL, hover = NULL, legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validate_fig(fig, "ly_quadratic")
  ## see if any options won't be used and give a message
  check_opts(list(...), "quadratic")

  axis_type_range <- get_glyph_axis_type_range(c(x0, x1), c(y0, y1), assert_x = "numeric", assert_y = "numeric")

  make_glyph(fig, type = "quadratic", lname = lname, lgroup = lgroup,
    data = list(x0 = x0, y0 = y0, x1 = x1, y1 = y1, cx = cx, cy = cy),
    args = list(...), axis_type_range = axis_type_range)
}

#' @export
ly_patch <- function(fig, x, y, data = NULL,
  color = NULL, alpha = 1,
  hover = NULL, legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validate_fig(fig, "ly_patch")

  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    dots  <- substitute(list(...))[-1]
    args  <- lapply(dots, function(x) v_eval(x, data))
    x     <- v_eval(substitute(x), data)
    y     <- v_eval(substitute(y), data)
    color <- v_eval(substitute(color), data)
    alpha <- v_eval(substitute(alpha), data)
  } else {
    args <- list(...)
  }

  hover <- get_hover(substitute(hover), data)
  xy_names <- get_xy_names(x, y, xname, yname, args)
  ## translate different x, y types to vectors
  xy <- get_xy_data(x, y)
  lgroup <- get_lgroup(lgroup, fig)

  args <- c(args, list(color = color, alpha = alpha))

  args <- resolve_color_alpha(args, has_line = TRUE, has_fill = TRUE, fig$layers[[lgroup]])

  axis_type_range <- get_glyph_axis_type_range(x, y)

  make_glyph(fig, type = "patch", data = xy, args = args,
    legend = legend, hover = hover,
    lname = lname, lgroup = lgroup,
    axis_type_range = axis_type_range)
}
