## "second-class" layer functions
## main purpose of these is to provide a direct interface to bokeh glyphs
## but where either there is a more natural way to do it with another glyph
## or there is not a natural way to have a "data" argument, etc.
## or it is not planned to be commonly used

#' @export
ly_multi_line <- function(fig, xs, ys, lname = NULL, lgroup = NULL, ...) {

  validateFig(fig, "ly_multi_line")
  ## see if any options won't be used and give a message
  checkOpts(list(...), "multi_line")

  axisTypeRange <- getGlyphAxisTypeRange(unlist(xs), unlist(ys))
  make_glyph(fig, type = "multi_line", lname = lname, lgroup = lgroup,
    data = list(xs = xs, ys = ys), args = list(...), axisTypeRange = axisTypeRange)
}

#' @export
ly_bezier <- function(fig, x0, y0, x1, y1, cx0, cy0, cx1, cy1, data = NULL, hover = NULL, legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validateFig(fig, "ly_bezier")
  ## see if any options won't be used and give a message
  checkOpts(list(...), "bezier")

  axisTypeRange <- getGlyphAxisTypeRange(c(x0, x1), c(y0, y1), assertX = "numeric", assertY = "numeric")
  make_glyph(fig, type = "bezier", lname = lname, lgroup = lgroup,
    data = list(x0 = x0, y0 = y0, x1 = x1, y1 = y1, cx0 = cx0, cy0 = cy0, cx1 = cx1, cy1 = cy1),
    args = list(...), axisTypeRange = axisTypeRange)
}


#' @export
ly_quadratic <- function(fig, x0, y0, x1, y1, cx, cy, data = NULL, hover = NULL, legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validateFig(fig, "ly_quadratic")
  ## see if any options won't be used and give a message
  checkOpts(list(...), "quadratic")

  axisTypeRange <- getGlyphAxisTypeRange(c(x0, x1), c(y0, y1), assertX = "numeric", assertY = "numeric")

  make_glyph(fig, type = "quadratic", lname = lname, lgroup = lgroup,
    data = list(x0 = x0, y0 = y0, x1 = x1, y1 = y1, cx = cx, cy = cy),
    args = list(...), axisTypeRange = axisTypeRange)
}

#' @export
ly_patch <- function(fig, x, y, data = NULL,
  color = NULL, alpha = NULL,
  fill_color = NULL, line_color = NULL, fill_alpha = 1, line_alpha = 1,
  hover = NULL, legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validateFig(fig, "ly_patch")

  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    x          <- v_eval(substitute(x), data)
    y          <- v_eval(substitute(y), data)
    color      <- v_eval(substitute(color), data)
    alpha      <- v_eval(substitute(alpha), data)
    line_color <- v_eval(substitute(line_color), data)
    fill_color <- v_eval(substitute(fill_color), data)
  }

  hover <- getHover(substitute(hover), data)
  xyNames <- getXYNames(x, y, xname, yname, list(...))
  ## translate different x, y types to vectors
  xy <- getXYData(x, y)
  lgroup <- getLgroup(lgroup, fig)

  args <- list(color = color, alpha = alpha,
    fill_color = fill_color, fill_alpha = fill_alpha,
    line_color = line_color, line_alpha = line_alpha, ...)

  args <- resolveColorAlpha(args, hasLine = TRUE, hasFill = TRUE, fig$layers[[lgroup]])

  axisTypeRange <- getGlyphAxisTypeRange(x, y)

  make_glyph(fig, type = "patch", data = xy, args = args,
    legend = legend, hover = hover,
    lname = lname, lgroup = lgroup,
    axisTypeRange = axisTypeRange)
}
