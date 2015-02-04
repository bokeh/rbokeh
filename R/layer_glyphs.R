
# x The x-coordinates of the center of the annular wedges.
# y The y-coordinates of the center of the annular wedges.
# direction ("clock", "anticlock") Which direction to stroke between the start and end angles.
# end_angle The angles to end the annular wedges, in radians, as measured from the horizontal.
# fill_alpha The fill alpha values for the annular wedges.
# fill_color The fill color values for the annular wedges.
# inner_radius The inner radii of the annular wedges.
# line_alpha The line alpha values for the annular wedges.
# line_color The line color values for the annular wedges.
# outer_radius The outer radii of the annular wedges.
# start_angle The angles to start the annular wedges, in radians, as measured from the horizontal.
# addiional parameters \code{line_cap}, \code{line_dash}, \code{line_dash_offset}, \code{line_join}, \code{line_width}

#' @export
ly_annular_wedge <- function(fig, x, y = NULL, data = NULL,
  inner_radius = 0.1, outer_radius = 0.3,
  start_angle = 0, end_angle = 2*pi, direction = "anticlock",
  color = NULL, alpha = NULL, fill_color = NULL, fill_alpha = 0.75,
  line_color = NULL, line_alpha = NULL,
  hover = NULL, legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validateFig(fig, "ly_annular_wedge")
  ## see if any options won't be used and give a message
  checkOpts(list(...), "annular_wedge")

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

  hover <- getHover(substitute(hover), data)
  xyNames <- getXYNames(x, y, xname, yname, list(...))
  ## translate different x, y types to vectors
  xy <- getXYData(x, y)
  lgroup <- getLgroup(lgroup, fig)

  args <- list(glyph = "annular_wedge", color = color, alpha = alpha,
    fill_color = fill_color, fill_alpha = fill_alpha,
    line_color = line_color, line_alpha = line_alpha,
    inner_radius = inner_radius, outer_radius = outer_radius,
    start_angle = start_angle, end_angle = end_angle,
    direction = direction, ...)

  args <- resolveColorAlpha(args, hasLine = TRUE, hasFill = TRUE, fig$layers[[lgroup]])

  checkArcDirection(direction)

  axisTypeRange <- getGlyphAxisTypeRange(x, y, assertX = "numeric", assertY = "numeric")

  make_glyph(fig, type = "annular_wedge", lname = lname, lgroup = lgroup,
    data = xy, dataSig = ifelse(is.null(data), NULL, digest(data)),
    args = args, axisTypeRange = axisTypeRange,
    hover = hover, legend = legend,
    xname = xyNames$x, yname = xyNames$y)
}

#' @export
ly_annulus <- function(fig, x, y = NULL, data = NULL,
  inner_radius = 0.1, outer_radius = 0.2,
  color = NULL, alpha = NULL,
  line_color = NULL, line_alpha = NULL,
  fill_color = NULL, fill_alpha = 0.75,
  hover = NULL, legend = NULL,
  lname = NULL, lgroup = NULL, ...) {

  validateFig(fig, "ly_annulus")
  ## see if any options won't be used and give a message
  checkOpts(list(...), "annulus")

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

  hover <- getHover(substitute(hover), data)
  xyNames <- getXYNames(x, y, xname, yname, list(...))
  ## translate different x, y types to vectors
  xy <- getXYData(x, y)
  lgroup <- getLgroup(lgroup, fig)

  args <- list(glyph = "annulus", color = color, alpha = alpha,
    fill_color = fill_color, fill_alpha = fill_alpha,
    line_color = line_color, line_alpha = line_alpha,
    inner_radius = inner_radius, outer_radius = outer_radius, ...)

  args <- resolveColorAlpha(args, hasLine = TRUE, hasFill = TRUE, fig$layers[[lgroup]])

  axisTypeRange <- getGlyphAxisTypeRange(x, y, assertX = "numeric", assertY = "numeric")

  make_glyph(fig, type = "annulus", lname = lname, lgroup = lgroup,
    data = xy, dataSig = ifelse(is.null(data), NULL, digest(data)),
    args = args, axisTypeRange = axisTypeRange,
    hover = hover, legend = legend,
    xname = xyNames$x, yname = xyNames$y)
}

# doesn't seem to have hover...
#' @export
ly_arc <- function(fig, x, y = NULL, data = NULL,
  color = NULL, alpha = NULL, line_width = 2,
  radius = 0.2,
  start_angle = 0, end_angle = 2*pi, direction = "anticlock",
  hover = NULL, legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validateFig(fig, "ly_arc")
  ## see if any options won't be used and give a message
  checkOpts(list(...), "arc")

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

  hover <- getHover(substitute(hover), data)
  xyNames <- getXYNames(x, y, xname, yname, list(...))
  ## translate different x, y types to vectors
  xy <- getXYData(x, y)
  lgroup <- getLgroup(lgroup, fig)

  args <- list(glyph = "arc", color = color, alpha = alpha,
    radius = radius, start_angle = start_angle, end_angle = end_angle,
    direction = direction,
    line_width = line_width, start_angle = start_angle,
    end_angle = end_angle, ...)

  args <- resolveColorAlpha(args, hasLine = TRUE, hasFill = FALSE, fig$layers[[lgroup]])

  checkArcDirection(direction)

  axisTypeRange <- getGlyphAxisTypeRange(x, y, assertX = "numeric", assertY = "numeric")

  make_glyph(fig, type = "arc", lname = lname, lgroup = lgroup,
    data = xy, dataSig = ifelse(is.null(data), NULL, digest(data)),
    args = args, axisTypeRange = axisTypeRange,
    hover = hover, legend = legend,
    xname = xyNames$x, yname = xyNames$y)
}

# doesn't seem to have hover...
#' @export
ly_oval <- function(fig, x, y = NULL, data = NULL,
  width = 0.1, height = 0.1, angle = 0,
  color = NULL, alpha = NULL, fill_color = NULL, fill_alpha = 0.75,
  line_color = NULL, line_alpha = NULL,
  hover = NULL, legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validateFig(fig, "ly_oval")
  ## see if any options won't be used and give a message
  checkOpts(list(...), "oval")

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

  hover <- getHover(substitute(hover), data)
  xyNames <- getXYNames(x, y, xname, yname, list(...))
  ## translate different x, y types to vectors
  xy <- getXYData(x, y)
  lgroup <- getLgroup(lgroup, fig)

  args <- list(glyph = "oval", color = color, alpha = alpha,
    width = width, height = height, angle = angle,
    fill_color = fill_color, fill_alpha = fill_alpha,
    line_color = line_color, line_alpha = line_alpha, ...)

  args <- resolveColorAlpha(args, hasLine = TRUE, hasFill = TRUE, fig$layers[[lgroup]])

  axisTypeRange <- getGlyphAxisTypeRange(x, y)

  make_glyph(fig, type = "oval", lname = lname, lgroup = lgroup,
    data = xy, dataSig = ifelse(is.null(data), NULL, digest(data)),
    args = args, axisTypeRange = axisTypeRange,
    hover = hover, legend = legend,
    xname = xyNames$x, yname = xyNames$y)
}

#' @export
ly_wedge <- function(fig, x, y = NULL, data = NULL, radius = 0.3,
  start_angle = 0, end_angle = 2*pi, direction = "anticlock",
  color = NULL, alpha = NULL, fill_color = NULL, fill_alpha = 0.75,
  line_color = NULL, line_alpha = NULL,
  hover = NULL, legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validateFig(fig, "ly_wedge")
  ## see if any options won't be used and give a message
  checkOpts(list(...), "wedge")

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

  hover <- getHover(substitute(hover), data)
  xyNames <- getXYNames(x, y, xname, yname, list(...))
  ## translate different x, y types to vectors
  xy <- getXYData(x, y)
  lgroup <- getLgroup(lgroup, fig)

  args <- list(glyph = "wedge", color = color, alpha = alpha,
    radius = radius, start_angle = start_angle, end_angle = end_angle,
    direction = direction,
    fill_color = fill_color, fill_alpha = fill_alpha, line_color = line_color,
    line_alpha = line_alpha, radius = radius, start_angle = start_angle,
    end_angle = end_angle, direction = direction, ...)

  args <- resolveColorAlpha(args, hasLine = TRUE, hasFill = TRUE, fig$layers[[lgroup]])

  checkArcDirection(direction)

  axisTypeRange <- getGlyphAxisTypeRange(x, y, assertX = "numeric", assertY = "numeric")

  make_glyph(fig, type = "wedge", lname = lname, lgroup = lgroup,
    data = xy, dataSig = ifelse(is.null(data), NULL, digest(data)),
    args = args, axisTypeRange = axisTypeRange,
    hover = hover, legend = legend,
    xname = xyNames$x, yname = xyNames$y)
}


