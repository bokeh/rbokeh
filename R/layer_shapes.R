
#' @export
lay_polygon <- function(fig, x, y, data = NULL, fill_color = NULL, line_color = NULL, fill_alpha = 1, line_alpha = 1, name = NULL, ...) {

  validateFig(fig, "lay_polygon")

  if(!is.null(data)) {
    x          <- getVarData(data, substitute(x))
    y          <- getVarData(data, substitute(y))
    fill_color <- getVarData(data, substitute(fill_color))
    line_color <- getVarData(data, substitute(line_color))
    fill_alpha <- getVarData(data, substitute(fill_alpha))
    line_alpha <- getVarData(data, substitute(line_alpha))
  }

  opts <- c(list(fill_color = fill_color, line_color = line_color, fill_alpha = fill_alpha, line_alpha = line_alpha), list(...))

  nextColor <- getNextColor(fig)

  if(is.null(opts$line_color))
    opts$line_color <- nextColor
  if(is.null(opts$fill_color))
    opts$fill_color <- reduceSaturation(opts$line_color)

  axisTypeRange <- getGlyphAxisTypeRange(x, y)
  makeGlyph(fig, type = "patch", name = name,
    data = list(x = x, y = y), args = opts, axisTypeRange = axisTypeRange)
}

#' @export
lay_polygons <- function(fig, xs, ys, group = NULL, data = NULL, fill_color = NULL, line_color = NULL, fill_alpha = 1, line_alpha = 1, name = NULL, ...) {

  validateFig(fig, "lay_polygons")

  ## xs and ys can be a list
  ## or obtained from a data frame with a group variable
  if(!is.null(data)) {
    xs         <- getVarData(data, substitute(xs))
    ys         <- getVarData(data, substitute(ys))
    group      <- getVarData(data, substitute(group))
    # fill_color <- getVarData(data, substitute(fill_color))
    # line_color <- getVarData(data, substitute(line_color))
    # fill_alpha <- getVarData(data, substitute(fill_alpha))
    # line_alpha <- getVarData(data, substitute(line_alpha))

    xs <- unname(split(xs, group))
    ys <- unname(split(ys, group))
  } else if(!is.null(group)) {
    xs <- unname(split(xs, group))
    ys <- unname(split(ys, group))    
  }
  if(!(is.list(xs) && is.list(ys)))
    stop("For lay_polygon, xs and ys must be lists or specified through a data frame through 'data' argument.")

  opts <- c(list(fill_color = fill_color, line_color = line_color, fill_alpha = fill_alpha, line_alpha = line_alpha), list(...))

  nextColor <- getNextColor(fig)

  if(is.null(opts$line_color) && is.null(opts$fill_color)) {
    opts$line_color <- nextColor
    opts$fill_color <- reduceSaturation(opts$line_color)
  }

  axisTypeRange <- getGlyphAxisTypeRange(unlist(xs), unlist(ys))
  makeGlyph(fig, type = "patches", name = name,
    data = list(xs = unname(xs), ys = unname(ys)), args = opts, axisTypeRange = axisTypeRange)
}

#' @export
lay_rect <- function(fig, xleft, ybottom, xright, ytop, data = NULL, fill_color = NULL, line_color = NULL, fill_alpha = 1, line_alpha = 1, name = NULL, ...) {

  validateFig(fig, "lay_rect")

  if(!is.null(data)) {
    xleft      <- getVarData(data, substitute(xleft))
    ybottom    <- getVarData(data, substitute(ybottom))
    xright     <- getVarData(data, substitute(xright))
    ytop       <- getVarData(data, substitute(ytop))
    fill_color <- getVarData(data, substitute(fill_color))
    line_color <- getVarData(data, substitute(line_color))
    fill_alpha <- getVarData(data, substitute(fill_alpha))
    line_alpha <- getVarData(data, substitute(line_alpha))
  }

  opts <- c(list(fill_color = fill_color, line_color = line_color, fill_alpha = fill_alpha, line_alpha = line_alpha), list(...))

  nextColor <- getNextColor(fig)

  ## defaults if nothing is specified
  if(is.null(opts$line_color) && is.null(opts$fill_color)) {
    opts$line_color <- nextColor
    opts$fill_color <- reduceSaturation(opts$line_color)
  }

  axisTypeRange <- getGlyphAxisTypeRange(c(xleft, xright), c(ybottom, ytop))
  makeGlyph(fig, type = "quad", name = name,
    data = list(left = xleft, right = xright, top = ytop, bottom = ybottom), args = opts, axisTypeRange = axisTypeRange)
}

#' @export
lay_crect <- function(fig, x, y, width = 1, height = 1, data = NULL, fill_color = NULL, line_color = NULL, fill_alpha = 1, line_alpha = 1, angle = 0, name = NULL, ...) {

  validateFig(fig, "lay_crect")

  if(!is.null(data)) {
    x          <- getVarData(data, substitute(x))
    y          <- getVarData(data, substitute(y))
    fill_color <- getVarData(data, substitute(fill_color))
    line_color <- getVarData(data, substitute(line_color))
    fill_alpha <- getVarData(data, substitute(fill_alpha))
    line_alpha <- getVarData(data, substitute(line_alpha))
    angle      <- getVarData(data, substitute(angle))
    width      <- getVarData(data, substitute(width))
    height     <- getVarData(data, substitute(height))
  }
  xy <- getXYData(x, y)

  opts <- c(list(fill_color = fill_color, line_color = line_color, fill_alpha = fill_alpha, line_alpha = line_alpha), list(...))

  nextColor <- getNextColor(fig)

  ## defaults if nothing is specified
  if(is.null(opts$line_color) && is.null(opts$fill_color)) {
    opts$line_color <- nextColor
    opts$fill_color <- reduceSaturation(opts$line_color)
  }

  xr <- xy$x
  if(is.numeric(xy$x)) {
    xr <- c(xy$x - width / 2, xy$x + width / 2)
  }
  yr <- xy$y
  if(is.numeric(xy$y)) {
    yr <- c(xy$y - height / 2, xy$y + height / 2)
  }
  axisTypeRange <- getGlyphAxisTypeRange(xr, yr)
  makeGlyph(fig, type = "rect", name = name,
    data = c(xy, list(width = width, height = height, angle = angle)),
    args = opts, axisTypeRange = axisTypeRange)
}

