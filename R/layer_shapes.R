
#' @export
lay_polygon <- function(fig, x, y, data = NULL, fill_color = NULL, line_color = NULL, fill_alpha = 1, line_alpha = 1, hover = NULL, legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validateFig(fig, "lay_polygon")

  ##### boilerplate
  validateFig(fig, "lay_points")

  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    x          <- v_eval(substitute(x), data)
    y          <- v_eval(substitute(y), data)
    size       <- v_eval(substitute(size), data)
    glyph      <- v_eval(substitute(glyph), data)
    color      <- v_eval(substitute(color), data)
    line_color <- v_eval(substitute(line_color), data)
    fill_color <- v_eval(substitute(fill_color), data)
  }

  hover <- getHover(substitute(hover), data)
  xyNames <- getXYNames(x, y, xname, yname, list(...))
  ## translate different x, y types to vectors
  xy <- getXYData(x, y)
  lgroup <- getLgroup(lgroup, fig)
  ##### boilerplate

  if(!is.null(data)) {
    x          <- eval(substitute(x), data)
    y          <- eval(substitute(y), data)
    fill_color <- eval(substitute(fill_color), data)
    line_color <- eval(substitute(line_color), data)
    fill_alpha <- eval(substitute(fill_alpha), data)
    line_alpha <- eval(substitute(line_alpha), data)
  }

  opts <- c(list(fill_color = fill_color, line_color = line_color, fill_alpha = fill_alpha, line_alpha = line_alpha), list(...))

  nextColor <- getNextColor(fig)

  if(is.null(opts$line_color))
    opts$line_color <- nextColor
  if(is.null(opts$fill_color))
    opts$fill_color <- reduceSaturation(opts$line_color)

  axisTypeRange <- getGlyphAxisTypeRange(x, y)
  make_glyph(fig, type = "patch", lname = lname, lgroup = lgroup,
    data = list(x = x, y = y), args = opts, axisTypeRange = axisTypeRange)
}

#' @export
lay_polygons <- function(fig, xs, ys, group = NULL, data = NULL, fill_color = NULL, line_color = NULL, fill_alpha = 1, line_alpha = 1, hover = NULL, legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validateFig(fig, "lay_polygons")

  ##### boilerplate
  validateFig(fig, "lay_points")

  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    x          <- v_eval(substitute(x), data)
    y          <- v_eval(substitute(y), data)
    size       <- v_eval(substitute(size), data)
    glyph      <- v_eval(substitute(glyph), data)
    color      <- v_eval(substitute(color), data)
    line_color <- v_eval(substitute(line_color), data)
    fill_color <- v_eval(substitute(fill_color), data)
  }

  hover <- getHover(substitute(hover), data)
  xyNames <- getXYNames(x, y, xname, yname, list(...))
  ## translate different x, y types to vectors
  xy <- getXYData(x, y)
  lgroup <- getLgroup(lgroup, fig)
  ##### boilerplate

  ## xs and ys can be a list
  ## or obtained from a data frame with a group variable
  if(!is.null(data)) {
    xs         <- eval(substitute(xs), data)
    ys         <- eval(substitute(ys), data)
    group      <- eval(substitute(group), data)
    # fill_color <- eval(substitute(fill_color), data)
    # line_color <- eval(substitute(line_color), data)
    # fill_alpha <- eval(substitute(fill_alpha), data)
    # line_alpha <- eval(substitute(line_alpha), data)

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
  make_glyph(fig, type = "patches", lname = lname, lgroup = lgroup,
    data = list(xs = unname(xs), ys = unname(ys)), args = opts, axisTypeRange = axisTypeRange)
}

#' @export
lay_rect <- function(fig, xleft, ybottom, xright, ytop, data = NULL, fill_color = NULL, line_color = NULL, fill_alpha = 1, line_alpha = 1, hover = NULL, legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validateFig(fig, "lay_rect")

  ##### boilerplate
  validateFig(fig, "lay_points")

  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    x          <- v_eval(substitute(x), data)
    y          <- v_eval(substitute(y), data)
    size       <- v_eval(substitute(size), data)
    glyph      <- v_eval(substitute(glyph), data)
    color      <- v_eval(substitute(color), data)
    line_color <- v_eval(substitute(line_color), data)
    fill_color <- v_eval(substitute(fill_color), data)
  }

  hover <- getHover(substitute(hover), data)
  xyNames <- getXYNames(x, y, xname, yname, list(...))
  ## translate different x, y types to vectors
  xy <- getXYData(x, y)
  lgroup <- getLgroup(lgroup, fig)
  ##### boilerplate

  xname <- NULL
  yname <- NULL
  dots <- list(...)
  if("xlab" %in% names(dots))
    xname <- dots$xlab
  if("ylab" %in% names(dots))
    yname <- dots$ylab

  if(!is.null(data)) {
    xleft      <- v_eval(substitute(xleft), data)
    ybottom    <- v_eval(substitute(ybottom), data)
    xright     <- v_eval(substitute(xright), data)
    ytop       <- v_eval(substitute(ytop), data)
    fill_color <- v_eval(substitute(fill_color), data)
    line_color <- v_eval(substitute(line_color), data)
    fill_alpha <- v_eval(substitute(fill_alpha), data)
    line_alpha <- v_eval(substitute(line_alpha), data)
  }

  args <- list(fill_color = fill_color, line_color = line_color, fill_alpha = fill_alpha, line_alpha = line_alpha, ...)

  nextColor <- getNextColor(fig)

  ## defaults if nothing is specified
  if(is.null(args$line_color) && is.null(args$fill_color)) {
    args$line_color <- nextColor
    args$fill_color <- reduceSaturation(args$line_color)
  }

  axisTypeRange <- getGlyphAxisTypeRange(c(xleft, xright), c(ybottom, ytop))
  make_glyph(fig, type = "quad", lname = lname, lgroup = lgroup,
    xname = xname, yname = yname,
    data = list(left = xleft, right = xright, top = ytop, bottom = ybottom), args = args, axisTypeRange = axisTypeRange)
}

#' @export
lay_crect <- function(fig, x, y = NULL, data = NULL, width = 1, height = 1, color = NULL, fill_color = NULL, line_color = NULL, fill_alpha = 1, line_alpha = 1, angle = 0, hover = NULL, legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validateFig(fig, "lay_crect")

  # ##### boilerplate
  # validateFig(fig, "lay_crect")

  # xname <- deparse(substitute(x))
  # yname <- deparse(substitute(y))

  # ## deal with possible named inputs from a data source
  # if(!is.null(data)) {
  #   x          <- v_eval(substitute(x), data)
  #   y          <- v_eval(substitute(y), data)
  #   size       <- v_eval(substitute(size), data)
  #   glyph      <- v_eval(substitute(glyph), data)
  #   color      <- v_eval(substitute(color), data)
  #   line_color <- v_eval(substitute(line_color), data)
  #   fill_color <- v_eval(substitute(fill_color), data)
  # }

  # hover <- getHover(substitute(hover), data)
  # xyNames <- getXYNames(x, y, xname, yname, list(...))
  # ## translate different x, y types to vectors
  # xy <- getXYData(x, y)
  # lgroup <- getLgroup(lgroup, fig)
  # ##### boilerplate

  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))

  if(!is.null(data)) {
    x          <- eval(substitute(x), data)
    y          <- eval(substitute(y), data)
    fill_color <- eval(substitute(fill_color), data)
    line_color <- eval(substitute(line_color), data)
    fill_alpha <- eval(substitute(fill_alpha), data)
    line_alpha <- eval(substitute(line_alpha), data)
    angle      <- eval(substitute(angle), data)
    width      <- eval(substitute(width), data)
    height     <- eval(substitute(height), data)
  }

  hover <- getHover(substitute(hover), data)
  xy <- getXYData(x, y)
  xyNames <- getXYNames(x, y, xname, yname, list(...))
  lgroup <- getLgroup(lgroup, fig)

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
  make_glyph(fig, type = "rect", lname = lname, lgroup = lgroup,
    xname = xyNames$x, yname = xyNames$y,
    hover = hover,
    data = c(xy, list(width = width, height = height, angle = angle)),
    args = opts, axisTypeRange = axisTypeRange)
}

