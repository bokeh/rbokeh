#' @export
lay_lines <- function(fig, x, y = NULL, group = NULL, data = NULL, type = 1, width = 1, color = NULL, alpha = NULL, line_join = 1, line_cap = "round", legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validateFig(fig, "lay_lines")
  ## see if any options won't be used and give a message
  checkOpts(list(...), "line")

  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    x          <- v_eval(substitute(x), data)
    y          <- v_eval(substitute(y), data)
    group      <- v_eval(substitute(group), data)
    type       <- v_eval(substitute(type), data)
    width      <- v_eval(substitute(width), data)
    color      <- v_eval(substitute(color), data)
  }

  xyNames <- getXYNames(x, y, xname, yname, list(...))
  ## translate different x, y types to vectors
  xy <- getXYData(x, y)
  lgroup <- getLgroup(lgroup, fig)

  args <- list(glyph = "line", group = group, line_color = color,
    line_alpha = alpha, line_width = width,
    line_dash = type, line_join = line_join,
    line_cap = line_cap, ...)

  # if any of group, type, width, or color are not unique, we need to split the data
  # and call make_glyph several times
  # otherwise we can just vary the values of things
  # and call make_glyph just once...
  groupVars <- c("group", "line_dash", "line_width", "line_color")
  groupable <- which(sapply(args[groupVars], function(x) length(unique(x)) > 1))
  if(length(groupable) > 0) {
    browser()

    # split(seq_along())

    gl <- args$glyph
    args$glyph <- NULL
    lns <- sapply(args, length)
    idx <- which(lns == length(xy$x))

    dfArgs <- args[idx]

    if(is.character(gl))
      gl <- factor(gl)

    dfSplit <- split(seq_along(gl), gl)
    for(ii in seq_along(dfSplit)) {
      curIdx <- dfSplit[[ii]]
      curGlyph <- paste("glyph_")

      fig <- do.call(lay_lines,
        c(lapply(dfArgs, function(x) subset_with_attributes(x, curIdx)), args[-idx],
          list(fig = fig, x = xy$x[curIdx], y = xy$y[curIdx],
            glyph = subset_with_attributes(gl, curIdx[1]), lgroup = lgroup,
            lname = ii, hover = hover$data[curIdx, , drop = FALSE])))
    }
    return(fig)
  }

  args <- updateLineOpts(fig, args)

  axisTypeRange <- getGlyphAxisTypeRange(xy$x, xy$y)

  make_glyph(fig, type = "line", lname = lname, lgroup = lgroup,
    data = xy, legend = legend,
    args = args, axisTypeRange = axisTypeRange,
    xname = xyNames$x, yname = xyNames$y)
}

#' @export
lay_multi_line <- function(fig, xs, ys, lname = NULL, lgroup = NULL, ...) {

  ##### boilerplate
  validateFig(fig, "lay_multi_line")
  ## see if any options won't be used and give a message
  checkOpts(list(...), "multi_line")

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

  axisTypeRange <- getGlyphAxisTypeRange(unlist(xs), unlist(ys))
  make_glyph(fig, type = "multi_line", lname = lname, lgroup = lgroup,
    data = list(xs = xs, ys = ys), args = list(...), axisTypeRange = axisTypeRange)
}

#' @export
lay_segments <- function(fig, x0, y0, x1, y1, data = NULL, line_color = NULL, line_alpha = NULL, line_width = 1, line_dash = 1, line_join = 1, line_cap = "round", lname = NULL, lgroup = NULL, ...) {

  validateFig(fig, "lay_segments")
  ## see if any options won't be used and give a message
  checkOpts(list(...), "segments")

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
    x0         <- v_eval(substitute(x0), data)
    y0         <- v_eval(substitute(y0), data)
    x1         <- v_eval(substitute(x1), data)
    y1         <- v_eval(substitute(y1), data)
    line_color <- v_eval(substitute(line_color), data)
    line_alpha <- v_eval(substitute(line_alpha), data)
  }

  args <- list(glyph = "segment", line_color = line_color,
    line_alpha = line_alpha, line_width = line_width,
    line_dash = line_dash, line_join = line_join,
    line_cap = line_cap, ...)

  args <- updateLineOpts(fig, args)

  axisTypeRange <- getGlyphAxisTypeRange(c(x0, x1), c(y0, y1))
  make_glyph(fig, type = "segment", lname = lname, lgroup = lgroup,
    data = list(x0 = x0, y0 = y0, x1 = x1, y1 = y1),
    args = args, axisTypeRange = axisTypeRange)
}

#' @export
lay_abline <- function(fig, a = NULL, b = NULL, v = NULL, h = NULL, coef = NULL, line_color = "black", line_alpha = NULL, line_width = 1, line_dash = 1, lname = NULL, lgroup = NULL, ...) {

  ##### boilerplate
  validateFig(fig, "lay_abline")
  ## see if any options won't be used and give a message
  checkOpts(list(...), "abline")

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

  args <- list(glyph = "segment", line_color = line_color,
    line_alpha = line_alpha, line_width = line_width,
    line_dash = line_dash, ...)

  args <- updateLineOpts(fig, args)

  if(!is.null(coef) || inherits(a, "lm")) {
    if(is.null(coef))
      coef <- a
    if(inherits(coef, "lm"))
      coef <- coef(coef)
    coef <- as.numeric(coef)
    a <- coef[1]
    b <- coef[2]
  }

  if(!is.null(a) && !is.null(b)) {
    nn <- max(c(length(a), length(b)))
    if(length(a) < nn)
      a <- rep(a, nn)[1:nn]
    if(length(b) < nn)
      b <- rep(b, nn)[1:nn]
    x0 <- rep(0, nn)
    y0 <- a
    x1 <- rep(1, nn)
    y1 <- b * x1 + a
  } else if(!is.null(h)) {
    nn <- length(h)
    x0 <- rep(0, nn)
    y0 <- h
    x1 <- rep(1, nn)
    y1 <- h
  } else if(!is.null(v)) {
    nn <- length(v)
    x0 <- v
    y0 <- rep(0, nn)
    x1 <- v
    y1 <- rep(1, nn)
  }

  deferFn <- function(data, xlim, ylim) {
    if(length(data$x0) == 1) {
      if(data$x0 == "x0")
        return(data)
    } else if(length(data$x0) == 0) {
      return(data)
    }
    if(all(data$x0 == data$x1)) {
      ## vertical lines
      data$y0 <- rep(ylim[1], length(data$y0))
      data$y1 <- rep(ylim[2], length(data$y1))
    } else if(all(data$y0 == data$y1)) {
      ## horizontal line
      data$x0 <- rep(xlim[1], length(data$x0))
      data$x1 <- rep(xlim[2], length(data$x1))
    } else {
      ## line
      b <- (data$y1 - data$y0) / (data$x1 - data$x0)
      a <- data$y1 - b * data$x1
      nn <- length(a)
      data$x0 <- rep(xlim[1], nn)
      data$x1 <- rep(xlim[2], nn)
      data$y0 <- data$x0 * b + a
      data$y1 <- data$x1 * b + a
    }
    data
  }

  axisTypeRange <- list(
    xAxisType = "numeric", yAxisType = "numeric",
    xRange = NULL, yRange = NULL)

  make_glyph(fig, type = "segment", lname = lname, lgroup = lgroup,
    data = list(x0 = x0, y0 = y0, x1 = x1, y1 = y1, defer = deferFn),
    args = args, axisTypeRange = axisTypeRange)
}

#' @export
lay_curve <- function(fig, expr, from = NULL, to = NULL, n = 101, xname = "x", line_color = "black", line_alpha = NULL, line_width = 1, line_dash = 1, line_join = 1, line_cap = "round", lname = NULL, lgroup = NULL, ...) {

  xname <- "x"

  ##### boilerplate
  validateFig(fig, "lay_curve")
  ## see if any options won't be used and give a message
  checkOpts(list(...), "curve")

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

  sexpr <- substitute(expr)
  if (is.name(sexpr)) {
    yname <- paste(deparse(sexpr), "(x)", sep = "")
    expr <- call(as.character(sexpr), as.name(xname))
  } else {
    yname <- deparse(sexpr)
    if (!((is.call(sexpr) || is.expression(sexpr)) && xname %in%
      all.vars(sexpr)))
      stop(gettextf("'expr' must be a function, or a call or an expression containing '%s'", xname), domain = NA)
    expr <- sexpr
  }

  x <- seq.int(from, to, length.out = n)
  ll <- list(x = x)
  names(ll) <- xname
  y <- eval(expr, envir = ll, enclos = parent.frame())

  opts <- c(list(line_color = line_color,
    line_alpha = line_alpha, line_width = line_width,
    line_dash = line_dash, line_join = line_join,
    line_cap = line_cap), list(...))

  opts <- updateLineOpts(fig, opts)

  do.call(lay_lines, c(list(fig = fig, x = x, y = y, lname = lname, lgroup = lgroup, xlab = xname, ylab = yname), opts))
}

#' @export
lay_contour <- function(fig, image, x = seq(0, 1, length.out = nrow(image)), y = seq(0, 1, length.out = ncol(image)), nlevels = 10, levels = pretty(range(image, na.rm = TRUE), nlevels), line_color = "black", line_alpha = 0.75, line_width = 1, line_dash = 1, line_join = 1, line_cap = "round", lname = NULL, lgroup = NULL, ...) {

  ##### boilerplate
  validateFig(fig, "lay_contour")
  ## see if any options won't be used and give a message
  checkOpts(list(...), "contour")

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

  opts <- c(list(line_color = line_color,
    line_alpha = line_alpha, line_width = line_width,
    line_dash = line_dash, line_join = line_join,
    line_cap = line_cap), list(...))

  opts <- updateLineOpts(fig, opts)

  contr <- do.call(contourLines, list(x = x, y = y, z = image, nlevels = nlevels, levels = levels))

  xs <- lapply(contr, "[[", 2)
  ys <- lapply(contr, "[[", 3)

  axisTypeRange <- getGlyphAxisTypeRange(x, y, assertX = "numeric", assertY = "numeric")

  make_glyph(fig, type = "multi_line", lname = lname, lgroup = lgroup,
    data = list(xs = xs, ys = ys),
    args = opts, axisTypeRange = axisTypeRange)
}

# lay_pointline

#' @export
lay_bezier <- function(fig, x0, y0, x1, y1, cx0, cy0, cx1, cy1, data = NULL, hover = NULL, legend = NULL, lname = NULL, lgroup = NULL, ...) {

  ##### boilerplate
  validateFig(fig, "lay_bezier")
  ## see if any options won't be used and give a message
  checkOpts(list(...), "bezier")

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

  axisTypeRange <- getGlyphAxisTypeRange(c(x0, x1), c(y0, y1), assertX = "numeric", assertY = "numeric")
  make_glyph(fig, type = "bezier", lname = lname, lgroup = lgroup,
    data = list(x0 = x0, y0 = y0, x1 = x1, y1 = y1, cx0 = cx0, cy0 = cy0, cx1 = cx1, cy1 = cy1),
    args = list(...), axisTypeRange = axisTypeRange)
}


#' @export
lay_quadratic <- function(fig, x0, y0, x1, y1, cx, cy, data = NULL, hover = NULL, legend = NULL, lname = NULL, lgroup = NULL, ...) {

  ##### boilerplate
  validateFig(fig, "lay_quadratic")
  ## see if any options won't be used and give a message
  checkOpts(list(...), "quadratic")

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

  axisTypeRange <- getGlyphAxisTypeRange(c(x0, x1), c(y0, y1), assertX = "numeric", assertY = "numeric")

  make_glyph(fig, type = "quadratic", lname = lname, lgroup = lgroup,
    data = list(x0 = x0, y0 = y0, x1 = x1, y1 = y1, cx = cx, cy = cy),
    args = list(...), axisTypeRange = axisTypeRange)
}

#' @export
lay_ray <- function(fig, x, y = NULL, data = NULL, length, angle = 0, hover = NULL, legend = NULL, lname = NULL, lgroup = NULL, ...) {

  ##### boilerplate
  validateFig(fig, "lay_ray")
  ## see if any options won't be used and give a message
  checkOpts(list(...), "ray")

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

  axisTypeRange <- getGlyphAxisTypeRange(x, y)
  make_glyph(fig, type = "ray", lname = lname, lgroup = lgroup,
    data = list(x = x, y = y, length = length, angle = angle),
    args = list(...), axisTypeRange = axisTypeRange)
}
