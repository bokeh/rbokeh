#' @export
lay_lines <- function(fig, x, y = NULL, data = NULL, line_color = NULL, line_alpha = NULL, line_width = 1, line_dash = 1, line_join = 1, line_cap = "round", lname = NULL, lgroup = NULL, ...) {

  validateFig(fig, "lay_lines")

  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))

  if(!is.null(data)) {
    x          <- eval(substitute(x), data)
    y          <- eval(substitute(y), data)
    line_color <- eval(substitute(line_color), data)
    line_alpha <- eval(substitute(line_alpha), data)
  }

  xy <- getXYData(x, y)
  xyNames <- getXYNames(x, y, xname, yname, list(...))

  opts <- c(list(line_color = line_color, 
    line_alpha = line_alpha, line_width = line_width, 
    line_dash = line_dash, line_join = line_join, 
    line_cap = line_cap), list(...))

  opts <- updateLineOpts(fig, opts)

  axisTypeRange <- getGlyphAxisTypeRange(xy$x, xy$y)

  makeGlyph(fig, type = "line", lname = lname, lgroup = lgroup, 
    data = xy,
    args = opts, axisTypeRange = axisTypeRange, 
    xname = xyNames$x, yname = xyNames$y)
}

#' @export
lay_multi_line <- function(fig, xs, ys, lname = NULL, lgroup = NULL, ...) {
  axisTypeRange <- getGlyphAxisTypeRange(unlist(xs), unlist(ys))
  makeGlyph(fig, type = "multi_line", lname = lname, lgroup = lgroup,
    data = list(xs = xs, ys = ys), args = list(...), axisTypeRange = axisTypeRange)
}

#' @export
lay_segments <- function(fig, x0, y0, x1, y1, data = NULL, line_color = NULL, line_alpha = NULL, line_width = 1, line_dash = 1, line_join = 1, line_cap = "round", lname = NULL, lgroup = NULL, ...) {

  validateFig(fig, "lay_segments")

  if(!is.null(data)) {
    x0         <- eval(substitute(x0), data)
    y0         <- eval(substitute(y0), data)
    x1         <- eval(substitute(x1), data)
    y1         <- eval(substitute(y1), data)
    line_color <- eval(substitute(line_color), data)
    line_alpha <- eval(substitute(line_alpha), data)
  }

  opts <- c(list(line_color = line_color, 
    line_alpha = line_alpha, line_width = line_width, 
    line_dash = line_dash, line_join = line_join, 
    line_cap = line_cap), list(...))

  opts <- updateLineOpts(fig, opts)

  axisTypeRange <- getGlyphAxisTypeRange(c(x0, x1), c(y0, y1))
  makeGlyph(fig, type = "segment", lname = lname, lgroup = lgroup,
    data = list(x0 = x0, y0 = y0, x1 = x1, y1 = y1),
    args = opts, axisTypeRange = axisTypeRange)
}

#' @export 
lay_abline <- function(fig, a = NULL, b = NULL, v = NULL, h = NULL, coef = NULL, line_color = "black", line_alpha = NULL, line_width = 1, line_dash = 1, lname = NULL, lgroup = NULL, ...) {

  validateFig(fig, "lay_abline")

  opts <- c(list(line_color = line_color, 
    line_alpha = line_alpha, line_width = line_width, 
    line_dash = line_dash), list(...))

  opts <- updateLineOpts(fig, opts)

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

  makeGlyph(fig, type = "segment", lname = lname, lgroup = lgroup,
    data = list(x0 = x0, y0 = y0, x1 = x1, y1 = y1, defer = deferFn),
    args = opts, axisTypeRange = axisTypeRange)
}

#' @export
lay_curve <- function(fig, expr, from = NULL, to = NULL, n = 101, xname = "x", line_color = "black", line_alpha = NULL, line_width = 1, line_dash = 1, line_join = 1, line_cap = "round", lname = NULL, lgroup = NULL, ...) {

  xname <- "x"

  validateFig(fig, "lay_lines")

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

  validateFig(fig, "lay_contour")

  opts <- c(list(line_color = line_color, 
    line_alpha = line_alpha, line_width = line_width, 
    line_dash = line_dash, line_join = line_join, 
    line_cap = line_cap), list(...))

  opts <- updateLineOpts(fig, opts)

  contr <- do.call(contourLines, list(x = x, y = y, z = image, nlevels = nlevels, levels = levels))

  xs <- lapply(contr, "[[", 2)
  ys <- lapply(contr, "[[", 3)

  axisTypeRange <- getGlyphAxisTypeRange(x, y, assertX = "numeric", assertY = "numeric")

  makeGlyph(fig, type = "multi_line", lname = lname, lgroup = lgroup,
    data = list(xs = xs, ys = ys),
    args = opts, axisTypeRange = axisTypeRange)
}

# lay_pointline

# lay_hline
# lay_vline



