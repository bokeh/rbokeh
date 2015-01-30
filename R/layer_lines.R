
#' @export
ly_line <- function(fig, x, y = NULL, data = NULL, group = NULL,
  color = "black", type = 1, width = 1, alpha = NULL,
  line_join = 1, line_cap = "round",
  legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validateFig(fig, "ly_line")
  ## see if any options won't be used and give a message
  checkOpts(list(...), "line")

  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    x     <- v_eval(substitute(x), data)
    y     <- v_eval(substitute(y), data)
    group <- v_eval(substitute(group), data)
    color <- v_eval(substitute(color), data)
    type  <- v_eval(substitute(type), data)
    width <- v_eval(substitute(width), data)
  }

  xyNames <- getXYNames(x, y, xname, yname, list(...))
  ## translate different x, y types to vectors
  xy <- getXYData(x, y)
  lgroup <- getLgroup(lgroup, fig)

  args <- list(glyph = "line", group = group, color = color,
    width = width, type = type, line_join = line_join,
    line_cap = line_cap, ...)

  args$alpha <- alpha

  # if any of group, type, width, or color are not unique, we need to split the data
  # and call make_glyph several times
  # otherwise we can just vary the values of things
  # and call make_glyph just once...
  groupVars <- c("group", "type", "width", "color")
  groupable <- which(names(args) %in% groupVars &
    sapply(args, function(x) length(unique(x)) > 1))
  if(length(groupable) > 0) {

    gArgs <- args[groupable]
    ngArgs <- args[-groupable]

    lns <- sapply(ngArgs, length)
    idx <- which(lns == length(xy$x))

    dfArgs <- args[idx]

    ## much more efficient way to do this but would probably require more dependencies...
    lvls <- apply(as.matrix(data.frame(gArgs)), 1,
      function(x) paste(x, collapse = ""))
    dfSplit <- split(seq_along(lvls), lvls)

    for(ii in seq_along(dfSplit)) {
      curIdx <- dfSplit[[ii]]

      fig <- do.call(ly_line,
        c(lapply(dfArgs, function(x) subset_with_attributes(x, curIdx)),
          lapply(gArgs, function(x) subset_with_attributes(x, curIdx[1])),
          ngArgs[-idx], list(fig = fig, x = xy$x[curIdx], y = xy$y[curIdx],
            lgroup = lgroup, lname = ii, xlab = xyNames$x, ylab = xyNames$y)))
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
ly_segments <- function(fig, x0, y0, x1, y1, data = NULL,
  color = NULL, alpha = NULL, width = 1, type = 1,
  line_join = 1, line_cap = "round",
  legend = NULL, lname = NULL, lgroup = NULL, ...) {

  ## see if any options won't be used and give a message
  checkOpts(list(...), "segment")

  validateFig(fig, "ly_segments")

  xname <- deparse(substitute(x0))
  yname <- deparse(substitute(y0))

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    x0    <- v_eval(substitute(x0), data)
    y0    <- v_eval(substitute(y0), data)
    x1    <- v_eval(substitute(x1), data)
    y1    <- v_eval(substitute(y1), data)
    color <- v_eval(substitute(color), data)
    type  <- v_eval(substitute(type), data)
    width <- v_eval(substitute(width), data)
  }

  xyNames <- getXYNames(x0, y0, xname, yname, list(...))

  lgroup <- getLgroup(lgroup, fig)

  args <- list(glyph = "segment", color = color,
    alpha = alpha, width = width,
    type = type, line_join = line_join,
    line_cap = line_cap, ...)

  args <- updateLineOpts(fig, args)

  axisTypeRange <- getGlyphAxisTypeRange(c(x0, x1), c(y0, y1))
  make_glyph(fig, type = "segment",
    legend = legend, lname = lname, lgroup = lgroup,
    data = list(x0 = x0, y0 = y0, x1 = x1, y1 = y1),
    args = args, axisTypeRange = axisTypeRange)
}

#' @export
ly_abline <- function(fig, a = NULL, b = NULL, v = NULL, h = NULL, coef = NULL,
  color = "black", alpha = NULL, width = 1, type = 1,
  legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validateFig(fig, "ly_abline")
  ## see if any options won't be used and give a message
  checkOpts(list(...), "segment")

  xyNames <- getXYNames(NULL, NULL, "x", "y", list(...))

  lgroup <- getLgroup(lgroup, fig)

  args <- list(glyph = "segment", color = color,
    alpha = alpha, width = width,
    type = type, ...)

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

  xAxisType <- "numeric"
  yAxisType <- "numeric"

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
    if(inherits(h, c("Date", "POSIXct"))) {
      yAxisType <- "datetime"
      h <- toEpoch(h)
    }
    nn <- length(h)
    x0 <- rep(0, nn)
    y0 <- h
    x1 <- rep(1, nn)
    y1 <- h
  } else if(!is.null(v)) {
    if(inherits(v, c("Date", "POSIXct"))) {
      xAxisType <- "datetime"
      v <- toEpoch(v)
    }
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
    xAxisType = xAxisType, yAxisType = yAxisType,
    xRange = NULL, yRange = NULL)

  make_glyph(fig, type = "segment", legend = legend,
    lname = lname, lgroup = lgroup,
    xname = xyNames$x, yname = xyNames$y,
    data = list(x0 = x0, y0 = y0, x1 = x1, y1 = y1, defer = deferFn),
    args = args, axisTypeRange = axisTypeRange)
}

#' @export
ly_curve <- function(fig, expr, from = NULL, to = NULL, n = 101,
  color = "black", alpha = NULL, width = 1, type = 1,
  line_join = 1, line_cap = "round",
  legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validateFig(fig, "ly_curve")
  ## see if any options won't be used and give a message
  checkOpts(list(...), "line")

  xyNames <- getXYNames(NULL, NULL, "x", "f(x)", list(...))

  lgroup <- getLgroup(lgroup, fig)

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

  args <- list(color = color,
    alpha = alpha, width = width,
    type = type, line_join = line_join,
    line_cap = line_cap, ...)

  args <- updateLineOpts(fig, args)

  do.call(ly_line, c(list(fig = fig, x = x, y = y, legend = legend, lname = lname, lgroup = lgroup, xlab = xname, ylab = yname), args))
}

#' @export
ly_contour <- function(fig, image,
  x = seq(0, 1, length.out = nrow(image)), y = seq(0, 1, length.out = ncol(image)),
  nlevels = 10, levels = pretty(range(image, na.rm = TRUE), nlevels),
  color = "black", alpha = 0.75, width = 1,
  type = 1, line_join = 1, line_cap = "round",
  lname = NULL, lgroup = NULL, ...) {

  validateFig(fig, "ly_contour")
  ## see if any options won't be used and give a message
  checkOpts(list(...), "multi_line")

  xyNames <- getXYNames(NULL, NULL, "x", "y", list(...))

  lgroup <- getLgroup(lgroup, fig)

  args <- list(color = color,
    alpha = alpha, width = width,
    type = type, line_join = line_join,
    line_cap = line_cap, ...)

  args <- updateLineOpts(fig, args)

  contr <- do.call(contourLines, list(x = x, y = y, z = image, nlevels = nlevels, levels = levels))

  xs <- lapply(contr, "[[", 2)
  ys <- lapply(contr, "[[", 3)

  axisTypeRange <- getGlyphAxisTypeRange(x, y, assertX = "numeric", assertY = "numeric")

  make_glyph(fig, type = "multi_line", lname = lname, lgroup = lgroup,
    data = list(xs = xs, ys = ys),
    args = args, axisTypeRange = axisTypeRange)
}

# ly_pointline
## a common thing to do is make a layer with both points and lines (type = "b")
## so why not make it a special layer function

#' @export
ly_ray <- function(fig, x, y = NULL, data = NULL, length = NULL, angle = 0,
  color = "black", type = 1, width = 1, alpha = NULL,
  line_join = NULL, line_cap = NULL,
  legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validateFig(fig, "ly_ray")
  ## see if any options won't be used and give a message
  checkOpts(list(...), "ray")

  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    x     <- v_eval(substitute(x), data)
    y     <- v_eval(substitute(y), data)
    color <- v_eval(substitute(color), data)
    type  <- v_eval(substitute(type), data)
    width <- v_eval(substitute(width), data)
  }

  xyNames <- getXYNames(x, y, xname, yname, list(...))
  ## translate different x, y types to vectors
  xy <- getXYData(x, y)
  lgroup <- getLgroup(lgroup, fig)

  args <- list(glyph = "ray", color = color,
    alpha = alpha, width = width, type = type,
    length = length, angle = angle,
    line_join = line_join, line_cap = line_cap, ...)

  args <- updateLineOpts(fig, args)

  axisTypeRange <- getGlyphAxisTypeRange(x, y)

  make_glyph(fig, type = "ray", lname = lname, lgroup = lgroup,
    data = xy, legend = legend,
    args = args, axisTypeRange = axisTypeRange)
}
