#' Add a "lines" layer to a Bokeh figure
#' Draws lines with the given coordinates.
#' @param fig figure to modify
#' @param x values or field name of line x coordinates
#' @param y values or field name of line y coordinates
#' @param data an optional data frame, providing the source for inputs x, y, and other glyph properties
#' @param group values or field name of a grouping variable to break lines up by
#' @template par-lineprops
#' @template par-legend
#' @template par-lnamegroup
#' @template dots-line
#' @example man-roxygen/ex-lines.R
#' @family layer functions
#' @export
ly_lines <- function(fig, x, y = NULL, data = NULL, group = NULL,
  color = "black", type = 1, width = 1, alpha = 1,
  legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validate_fig(fig, "ly_lines")
  ## see if any options won't be used and give a message
  check_opts(list(...), "line", formals = names(formals(ly_lines)))

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

  ## v_eval will repeat these, but the line glyph doesn't like this
  if(length(unique(color)) == 1)
    color <- color[1]
  if(length(unique(color)) == 1)
    type <- type[1]
  if(length(unique(color)) == 1)
    width <- width[1]

  xy_names <- get_xy_names(x, y, xname, yname, list(...))
  ## translate different x, y types to vectors
  xy <- get_xy_data(x, y)
  lgroup <- get_lgroup(lgroup, fig)

  args <- list(glyph = "line", group = group, color = color,
    width = width, type = type, ...)

  if(missing(color) && !is.null(args$line_color))
    args$color <- NULL

  args$alpha <- alpha

  # if any of group, type, width, or color are not unique, we need to split the data
  # and call make_glyph several times
  # otherwise we can just vary the values of things
  # and call make_glyph just once...
  group_vars <- c("group", "type", "width", "color")
  groupable <- which(names(args) %in% group_vars &
    sapply(args, function(x) length(unique(x)) > 1))
  if(length(groupable) > 0) {

    g_args <- args[groupable]
    ng_args <- args[-groupable]

    lns <- sapply(ng_args, length)
    idx <- which(lns == length(xy$x))

    df_args <- args[idx]
    df_args$group <- NULL

    ## much more efficient way to do this but would probably require more dependencies...
    lvls <- apply(as.matrix(data.frame(g_args)), 1,
      function(x) paste(x, collapse = ""))
    df_split <- split(seq_along(lvls), lvls)

    g_args$group <- NULL

    for(ii in seq_along(df_split)) {
      cur_idx <- df_split[[ii]]

      fig <- do.call(ly_lines,
        c(lapply(df_args, function(x) subset_with_attributes(x, cur_idx)),
          lapply(g_args, function(x) subset_with_attributes(x, cur_idx[1])),
          ng_args[-idx], list(fig = fig, x = xy$x[cur_idx], y = xy$y[cur_idx],
            lgroup = lgroup, lname = ii, xlab = xy_names$x, ylab = xy_names$y)))
    }
    return(fig)
  }

  args <- resolve_line_args(fig, args)

  axis_type_range <- get_glyph_axis_type_range(xy$x, xy$y)

  make_glyph(fig, type = "line", lname = lname, lgroup = lgroup,
    data = xy, legend = legend,
    args = args, axis_type_range = axis_type_range,
    xname = xy_names$x, yname = xy_names$y)
}

#' Add a "segments" layer to a Bokeh figure
#'
#' Draws line segments with the given starting and ending coordinates.
#' @param fig figure to modify
#' @param x0 values or field name of starting x coordinates
#' @param y0 values or field name of starting y coordinates
#' @param x1 values or field name of ending x coordinates
#' @param y1 values or field name of ending y coordinates
#' @param data an optional data frame, providing the source for inputs x, y, and other glyph properties
#' @template par-lineprops
#' @template par-legend
#' @template par-lnamegroup
#' @template dots-line
#' @family layer functions
#' @export
ly_segments <- function(fig, x0, y0, x1, y1, data = NULL,
  color = "black", alpha = 1, width = 1, type = 1,
  legend = NULL, lname = NULL, lgroup = NULL, ...) {

  ## see if any options won't be used and give a message
  check_opts(list(...), "segment", formals = names(formals(ly_segments)))

  validate_fig(fig, "ly_segments")

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

  xy_names <- get_xy_names(x0, y0, xname, yname, list(...))

  lgroup <- get_lgroup(lgroup, fig)

  args <- list(glyph = "segment", color = color,
    alpha = alpha, width = width,
    type = type, ...)

  if(missing(color) && !is.null(args$line_color))
    args$color <- NULL

  args <- resolve_line_args(fig, args)

  axis_type_range <- get_glyph_axis_type_range(c(x0, x1), c(y0, y1))
  make_glyph(fig, type = "segment",
    xname = xy_names$x, yname = xy_names$y,
    legend = legend, lname = lname, lgroup = lgroup,
    data = list(x0 = x0, y0 = y0, x1 = x1, y1 = y1),
    args = args, axis_type_range = axis_type_range)
}

#' Add an "abline" layer to a Bokeh figure
#'
#' Draws one or more straight lines.
#' @param fig figure to modify
#' @param a,b the intercept and slope of the line(s) to draw
#' @param v the x value(s) for vertical lines
#' @param h the y value(s) for horizontal lines
#' @param coef a vector of length two giving the intercept and slope
#' @template par-lineprops
#' @template par-legend
#' @template par-lnamegroup
#' @template dots-line
#' @example man-roxygen/ex-lines.R
#' @family layer functions
#' @export
ly_abline <- function(fig, a = NULL, b = NULL, v = NULL, h = NULL, coef = NULL,
  color = "black", alpha = NULL, width = 1, type = 1,
  legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validate_fig(fig, "ly_abline")
  ## see if any options won't be used and give a message
  check_opts(list(...), "segment", formals = names(formals(ly_abline)))

  xy_names <- get_xy_names(NULL, NULL, "x", "y", list(...))

  lgroup <- get_lgroup(lgroup, fig)

  args <- list(glyph = "segment", color = color,
    alpha = alpha, width = width,
    type = type, ...)

  if(missing(color) && !is.null(args$line_color))
    args$color <- NULL

  args <- resolve_line_args(fig, args)

  if(!is.null(coef) || inherits(a, "lm")) {
    if(is.null(coef))
      coef <- a
    if(inherits(coef, "lm"))
      coef <- coef(coef)
    coef <- as.numeric(coef)
    a <- coef[1]
    b <- coef[2]
  }

  x_axis_type <- "numeric"
  y_axis_type <- "numeric"

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
      y_axis_type <- "datetime"
      h <- to_epoch(h)
    }
    nn <- length(h)
    x0 <- rep(0, nn)
    y0 <- h
    x1 <- rep(1, nn)
    y1 <- h
  } else if(!is.null(v)) {
    if(inherits(v, c("Date", "POSIXct"))) {
      x_axis_type <- "datetime"
      v <- to_epoch(v)
    }
    nn <- length(v)
    x0 <- v
    y0 <- rep(0, nn)
    x1 <- v
    y1 <- rep(1, nn)
  }

  defer_fn <- function(data, xlim, ylim) {
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

  axis_type_range <- list(
    x_axis_type = x_axis_type, y_axis_type = y_axis_type,
    x_range = NULL, y_range = NULL)

  make_glyph(fig, type = "segment", legend = legend,
    lname = lname, lgroup = lgroup,
    xname = xy_names$x, yname = xy_names$y,
    data = list(x0 = x0, y0 = y0, x1 = x1, y1 = y1, defer = defer_fn),
    args = args, axis_type_range = axis_type_range)
}

#' Add a "curve" layer to a Bokeh figure
#'
#' Draws a curve corresponding to a function over the interval \code{[from, to]}.
#' @param fig figure to modify
#' @param expr,from,to,n parameters sent to \code{\link[graphics]{curve}}
#' @template par-lineprops
#' @template par-legend
#' @template par-lnamegroup
#' @template dots-line
#' @examples
#' \donttest{
#' chippy <- function(x) sin(cos(x)*exp(-x/2))
#' figure(width = 800) %>%
#'   ly_curve(chippy, -8, 7, n = 2001)
#' }
#' @family layer functions
#' @export
ly_curve <- function(fig, expr, from = NULL, to = NULL, n = 101,
  color = "black", alpha = 1, width = 1, type = 1,
  legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validate_fig(fig, "ly_curve")
  ## see if any options won't be used and give a message
  check_opts(list(...), "line", formals = names(formals(ly_curve)))

  xname <- "x"

  lgroup <- get_lgroup(lgroup, fig)

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
    type = type, ...)

  if(missing(color) && !is.null(args$line_color))
    args$color <- NULL

  args <- resolve_line_args(fig, args)

  do.call(ly_lines, c(list(fig = fig, x = x, y = y, legend = legend,
    lname = lname, lgroup = lgroup, xlab = xname, ylab = yname), args))
}

#' Add a "contour" layer to a Bokeh figure
#'
#' Computes and draws contour lines.
#' @param fig figure to modify
#' @param z a matrix containing the values to compute contour lines for
#' @param x,y locations of grid lines at which the values in \code{image} are measured (see \code{\link[grDevices]{contourLines}})
#' @param nlevels,levels parameters sent to \code{\link[grDevices]{contourLines}})
#' @template par-lineprops
#' @template par-lnamegroup
#' @template dots-line
#' @example man-roxygen/ex-image.R
#' @family layer functions
#' @export
ly_contour <- function(fig, z,
  x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, length.out = ncol(z)),
  nlevels = 10, levels = pretty(range(z, na.rm = TRUE), nlevels),
  color = "black", alpha = 1, width = 1, type = 1,
  lname = NULL, lgroup = NULL, ...) {

  validate_fig(fig, "ly_contour")
  ## see if any options won't be used and give a message
  check_opts(list(...), "multi_line", formals = names(formals(ly_contour)))

  xy_names <- get_xy_names(NULL, NULL, "x", "y", list(...))

  lgroup <- get_lgroup(lgroup, fig)

  args <- list(color = color,
    alpha = alpha, width = width,
    type = type, ...)

  args <- resolve_line_args(fig, args)

  contr <- do.call(contourLines, list(x = x, y = y, z = z, nlevels = nlevels, levels = levels))

  xs <- lapply(contr, "[[", 2)
  ys <- lapply(contr, "[[", 3)

  axis_type_range <- get_glyph_axis_type_range(x, y, assert_x = "numeric", assert_y = "numeric")

  make_glyph(fig, type = "multi_line", lname = lname, lgroup = lgroup,
    xname = xy_names$x, yname = xy_names$y,
    data = list(xs = xs, ys = ys),
    args = args, axis_type_range = axis_type_range)
}

#' Add a "ray" layer to a Bokeh figure
#'
#' Draws line segments starting at the given coordinate and extending the given length at the given angle.
#' @param fig figure to modify
#' @param x values or field name of center x coordinates
#' @param y values or field name of center y coordinates
#' @param data an optional data frame, providing the source for inputs x, y, and other glyph properties
#' @param length values or field name of ray lengths in screen units
#' @param angle values or field name of ray angles
#' @template par-lineprops
#' @template par-legend
#' @template par-lnamegroup
#' @template dots-line
#' @family layer functions
#' @export
ly_ray <- function(fig, x, y = NULL, data = NULL, length = NULL, angle = 0,
  color = "black", type = 1, width = 1, alpha = NULL,
  legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validate_fig(fig, "ly_ray")
  ## see if any options won't be used and give a message
  check_opts(list(...), "ray", formals = names(formals(ly_ray)))

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

  xy_names <- get_xy_names(x, y, xname, yname, list(...))
  ## translate different x, y types to vectors
  xy <- get_xy_data(x, y)
  lgroup <- get_lgroup(lgroup, fig)

  args <- list(glyph = "ray", color = color,
    alpha = alpha, width = width, type = type,
    length = length, angle = angle, ...)

  if(missing(color) && !is.null(args$line_color))
    args$color <- NULL

  args <- resolve_line_args(fig, args)

  axis_type_range <- get_glyph_axis_type_range(x, y)

  make_glyph(fig, type = "ray", lname = lname, lgroup = lgroup,
    xname = xy_names$x, yname = xy_names$y,
    data = xy, legend = legend,
    args = args, axis_type_range = axis_type_range)
}


#' Add a "bezier" layer to a Bokeh figure
#'
#' Draws Bezier curves with the given starting, ending, and control points.
#' @param fig figure to modify
#' @param x0 values or field name of starting x coordinates
#' @param y0 values or field name of starting y coordinates
#' @param x1 values or field name of ending x coordinates
#' @param y1 values or field name of ending y coordinates
#' @param cx0 values or field name of first control point x coordinates
#' @param cy0 values or field name of first control point y coordinates
#' @param cx1 values or field name of second control point x coordinates
#' @param cy1 values or field name of second control point y coordinates
#' @param data an optional data frame, providing the source for start, end, and control point intputs, as well as other glyph properties
#' @template par-lineprops
#' @template par-legend
#' @template par-lnamegroup
#' @template dots-line
#' @family layer functions
#' @export
ly_bezier <- function(fig, x0, y0, x1, y1, cx0, cy0, cx1, cy1, data = NULL,
  color = "black", alpha = 1, width = 1, type = 1,
  legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validate_fig(fig, "ly_bezier")
  ## see if any options won't be used and give a message
  check_opts(list(...), "bezier", formals = names(formals(ly_bezier)))

  xname <- deparse(substitute(x0))
  yname <- deparse(substitute(y0))

  if(!is.null(data)) {
    x0    <- v_eval(substitute(x0), data)
    y0    <- v_eval(substitute(y0), data)
    x1    <- v_eval(substitute(x1), data)
    y1    <- v_eval(substitute(y1), data)
    cx0   <- v_eval(substitute(cx0), data)
    cy0   <- v_eval(substitute(cy0), data)
    cx1   <- v_eval(substitute(cx1), data)
    cy1   <- v_eval(substitute(cy1), data)
    color <- v_eval(substitute(color), data)
    type  <- v_eval(substitute(type), data)
    width <- v_eval(substitute(width), data)
  }

  xy_names <- get_xy_names(x0, y0, xname, yname, list(...))
  lgroup <- get_lgroup(lgroup, fig)

  args <- list(glyph = "bezier", color = color, type = type,
    width = width, alpha = alpha, ...)

  if(missing(color) && !is.null(args$line_color))
    args$color <- NULL

  args <- resolve_line_args(fig, args)

  axis_type_range <- get_glyph_axis_type_range(c(x0, x1), c(y0, y1),
    assert_x = "numeric", assert_y = "numeric")

  make_glyph(fig, type = "bezier", lname = lname, lgroup = lgroup,
    xname = xy_names$x, yname = xy_names$y,
    data = list(x0 = x0, y0 = y0, x1 = x1, y1 = y1,
      cx0 = cx0, cy0 = cy0, cx1 = cx1, cy1 = cy1),
    args = args, axis_type_range = axis_type_range)
}

#' Add a "quadratic" layer to a Bokeh figure
#'
#' Draws quadratic curves with the given starting, ending, and control points.
#' @param fig figure to modify
#' @param x0 values or field name of starting x coordinates
#' @param y0 values or field name of starting y coordinates
#' @param x1 values or field name of ending x coordinates
#' @param y1 values or field name of ending y coordinates
#' @param cx values or field name of control point x coordinates
#' @param cy values or field name of control point y coordinates
#' @param data an optional data frame, providing the source for start, end, and control point intputs, as well as other glyph properties
#' @template par-lineprops
#' @template par-legend
#' @template par-lnamegroup
#' @template dots-fillline
#' @family layer functions
#' @export
ly_quadratic <- function(fig, x0, y0, x1, y1, cx, cy, data = NULL,
  color = "black", alpha = 1, width = 1, type = 1,
  legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validate_fig(fig, "ly_quadratic")
  ## see if any options won't be used and give a message
  check_opts(list(...), "quadratic", formals = names(formals(ly_quadratic)))

  xname <- deparse(substitute(x0))
  yname <- deparse(substitute(y0))

  if(!is.null(data)) {
    x0    <- v_eval(substitute(x0), data)
    y0    <- v_eval(substitute(y0), data)
    x1    <- v_eval(substitute(x1), data)
    y1    <- v_eval(substitute(y1), data)
    cx    <- v_eval(substitute(cx), data)
    cy    <- v_eval(substitute(cy), data)
    color <- v_eval(substitute(color), data)
    type  <- v_eval(substitute(type), data)
    width <- v_eval(substitute(width), data)
  }

  xy_names <- get_xy_names(x0, y0, xname, yname, list(...))
  lgroup <- get_lgroup(lgroup, fig)

  args <- list(glyph = "quadratic", color = color, type = type,
    width = width, alpha = alpha, ...)

  if(missing(color) && !is.null(args$line_color))
    args$color <- NULL

  args <- resolve_line_args(fig, args)

  axis_type_range <- get_glyph_axis_type_range(c(x0, x1), c(y0, y1),
    assert_x = "numeric", assert_y = "numeric")

  make_glyph(fig, type = "quadratic", lname = lname, lgroup = lgroup,
    xname = xy_names$x, yname = xy_names$y,
    data = list(x0 = x0, y0 = y0, x1 = x1, y1 = y1, cx = cx, cy = cy),
    args = list(...), axis_type_range = axis_type_range)
}

## a common thing to do is make a layer with both points and lines (type = "b")
# ly_pointsline <- function()

#' Add a "multi_line" layer to a Bokeh figure
#'
#' Draws multiple lines with the given lists of coordinates.
#' @param fig figure to modify
#' @param xs list of vectors of x coordinates
#' @param ys list of vectors of y coordinates
#' @template par-lineprops
#' @template par-lnamegroup
#' @template dots-line
#' @family layer functions
#' @export
ly_multi_line <- function(fig, xs, ys,
  color = "black", alpha = 1, width = 1, type = 1,
  lname = NULL, lgroup = NULL, ...) {

  xname <- deparse(substitute(xs))
  yname <- deparse(substitute(ys))

  validate_fig(fig, "ly_multi_line")
  ## see if any options won't be used and give a message
  check_opts(list(...), "multi_line")

  args <- list(glyph = "line", color = color,
    width = width, type = type, ...)

  if(missing(color) && !is.null(args$line_color))
    args$color <- NULL

  args$alpha <- alpha

  args <- resolve_line_args(fig, args)

  axis_type_range <- get_glyph_axis_type_range(unlist(xs), unlist(ys))

  make_glyph(fig, type = "multi_line", xname = xname, yname = yname,
    lname = lname, lgroup = lgroup,
    data = list(xs = xs, ys = ys), args = args,
    axis_type_range = axis_type_range)
}
