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
ly_lines <- function(
  fig, x, y = NULL, data = figure_data(fig), group = NULL,
  color = "black", type = 1, width = 1, alpha = 1,
  legend = NULL, lname = NULL, lgroup = NULL, visible = TRUE,
  ...
) {

  validate_fig(fig, "ly_lines")

  ## deal with possible named inputs from a data source
  bv <- b_eval(data)
  args <- sub_names(fig, data,
    grab(
      x,
      y,
      group,
      color,
      type,
      width,
      alpha,
      legend,
      lname,
      lgroup,
      visible,
      dots = lazy_dots(...)
    )
  )
  args$params$glyph <- "line"

  if(missing(color) && !is.null(args$params$line_color))
    args$params$color <- NULL

  # args$params$alpha <- alpha

  # if any of group, type, width, or color are not unique, we need to split the data
  # and call make_glyph several times
  # otherwise we can just vary the values of things
  # and call make_glyph just once...
  group_dt <- args$params
  group_dt$group <- args$info$group
  group_vars <- c("group", "type", "width", "color")
  groupable <- which(
    (names(group_dt) %in% group_vars) &
    sapply(group_dt, function(x) length(unique(x)) > 1)
  )

  if (length(groupable) > 0) {
    # there are groups to split on

    # split works with a data.frame as the groups.
    split_list <- split(
      seq_along(args$data$x),
      as.data.frame(group_dt[names(groupable)])
    )
  } else {
    # no groups to split on.  will split on "one" group
    split_list <- list(seq_along(args$data$x))
  }


  axis_type_range <- get_glyph_axis_type_range(args$data$x, args$data$y)
  mc <- attr(fig, "ly_call")
  if(is.null(mc)) {
    mc <- lapply(match.call(), deparse)
  }

  for (ii in seq_along(split_list)) {
    arg_obj <- subset_arg_obj(args, split_list[[ii]])

    ## b_eval will repeat these, but the line glyph doesn't like this
    if(length(unique(arg_obj$params$color)) == 1)
      arg_obj$params$color <- subset_with_attributes(arg_obj$params$color, 1)
    if(length(unique(args$params$type)) == 1)
      arg_obj$params$type <- subset_with_attributes(arg_obj$params$type, 1)
    if(length(unique(args$params$width)) == 1)
      arg_obj$params$width <- subset_with_attributes(arg_obj$params$width, 1)

    arg_obj$params <- resolve_line_args(fig, arg_obj$params)

    ## see if any options won't be used and give a message
    check_opts(arg_obj$params, "line", formals = names(formals(ly_lines)))

    fig <- make_glyph(
      fig, type = "line", data = arg_obj$data,
      legend = arg_obj$info$legend,
      args = arg_obj$params, axis_type_range = axis_type_range,
      xname = arg_obj$info$x_name, yname = arg_obj$info$y_name,
      lname = arg_obj$info$lname, lgroup = arg_obj$info$lgroup,
      ly_call = mc
    )

  }

  return(fig)
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
ly_segments <- function(fig, x0, y0, x1, y1, data = figure_data(fig),
  color = "black", alpha = 1, width = 1, type = 1,
  legend = NULL, lname = NULL, lgroup = NULL, visible = TRUE, ...) {

  validate_fig(fig, "ly_segments")

  args <- sub_names(fig, data,
    grab(
      x0,
      y0,
      x1,
      y1,
      color,
      alpha,
      width,
      type,
      legend,
      lname,
      lgroup,
      visible,
      dots = lazy_dots(...)
    )
  )
  args$params$glyph <- "segment"

  if(missing(color) && !is.null(args$params$line_color))
    args$params$color <- NULL

  args$params <- resolve_line_args(fig, args$params)

  axis_type_range <- get_glyph_axis_type_range(
    c(args$data$x0, args$data$x1),
    c(args$data$y0, args$data$y1)
  )

  ## see if any options won't be used and give a message
  check_opts(args$params, "segment", formals = names(formals(ly_segments)))

  mc <- lapply(match.call(), deparse)

  make_glyph(
    fig, type = "segment",
    data = args$data,
    xname = args$info$x_name, yname = args$info$y_name,
    args = args$params, axis_type_range = axis_type_range,
    legend = args$info$legend, lname = args$info$lname, lgroup = args$info$lgroup,
    ly_call = mc
  )
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
ly_abline <- function(
  fig, a = NULL, b = NULL, v = NULL, h = NULL, coef = NULL,
  color = "black", alpha = NULL, width = 1, type = 1,
  legend = NULL, lname = NULL, lgroup = NULL, visible = TRUE,
  ...) {

  validate_fig(fig, "ly_abline")

  args <- sub_names(fig, data = NULL,
    grab(
      color,
      alpha,
      width,
      type,
      legend,
      lname,
      lgroup,
      visible,
      dots = lazy_dots(...),
      null_data = TRUE
    )
  )
  args$params$glyph <- "segment"

  if(missing(color) && !is.null(args$params$line_color)) {
    args$params$color <- NULL
  }

  ## see if any options won't be used and give a message
  check_opts(args$params, "segment", formals = names(formals(ly_abline)))

  args$params <- resolve_line_args(fig, args$params)

  x_axis_type <- "numeric"
  y_axis_type <- "numeric"

  # manage data
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
    if(inherits(h, c("Date", "POSIXt"))) {
      y_axis_type <- "datetime"
      h <- to_epoch(h)
    }
    nn <- length(h)
    x0 <- rep(0, nn)
    y0 <- h
    x1 <- rep(1, nn)
    y1 <- h
  } else if(!is.null(v)) {
    if(inherits(v, c("Date", "POSIXt"))) {
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
    if(length(data$x0[[1]]) == 1) {
      if(data$x0 == "x0")
        return(data)
    } else if(length(data$x0[[1]]) == 0) {
      return(data)
    }
    # unlist because of json encoding issues
    # (json wants each as a list but we want to work with scalars here)
    data <- unlist(data, recursive = FALSE)
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
    # now below wrap each result with list so json encoding is happy
    data$x0 <- list(data$x0)
    data$x1 <- list(data$x1)
    data$y0 <- list(data$y0)
    data$y1 <- list(data$y1)
    data
  }

  axis_type_range <- list(
    x_axis_type = x_axis_type, y_axis_type = y_axis_type,
    x_range = NULL, y_range = NULL)

  mc <- lapply(match.call(), deparse)

  make_glyph(
    fig, type = "segment",
    data = list(x0 = x0, y0 = y0, x1 = x1, y1 = y1, defer = defer_fn),
    legend = args$info$legend,
    lname = args$info$lname, lgroup = args$info$lgroup,
    xname = args$info$x_name, yname = args$info$y_name,
    args = args$params, axis_type_range = axis_type_range,
    ly_call = mc
  )
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
ly_curve <- function(
  fig, expr, from = NULL, to = NULL, n = 101,
  color = "black", alpha = 1, width = 1, type = 1,
  legend = NULL, lname = NULL, lgroup = NULL, visible = TRUE,
  ...
) {

  validate_fig(fig, "ly_curve")

  xname <- "x"
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

  args <- sub_names(fig, data = NULL,
    grab(
      color,
      alpha,
      width,
      type,
      legend,
      lname,
      lgroup,
      visible,
      dots = lazy_dots(...),
      null_data = TRUE
    )
  )

  ## see if any options won't be used and give a message
  check_opts(args$params, "line", formals = names(formals(ly_curve)))

  if(missing(color) && !is.null(args$params$line_color))
    args$params$color <- NULL

  args$params <- resolve_line_args(fig, args$params)

  do.call(ly_lines,
    c(
      list(
        fig = fig,
        x = x, y = y,
        legend = args$info$legend, lname = args$info$lname, lgroup = args$info$lgroup,
        xlab = xname, ylab = yname
      ),
      args$params
    )
  )
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
ly_contour <- function(
  fig, z,
  x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, length.out = ncol(z)),
  nlevels = 10, levels = pretty(range(z, na.rm = TRUE), nlevels),
  color = "black", alpha = 1, width = 1, type = 1,
  lname = NULL, lgroup = NULL, visible = TRUE,
  ...
) {

  validate_fig(fig, "ly_contour")
  ## see if any options won't be used and give a message

  args <- sub_names(fig, data = NULL,
    grab(
      color,
      alpha,
      width,
      type,
      lname,
      lgroup,
      visible,
      dots = lazy_dots(...),
      null_data = TRUE
    )
  )

  args$params <- resolve_line_args(fig, args$params)

  contr <- do.call(grDevices::contourLines,
    list(x = x, y = y, z = z, nlevels = nlevels, levels = levels))

  xs <- lapply(contr, "[[", 2)
  ys <- lapply(contr, "[[", 3)

  check_opts(args$params, "multi_line", formals = names(formals(ly_contour)))

  axis_type_range <- get_glyph_axis_type_range(x, y, assert_x = "numeric", assert_y = "numeric")

  mc <- lapply(match.call(), deparse)

  make_glyph(
    fig, type = "multi_line",
    lname = args$info$lname, lgroup = args$info$lgroup,
    xname = args$info$x_name, yname = args$info$y_name,
    data = list(xs = xs, ys = ys),
    args = args$params, axis_type_range = axis_type_range,
    ly_call = mc
  )
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
ly_ray <- function(
  fig, x, y = NULL, data = figure_data(fig),
  length = NULL, angle = 0,
  color = "black", type = 1, width = 1, alpha = NULL,
  legend = NULL, lname = NULL, lgroup = NULL, visible = TRUE,
  ...
) {

  validate_fig(fig, "ly_ray")

  args <- sub_names(fig, data,
    grab(
      x, y,
      length,
      angle,
      color,
      alpha,
      width,
      type,
      legend,
      lname,
      lgroup,
      visible,
      dots = lazy_dots(...)
    )
  )

  if(missing(color) && !is.null(args$params$line_color)) {
    args$params$color <- NULL
  }

  ## see if any options won't be used and give a message
  check_opts(args$params, "ray", formals = names(formals(ly_ray)))

  args$params <- resolve_line_args(fig, args$params)

  axis_type_range <- get_glyph_axis_type_range(args$data$x, args$data$y)

  mc <- lapply(match.call(), deparse)

  make_glyph(
    fig, type = "ray",
    xname = args$info$x_name, yname = args$info$y_name,
    data = args$data, legend = args$info$legend,
    args = args$params, axis_type_range = axis_type_range,
    lname = args$info$lname, lgroup = args$info$lgroup,
    ly_call = mc
  )
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
ly_bezier <- function(
  fig,
  x0, y0, x1, y1, cx0, cy0, cx1, cy1,
  data = figure_data(fig),
  color = "black", alpha = 1, width = 1, type = 1,
  legend = NULL, lname = NULL, lgroup = NULL, visible = TRUE,
  ...
) {

  validate_fig(fig, "ly_bezier")

  args <- sub_names(fig, data,
    grab(
      x0, y0, x1, y1, cx0, cy0, cx1, cy1,
      color,
      alpha,
      width,
      type,
      legend,
      lname,
      lgroup,
      visible,
      dots = lazy_dots(...)
    )
  )
  args$params$glyph = "bezier"

  if(missing(color) && !is.null(args$params$line_color)) {
    args$params$color <- NULL
  }

  ## see if any options won't be used and give a message
  check_opts(args$params, "bezier", formals = names(formals(ly_bezier)))

  args$params <- resolve_line_args(fig, args$params)

  axis_type_range <- get_glyph_axis_type_range(
    c(args$data$x0, args$data$x1),
    c(args$data$y0, args$data$y1),
    assert_x = "numeric", assert_y = "numeric"
  )

  mc <- lapply(match.call(), deparse)

  make_glyph(
    fig, type = "bezier",
    lname = args$info$lname, lgroup = args$info$lgroup,
    xname = args$info$x_name, yname = args$info$y_name,
    data = args$data,
    args = args$params, axis_type_range = axis_type_range,
    ly_call = mc
  )
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
ly_quadratic <- function(
  fig,
  x0, y0, x1, y1, cx, cy,
  data = figure_data(fig),
  color = "black", alpha = 1, width = 1, type = 1,
  legend = NULL, lname = NULL, lgroup = NULL, visible = TRUE,
  ...
) {

  validate_fig(fig, "ly_quadratic")

  args <- sub_names(fig, data,
    grab(
      x0, y0, x1, y1, cx, cy,
      color,
      alpha,
      width,
      type,
      legend,
      lname,
      lgroup,
      visible,
      dots = lazy_dots(...)
    )
  )
  args$params$glyph <- "quadratic"

  if(missing(color) && !is.null(args$params$line_color)) {
    args$color <- NULL
  }

  ## see if any options won't be used and give a message
  check_opts(args$params, "quadratic", formals = names(formals(ly_quadratic)))

  args$params <- resolve_line_args(fig, args$params)

  axis_type_range <- get_glyph_axis_type_range(
    c(args$data$x0, args$data$x1),
    c(args$data$y0, args$data$y1),
    assert_x = "numeric", assert_y = "numeric"
  )

  mc <- lapply(match.call(), deparse)

  make_glyph(
    fig, type = "quadratic",
    lname = args$info$lname, lgroup = args$info$lgroup,
    xname = args$info$x_name, yname = args$info$y_name,
    data = args$data,
    args = args$params, axis_type_range = axis_type_range,
    ly_call = mc
  )
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
ly_multi_line <- function(
  fig,
  xs, ys,
  color = "black", alpha = 1, width = 1, type = 1,
  lname = NULL, lgroup = NULL, visible = TRUE,
  ...
) {

  validate_fig(fig, "ly_multi_line")

  args <- sub_names(fig, data = NULL,
    grab(
      xs, ys,
      color,
      alpha,
      width,
      type,
      # no legend?
      lname,
      lgroup,
      visible,
      dots = lazy_dots(...)
    )
  )
  args$params$glyph <- "line"

  if(missing(color) && !is.null(args$params$line_color)) {
    args$color <- NULL
  }

  ## see if any options won't be used and give a message
  # can't pass in color, alpha, width, or type
  good_names = names(args$params)
  good_names = good_names[! (good_names %in% c("color", "alpha", "width", "type"))]
  check_opts(args$params[good_names], "multi_line")

  args$params <- resolve_line_args(fig, args$params)

  axis_type_range <- get_glyph_axis_type_range(unlist(args$data$xs), unlist(args$data$ys))

  mc <- lapply(match.call(), deparse)

  make_glyph(
    fig, type = "multi_line",
    data = args$data, args = args$params,
    xname = args$info$x_name, yname = args$info$y_name,
    lname = args$info$lname, lgroup = args$info$lgroup,
    axis_type_range = axis_type_range,
    ly_call = mc
  )
}
