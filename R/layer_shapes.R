#' Add a "polygons" layer to a Bokeh figure
#' @param fig figure to modify
#' @param xs vector or list of values or field name of polygon x coordinates - see details
#' @param ys vector or list of values or field name of polygon y coordinates - see details
#' @param group vector or field name of grouping variable - see details
#' @param data an optional data frame, providing the source for inputs xs, ys, group, and other glyph properties
#' @details \code{xs} and \code{ys} can be a list of vectors, each element for one polygon to be drawn, or can be vectors with the \code{group} argument specifying how to break them up into individual polygons.
#' @template par-coloralpha
#' @template par-hover
#' @template par-url
#' @template par-lnamegroup
#' @template dots-fillline
#' @family layer functions
#' @export
ly_polygons <- function(fig, xs, ys, group = NULL, data = NULL,
  color = NULL, alpha = 1,
  hover = NULL, url = NULL, # legend = NULL,
  lname = NULL, lgroup = NULL, ...) {

  validate_fig(fig, "ly_polygons")

  xname <- deparse(substitute(xs))
  yname <- deparse(substitute(ys))

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    dots  <- substitute(list(...))[-1]
    args  <- lapply(dots, function(x) v_eval(x, data))
    xs    <- v_eval(substitute(xs), data)
    ys    <- v_eval(substitute(ys), data)
    group <- v_eval(substitute(group), data)
    color <- v_eval(substitute(color), data)
  } else {
    args <- list(...)
  }

  args$color <- color
  args$alpha <- alpha
  if(missing(alpha))
    args$alpha <- NULL

  if(!is.null(group)) {
    idx <- unname(split(seq_along(group), group))
    xs <- lapply(idx, function(x) xs[x])
    ys <- lapply(idx, function(x) ys[x])

    ns <- lapply(args, length)
    bad_ind <- which(!ns %in% c(0, 1, length(idx), length(group)))
    if(length(bad_ind) > 0) {
      message("The following arguments do not have length the same as the number of groups or the total number of observations for ly_polygons() and will be ignored: ", paste(names(args[bad_ind]), collapse = ", "))
      args[bad_ind] <- NULL
    }

    full_length <- which(ns == length(group))
    for(ii in full_length) {
      args[[ii]] <- sapply(idx, function(x) args[[ii]][x[1]])
    }
  }

  # hover <- get_hover(substitute(hover), data)
  xy_names <- get_xy_names(xs, ys, xname, yname, args)
  ## translate different x, y types to vectors
  lgroup <- get_lgroup(lgroup, fig)

  if(is.atomic(xs) && !is.list(xs))
    xs <- list(xs)

  if(is.atomic(ys) && !is.list(ys))
    ys <- list(ys)

  if(!(is.list(xs) && is.list(ys))) {
    stop("For ly_polygons, xs and ys must be lists or specified through a data frame through 'data' argument.")
  }

  args <- resolve_color_alpha(args, has_line = TRUE, has_fill = TRUE, fig$x$spec$layers[[lgroup]])

  ## see if any options won't be used and give a message
  check_opts(args, "patches", formals = names(formals(ly_polygons)))

  if(is.null(args$fill_alpha))
    args$fill_alpha <- 0.5

  hover <- get_hover(hover)
  url <- get_url(url, data)

  axis_type_range <- get_glyph_axis_type_range(unlist(xs), unlist(ys))
  make_glyph(fig, type = "patches", data = list(xs = unname(xs), ys = unname(ys)),
    args = args, axis_type_range = axis_type_range, xname = xy_names$x, yname = xy_names$y,
    lname = lname, lgroup = lgroup, hover = hover, url = url)
}

#' Add a "rect" layer to a Bokeh figure
#' @param fig figure to modify
#' @param xleft values or field name of left edges
#' @param ybottom values or field name of bottom edges
#' @param xright values or field name of right edges
#' @param ytop values or field name of top edges
#' @param data an optional data frame, providing the source for inputs xleft, ybottom, xright, ytop, and other glyph properties
#' @template par-coloralpha
#' @template par-hover
#' @template par-url
#' @template par-legend
#' @template par-lnamegroup
#' @template dots-fillline
#' @family layer functions
#' @export
ly_rect <- function(fig, xleft, ybottom, xright, ytop, data = NULL,
  color = NULL, alpha = 1,
  hover = NULL, url = NULL, legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validate_fig(fig, "ly_rect")

  xname <- deparse(substitute(xleft))
  yname <- deparse(substitute(ybottom))

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    dots    <- substitute(list(...))[-1]
    args    <- lapply(dots, function(x) v_eval(x, data))
    xleft   <- v_eval(substitute(xleft), data)
    xright  <- v_eval(substitute(xright), data)
    ybottom <- v_eval(substitute(ybottom), data)
    ytop    <- v_eval(substitute(ytop), data)
    color   <- v_eval(substitute(color), data)
  } else {
    args <- list(...)
  }

  hover <- get_hover(substitute(hover), data)
  url <- get_url(url, data)

  xy_names <- get_xy_names(xleft, ybottom, xname, yname, args)

  lgroup <- get_lgroup(lgroup, fig)

  args <- c(args, list(color = color, alpha = alpha))
  if(missing(alpha))
    args$alpha <- NULL

  args <- resolve_color_alpha(args, has_line = TRUE, has_fill = TRUE, fig$x$spec$layers[[lgroup]])

  ## see if any options won't be used and give a message
  check_opts(args, "quad", formals = names(formals(ly_rect)))

  if(is.null(args$fill_alpha))
    args$fill_alpha <- 0.5

  axis_type_range <- get_glyph_axis_type_range(c(xleft, xright), c(ybottom, ytop))
  make_glyph(fig, type = "quad", lname = lname, lgroup = lgroup,
    xname = xy_names$x, yname = xy_names$y,
    legend = legend, hover = hover, url = url,
    data = list(left = xleft, right = xright, top = ytop, bottom = ybottom),
    data_sig = ifelse(is.null(data), NA, digest(data)),
    args = args, axis_type_range = axis_type_range)
}

#' Add a "crect" (centered rectangle) layer to a Bokeh figure
#' @param fig figure to modify
#' @param x values or field name of center x coordinates
#' @param y values or field name of center y coordinates
#' @param data an optional data frame, providing the source for inputs xleft, ybottom, xright, ytop, and other glyph properties
#' @param width values or field name of widths
#' @param height values or field name of heights
#' @param angle values or field name of rotation angles
#' @param dilate logical - whether to dilate pixel distance computations when drawing
#' @template par-coloralpha
#' @template par-hover
#' @template par-url
#' @template par-legend
#' @template par-lnamegroup
#' @template dots-fillline
#' @example man-roxygen/ex-elements.R
#' @family layer functions
#' @export
ly_crect <- function(fig, x, y = NULL, data = NULL,
  width = 1, height = 1, angle = 0, dilate = FALSE,
  color = NULL, alpha = 1,
  hover = NULL, url = NULL, legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validate_fig(fig, "ly_crect")

  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    dots   <- substitute(list(...))[-1]
    args   <- lapply(dots, function(x) v_eval(x, data))
    x      <- v_eval(substitute(x), data)
    y      <- v_eval(substitute(y), data)
    width  <- v_eval(substitute(width), data)
    height <- v_eval(substitute(height), data)
    angle  <- v_eval(substitute(angle), data)
    color  <- v_eval(substitute(color), data)
  } else {
    args <- list(...)
  }

  hover <- get_hover(substitute(hover), data)
  url <- get_url(url, data)
  xy_names <- get_xy_names(x, y, xname, yname, args)
  ## translate different x, y types to vectors
  xy <- get_xy_data(x, y)
  lgroup <- get_lgroup(lgroup, fig)

  args <- c(args, list(glyph = "rect", color = color, alpha = alpha,
    width = width, height = height, angle = angle, dilate = dilate))
  if(missing(alpha))
    args$alpha <- NULL

  args <- resolve_color_alpha(args, has_line = TRUE, has_fill = TRUE, fig$x$spec$layers[[lgroup]])

  ## see if any options won't be used and give a message
  check_opts(args, "rect", formals = names(formals(ly_crect)))

  if(is.null(args$fill_alpha))
    args$fill_alpha <- 0.5

  xr <- xy$x
  if(is.numeric(xy$x)) {
    xr <- c(xy$x - width / 2, xy$x + width / 2)
  }
  yr <- xy$y
  if(is.numeric(xy$y)) {
    yr <- c(xy$y - height / 2, xy$y + height / 2)
  }

  axis_type_range <- get_glyph_axis_type_range(xr, yr)

  make_glyph(fig, type = "rect", lname = lname, lgroup = lgroup,
    xname = xy_names$x, yname = xy_names$y,
    legend = legend, hover = hover, url = url,
    data_sig = ifelse(is.null(data), NA, digest(data)),
    data = xy, args = args, axis_type_range = axis_type_range)
}

#' Add an "oval" layer to a Bokeh figure
#' @param fig figure to modify
#' @param x values or field name of center x coordinates
#' @param y values or field name of center y coordinates
#' @param data an optional data frame, providing the source for inputs x, y, and other glyph properties
#' @param width values or field name of widths
#' @param height values or field name of heights
#' @param angle values or field name of rotation angles
#' @template par-coloralpha
#' @template par-legend
#' @template par-lnamegroup
#' @template dots-fillline
#' @family layer functions
#' @export
ly_oval <- function(fig, x, y = NULL, data = NULL,
  width = 0.1, height = 0.1, angle = 0,
  color = NULL, alpha = 1,
  legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validate_fig(fig, "ly_oval")

  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    dots   <- substitute(list(...))[-1]
    args   <- lapply(dots, function(x) v_eval(x, data))
    x      <- v_eval(substitute(x), data)
    y      <- v_eval(substitute(y), data)
    color  <- v_eval(substitute(color), data)
    width  <- v_eval(substitute(width), data)
    height <- v_eval(substitute(height), data)
    angle  <- v_eval(substitute(angle), data)
  } else {
    args <- list(...)
  }

  hover <- get_hover(substitute(hover), data)
  url <- get_url(url, data)
  xy_names <- get_xy_names(x, y, xname, yname, args)
  ## translate different x, y types to vectors
  xy <- get_xy_data(x, y)
  lgroup <- get_lgroup(lgroup, fig)

  args <- c(args, list(glyph = "oval", color = color, alpha = alpha,
    width = width, height = height, angle = angle))
  if(missing(alpha))
    args$alpha <- NULL

  args <- resolve_color_alpha(args, has_line = TRUE, has_fill = TRUE, fig$x$spec$layers[[lgroup]])

  ## see if any options won't be used and give a message
  check_opts(args, "oval", formals = names(formals(ly_oval)))

  axis_type_range <- get_glyph_axis_type_range(x, y)

  make_glyph(fig, type = "oval", lname = lname, lgroup = lgroup,
    data = xy, data_sig = ifelse(is.null(data), NA, digest(data)),
    args = args, axis_type_range = axis_type_range,
    hover = hover, url = url, legend = legend,
    xname = xy_names$x, yname = xy_names$y)
}

#' Add a "patch" layer to a Bokeh figure
#' @param fig figure to modify
#' @param x values or field name of patch x coordinates
#' @param y values or field name of patch y coordinates
#' @param data an optional data frame, providing the source for inputs x, y, and other glyph properties
#' @template par-coloralpha
#' @template par-hover
#' @template par-url
#' @template par-legend
#' @template par-lnamegroup
#' @template dots-fillline
#' @family layer functions
#' @note This function is included for completeness as it maps to Bokeh's \code{patch} glyph, but the same and more functionality can be obtained with \code{\link{ly_polygons}}.
#' @export
ly_patch <- function(fig, x, y, data = NULL,
  color = NULL, alpha = 1,
  hover = NULL, url = NULL, legend = NULL, lname = NULL, lgroup = NULL, ...) {

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
  url <- get_url(url, data)
  xy_names <- get_xy_names(x, y, xname, yname, args)
  ## translate different x, y types to vectors
  xy <- get_xy_data(x, y)
  lgroup <- get_lgroup(lgroup, fig)

  args <- c(args, list(color = color, alpha = alpha))
  if(missing(alpha))
    args$alpha <- NULL

  args <- resolve_color_alpha(args, has_line = TRUE, has_fill = TRUE, fig$x$spec$layers[[lgroup]])

  ## see if any options won't be used and give a message
  check_opts(args, "patch", formals = names(formals(ly_patch)))

  axis_type_range <- get_glyph_axis_type_range(x, y)

  make_glyph(fig, type = "patch", data = xy, args = args,
    legend = legend, hover = hover, url = url,
    lname = lname, lgroup = lgroup,
    axis_type_range = axis_type_range)
}
