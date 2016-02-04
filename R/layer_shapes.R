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
ly_polygons <- function(
  fig, xs, ys, group = NULL, data = figure_data(fig),
  color = NULL, alpha = 1,
  hover = NULL, url = NULL, # legend = NULL,
  lname = NULL, lgroup = NULL, visible = TRUE, ...
) {

  validate_fig(fig, "ly_polygons")

  args <- sub_names(fig, data,
    grab(
      xs,
      ys,
      group,
      color,
      alpha,
      hover,
      url,
      lname,
      lgroup,
      visible,
      dots = lazy_dots(...)
    )
  )

  # pull out manually, as they are repeatedly customized
  xs <- args$data$xs
  ys <- args$data$ys
  group <- args$info$group

  if(missing(alpha)) {
    args$params$alpha <- NULL
  }

  if(!is.null(group)) {
    if(is.factor(group)) {
      group <- as.character(group)
    }
    idx <- unname(split(seq_along(group), group))
    xs <- lapply(idx, function(x) xs[x])
    ys <- lapply(idx, function(x) ys[x])

    # data for hover and url will only be one row for each group
    data <- data[sapply(idx, "[", 1),]

    ns <- lapply(args$params, length)
    bad_ind <- which(!ns %in% c(0, 1, length(idx), length(group)))
    if(length(bad_ind) > 0) {
      message("The following arguments do not have length the same as the number of groups or the total number of observations for ly_polygons() and will be ignored: ", paste(names(args$params[bad_ind]), collapse = ", "))
      args$params[bad_ind] <- NULL
    }

    full_length <- which(ns == length(group))
    for(ii in full_length) {
      args$params[[ii]] <- sapply(idx, function(x) args$params[[ii]][x[1]])
    }
  }

  ## translate different x, y types to vectors
  if(is.atomic(xs) && !is.list(xs)) {
    xs <- list(xs)
  }

  if(is.atomic(ys) && !is.list(ys)) {
    ys <- list(ys)
  }

  if(!(is.list(xs) && is.list(ys))) {
    stop("For ly_polygons, xs and ys must be lists or specified through a data frame through 'data' argument.")
  }

  args$params <- resolve_color_alpha(args$params, has_line = TRUE, has_fill = TRUE, fig$x$spec$layers[[args$info$lgroup]], theme = fig$x$spec$theme)

  ## see if any options won't be used and give a message
  check_opts(args$params, "patches", formals = names(formals(ly_polygons)))

  if(is.null(args$params$fill_alpha)) {
    args$params$fill_alpha <- 0.5
  }

  axis_type_range <- get_glyph_axis_type_range(unlist(xs), unlist(ys))

  mc <- lapply(match.call(), deparse)

  make_glyph(
    fig, type = "patches", data = list(xs = unname(xs), ys = unname(ys)),
    args = args$params, axis_type_range = axis_type_range,
    xname = args$info$x_name, yname = args$info$y_name,
    lname = args$info$lname, lgroup = args$info$lgroup, hover = args$info$hover, url = args$info$url,
    ly_call = mc
  )
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
ly_rect <- function(
  fig,
  xleft, ybottom, xright, ytop,
  data = figure_data(fig),
  color = NULL, alpha = 1,
  hover = NULL, url = NULL, legend = NULL,
  lname = NULL, lgroup = NULL, visible = TRUE, ...
) {

  validate_fig(fig, "ly_rect")

  args <- sub_names(fig, data,
    grab(
      xleft,
      ybottom,
      xright,
      ytop,
      color,
      alpha,
      hover,
      url,
      lname,
      lgroup,
      visible,
      dots = lazy_dots(...)
    )
  )


  if (missing(alpha)) {
    args$params$alpha <- NULL
  }

  args$params <- resolve_color_alpha(args$params, has_line = TRUE, has_fill = TRUE, fig$x$spec$layers[[args$info$lgroup]], theme = fig$x$spec$theme)

  ## see if any options won't be used and give a message
  check_opts(args$params, "quad", formals = names(formals(ly_rect)))

  if(is.null(args$params$fill_alpha)) {
    args$params$fill_alpha <- 0.5
  }

  axis_type_range <- get_glyph_axis_type_range(
    c(args$data$xleft, args$data$xright),
    c(args$data$ybottom, args$data$ytop)
  )

  mc <- lapply(match.call(), deparse)

  make_glyph(
    fig, type = "quad", lname = args$info$lname, lgroup = args$info$lgroup,
    xname = args$info$x_name, yname = args$info$y_name,
    legend = args$info$legend, hover = args$info$hover, url = args$info$url,
    data = list(
      left = args$data$xleft,
      right = args$data$xright,
      top = args$data$ytop,
      bottom = args$data$ybottom
    ),
    data_sig = ifelse(is.null(data), NA, digest(data)),
    args = args$params, axis_type_range = axis_type_range,
    ly_call = mc
  )
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
ly_crect <- function(
  fig, x, y = NULL, data = figure_data(fig),
  width = 1, height = 1, angle = 0, dilate = FALSE,
  color = NULL, alpha = 1,
  hover = NULL, url = NULL, legend = NULL,
  lname = NULL, lgroup = NULL, visible = TRUE, ...) {

  validate_fig(fig, "ly_crect")

  args <- sub_names(fig, data,
    grab(
      x,
      y,
      width,
      height,
      angle,
      dilate,
      color,
      alpha,
      hover,
      url,
      lname,
      lgroup,
      visible,
      dots = lazy_dots(...)
    )
  )
  args$info$glyph <- "rect"

  if(missing(alpha)) {
    args$info$alpha <- NULL
  }

  args$params <- resolve_color_alpha(args$params, has_line = TRUE, has_fill = TRUE, fig$x$spec$layers[[args$info$lgroup]], theme = fig$x$spec$theme)

  ## see if any options won't be used and give a message
  check_opts(args$params, "rect", formals = names(formals(ly_crect)))

  if(is.null(args$params$fill_alpha)) {
    args$params$fill_alpha <- 0.5
  }

  xr <- args$data$x
  if(is.numeric(xr)) {
    xr <- c(xr - width / 2, xr + width / 2)
  }
  yr <- args$data$y
  if(is.numeric(yr)) {
    yr <- c(yr - height / 2, yr + height / 2)
  }

  axis_type_range <- get_glyph_axis_type_range(xr, yr)

  mc <- lapply(match.call(), deparse)

  make_glyph(
    fig, type = "rect", lname = lname, lgroup = lgroup,
    xname = args$info$x_name, yname = args$info$y_name,
    legend = args$info$legend, hover = args$info$hover, url = args$info$url,
    data_sig = ifelse(is.null(data), NA, digest(data)),
    data = args$data, args = args$params, axis_type_range = axis_type_range,
    ly_call = mc
  )
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
ly_oval <- function(
  fig, x, y = NULL, data = figure_data(fig),
  width = 0.1, height = 0.1, angle = 0,
  color = NULL, alpha = 1,
  legend = NULL, lname = NULL, lgroup = NULL, visible = TRUE,
  ...
) {

  validate_fig(fig, "ly_oval")

  args <- sub_names(fig, data,
    grab(
      x,
      y,
      width,
      height,
      angle,
      color,
      alpha,
      # hover, # no hover
      # url, # no url
      legend,
      lname,
      lgroup,
      visible,
      dots = lazy_dots(...)
    )
  )
  args$info$glyph <- "oval"

  if(missing(alpha)) {
    args$params$alpha <- NULL
  }

  args$params <- resolve_color_alpha(args$params, has_line = TRUE, has_fill = TRUE, fig$x$spec$layers[[args$info$lgroup]], theme = fig$x$spec$theme)

  ## see if any options won't be used and give a message
  check_opts(args$params, "oval", formals = names(formals(ly_oval)))

  axis_type_range <- get_glyph_axis_type_range(args$data$x, args$data$y)

  mc <- lapply(match.call(), deparse)

  make_glyph(
    fig, type = "oval", lname = args$info$lname, lgroup = args$info$lgroup,
    data = args$data, data_sig = ifelse(is.null(data), NA, digest(data)),
    args = args$params, axis_type_range = axis_type_range,
    hover = args$info$hover, url = args$info$url, legend = args$info$legend,
    xname = args$info$x_name, yname = args$info$y_name,
    ly_call = mc
  )
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
ly_patch <- function(
  fig, x, y, data = figure_data(fig),
  color = NULL, alpha = 1,
  hover = NULL, url = NULL,
  legend = NULL, lname = NULL, lgroup = NULL, visible = TRUE,
  ...) {

  validate_fig(fig, "ly_patch")

  args <- sub_names(fig, data,
    grab(
      x,
      y,
      color,
      alpha,
      hover,
      url,
      legend,
      lname,
      lgroup,
      visible,
      dots = lazy_dots(...)
    )
  )
  args$info$glyph <- "patch"

  if(missing(alpha)) {
    args$params$alpha <- NULL
  }

  args$params <- resolve_color_alpha(args$params, has_line = TRUE, has_fill = TRUE, fig$x$spec$layers[[args$info$lgroup]], theme = fig$x$spec$theme)

  ## see if any options won't be used and give a message
  check_opts(args, "patch", formals = names(formals(ly_patch)))

  axis_type_range <- get_glyph_axis_type_range(args$data$x, args$data$y)

  mc <- lapply(match.call(), deparse)

  make_glyph(
    fig, type = "patch", data = args$data, args = args$params,
    legend = args$info$legend, hover = args$info$hover, url = args$info$url,
    lname = args$info$lname, lgroup = args$info$lgroup,
    axis_type_range = axis_type_range, ly_call = mc
  )
}
