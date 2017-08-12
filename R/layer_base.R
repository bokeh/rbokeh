#' Add a "points" layer to a Bokeh figure
#' Draws points with the given coordinates.
#' @param fig figure to modify
#' @param x values or field name of line x coordinates
#' @param y values or field name of line y coordinates
#' @param data an optional data frame, providing the source for inputs x, y, and other glyph properties
# @template par-lineprops
# @template par-legend
# @template par-lnamegroup
# @template dots-line
# @example man-roxygen/ex-lines.R
#' @family layer functions
#' @export
ly_points <- function(fig, x = NULL, y = NULL, data = figure_data(fig),
  glyph = 21, color = NULL, alpha = NULL, size = NULL,
  hover = NULL, legend = TRUE,
  hov_color = NULL, hov_alpha = NULL, ns_color = NULL, ns_alpha = NULL,
  lgroup = NULL, lname = NULL, ...) {

  # TODO: check glyph to see if it varies and figure out how to handle that

  # wait until print time to resolve everything
  # because data sources and themes can change
  # so just store quosures
  # TODO: should still validate that ... only contains valid parameter names
  spec <- c(list(
    x = enquo(x), y = enquo(y), color = enquo(color), alpha = enquo(alpha),
    size = enquo(size), hov_color = enquo(hov_color), hov_alpha = enquo(hov_alpha),
    ns_color = enquo(ns_color), ns_alpha = enquo(ns_alpha), hover = enquo(hover),
    model = glyph,
    type = "glyph",
    data = data,
    legend = legend,
    use_all_data = FALSE), # for now (TODO)
    quos(...))

  add_layer(fig, spec, lgroup, lname)
}

#' Add a "lines" layer to a Bokeh figure
#' Draws lines with the given coordinates.
#' @param fig figure to modify
#' @param x values or field name of line x coordinates
#' @param y values or field name of line y coordinates
#' @param data an optional data frame, providing the source for inputs x, y, and other glyph properties
#' @param group values or field name of a grouping variable to break lines up by
# @template par-lineprops
# @template par-legend
# @template par-lnamegroup
# @template dots-line
# @example man-roxygen/ex-lines.R
#' @family layer functions
#' @export
ly_lines <- function(fig, x = NULL, y = NULL, data = figure_data(fig),
  type = 1, width = 1, color = NULL, alpha = NULL,
  hover = NULL, legend = TRUE,
  hov_color = NULL, hov_alpha = NULL, ns_color = NULL, ns_alpha = NULL,
  lgroup = NULL, lname = NULL, ...) {

  spec <- c(list(
    x = enquo(x), y = enquo(y), color = enquo(color), alpha = enquo(alpha),
    hov_color = enquo(hov_color), hov_alpha = enquo(hov_alpha),
    ns_color = enquo(ns_color), ns_alpha = enquo(ns_alpha), hover = enquo(hover),
    line_width = enquo(width), line_dash = enquo(type),
    model = "Line",
    type = "glyph",
    data = data,
    legend = legend,
    use_all_data = FALSE), # for now (TODO)
    quos(...))

  add_layer(fig, spec, lgroup, lname)
}


#' Add a "text" layer to a Bokeh figure
#' @param fig figure to modify
#' @param x x coordinates of text anchors
#' @param y y coordinates of text anchors
#' @param text text values to render
#' @param data an optional data frame, providing the source for inputs x, y, text, and other glyph properties
#' @param color text color values for the text
#' @param alpha text alpha values for the text
#' @param angle angle to rotate the text in radians
#' @param align text align values for the text ("left", "right", "center")
#' @param baseline text baseline values for the text ("top", "middle", "bottom", "alphabetic", "hanging")
#' @param font text font values for the text
#' @param font_size text font size values for the text
#' @param font_style text font style values for the text ("normal", "italic", "bold")
#' @param x_offset offset values to apply to the x-coordinates
#' @param y_offset offset values to apply to the y-coordinates
# template par-legend
# template par-lnamegroup
# example man-roxygen/ex-elements.R
#' @family layer functions
#' @export
ly_text <- function(fig,
  x, y = NULL, text = NULL, color = NULL, alpha = NULL, angle = NULL,
  data = figure_data(fig),
  x_offset = NULL, y_offset = NULL,
  font = NULL, font_size = NULL, font_style = NULL,
  align = NULL, baseline = NULL,
  hov_color = NULL, hov_alpha = NULL, ns_color = NULL, ns_alpha = NULL,
  lgroup = NULL, lname = NULL, ...) {

  # TODO: does this need a legend??

  spec <- c(list(
    x = enquo(x), y = enquo(y), text = enquo(text),
    text_color = enquo(color), text_alpha = enquo(alpha), angle = enquo(angle),
    x_offset = enquo(x_offset), y_offset = enquo(y_offset),
    text_font = font, text_font_size = enquo(font_size),
    text_font_style = enquo(font_style),
    text_align = enquo(align), text_baseline = enquo(baseline),
    hov_color = enquo(hov_color), hov_alpha = enquo(hov_alpha),
    ns_color = enquo(ns_color), ns_alpha = enquo(ns_alpha),
    model = "Text",
    type = "glyph",
    data = data,
    legend = legend,
    use_all_data = FALSE), # for now (TODO)
    quos(...))

  add_layer(fig, spec, lgroup, lname)
}

#' Add a "rect" layer to a Bokeh figure
#' @param fig figure to modify
#' @param xleft values or field name of left edges
#' @param ybottom values or field name of bottom edges
#' @param xright values or field name of right edges
#' @param ytop values or field name of top edges
#' @param data an optional data frame, providing the source for inputs xleft, ybottom, xright, ytop, and other glyph properties
# template par-coloralpha
# template par-hover
# template par-url
# template par-legend
# template par-lnamegroup
# template dots-fillline
#' @family layer functions
#' @export
ly_rect <- function(
  fig,
  xleft, ybottom, xright, ytop,
  data = figure_data(fig),
  color = NULL, alpha = NULL,
  hov_color = NULL, hov_alpha = NULL,
  ns_color = NULL, ns_alpha = NULL,
  sel_color = NULL, sel_alpha = NULL,
  hover = NULL, url = NULL,
  legend = TRUE,
  lname = NULL, lgroup = NULL,
  ...
) {
  spec <- c(list(
    left = enquo(xleft), bottom = enquo(ybottom),
    right = enquo(xright), top = enquo(ytop),
    color = enquo(color), alpha = enquo(alpha),
    hov_color = enquo(hov_color), hov_alpha = enquo(hov_alpha),
    ns_color = enquo(ns_color), ns_alpha = enquo(ns_alpha),
    sel_color = enquo(sel_color), sel_alpha = enquo(sel_alpha),
    hover = enquo(hover), url = enquo(url),
    model = "Quad",
    type = "glyph",
    data = data,
    legend = legend,
    use_all_data = FALSE), # for now (TODO)
    quos(...))

  add_layer(fig, spec, lgroup, lname)
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
# template par-coloralpha
# template par-hover
# template par-url
# template par-legend
# template par-lnamegroup
# template dots-fillline
# example man-roxygen/ex-elements.R
#' @family layer functions
#' @export
ly_crect <- function(
  fig,
  x, y = NULL, width = 1, height = 1, angle = 0,
  data = figure_data(fig),
  color = NULL, alpha = NULL,
  hov_color = NULL, hov_alpha = NULL,
  ns_color = NULL, ns_alpha = NULL,
  sel_color = NULL, sel_alpha = NULL,
  hover = NULL, url = NULL,
  legend = TRUE,
  lname = NULL, lgroup = NULL,
  ...
) {
  spec <- c(list(
    x = enquo(x), y = enquo(y), width = enquo(width), height = enquo(height),
    angle = enquo(angle),
    color = enquo(color), alpha = enquo(alpha),
    hov_color = enquo(hov_color), hov_alpha = enquo(hov_alpha),
    ns_color = enquo(ns_color), ns_alpha = enquo(ns_alpha),
    sel_color = enquo(sel_color), sel_alpha = enquo(sel_alpha),
    hover = enquo(hover), url = enquo(url),
    model = "Rect",
    type = "glyph",
    data = data,
    legend = legend,
    use_all_data = FALSE),
    quos(...))

  add_layer(fig, spec, lgroup, lname)
}

# note: width_units, height_units, angle_units show up in R6 model
# but they are not supported in BokehJS...

#' Add an "oval" layer to a Bokeh figure
#' @param fig figure to modify
#' @param x values or field name of center x coordinates
#' @param y values or field name of center y coordinates
#' @param data an optional data frame, providing the source for inputs x, y, and other glyph properties
#' @param width values or field name of widths
#' @param height values or field name of heights
#' @param angle values or field name of rotation angles
# template par-coloralpha
# template par-legend
# template par-lnamegroup
# template dots-fillline
#' @family layer functions
#' @export
ly_oval <- function(
  fig,
  x, y = NULL, width = 1, height = 1, angle = 0,
  data = figure_data(fig),
  color = NULL, alpha = NULL,
  hov_color = NULL, hov_alpha = NULL,
  ns_color = NULL, ns_alpha = NULL,
  sel_color = NULL, sel_alpha = NULL,
  hover = NULL, url = NULL,
  legend = TRUE,
  lname = NULL, lgroup = NULL,
  ...
) {
  spec <- c(list(
    x = enquo(x), y = enquo(y), width = enquo(width), height = enquo(height),
    angle = enquo(angle),
    color = enquo(color), alpha = enquo(alpha),
    hov_color = enquo(hov_color), hov_alpha = enquo(hov_alpha),
    ns_color = enquo(ns_color), ns_alpha = enquo(ns_alpha),
    sel_color = enquo(sel_color), sel_alpha = enquo(sel_alpha),
    hover = enquo(hover), url = enquo(url),
    model = "Oval",
    type = "glyph",
    data = data,
    legend = legend,
    use_all_data = FALSE),
    quos(...))

  add_layer(fig, spec, lgroup, lname)
}

#' Add a "patch" layer to a Bokeh figure
#' @param fig figure to modify
#' @param x values or field name of patch x coordinates
#' @param y values or field name of patch y coordinates
#' @param data an optional data frame, providing the source for inputs x, y, and other glyph properties
# template par-coloralpha
# template par-hover
# template par-url
# template par-legend
# template par-lnamegroup
# @template dots-fillline
#' @family layer functions
#' @note This function is included for completeness as it maps to Bokeh's \code{patch} glyph, but the same and more functionality can be obtained with \code{\link{ly_polygons}}.
#' @export
ly_patch <- function(
  fig,
  x, y = NULL, data = figure_data(fig),
  color = NULL, alpha = NULL,
  hov_color = NULL, hov_alpha = NULL,
  ns_color = NULL, ns_alpha = NULL,
  sel_color = NULL, sel_alpha = NULL,
  hover = NULL, url = NULL,
  legend = TRUE,
  lname = NULL, lgroup = NULL,
  ...
) {

  spec <- c(list(
    x = enquo(x), y = enquo(y),
    color = enquo(color), alpha = enquo(alpha),
    hov_color = enquo(hov_color), hov_alpha = enquo(hov_alpha),
    ns_color = enquo(ns_color), ns_alpha = enquo(ns_alpha),
    sel_color = enquo(sel_color), sel_alpha = enquo(sel_alpha),
    hover = enquo(hover), url = enquo(url),
    model = "Patch",
    type = "glyph",
    data = data,
    legend = legend,
    use_all_data = FALSE),
    quos(...))

  add_layer(fig, spec, lgroup, lname)
}


# #' Add a "polygons" layer to a Bokeh figure
# #' @param fig figure to modify
# #' @param xs vector or list of values or field name of polygon x coordinates - see details
# #' @param ys vector or list of values or field name of polygon y coordinates - see details
# #' @param group vector or field name of grouping variable - see details
# #' @param data an optional data frame, providing the source for inputs xs, ys, group, and other glyph properties
# #' @details \code{xs} and \code{ys} can be a list of vectors, each element for one polygon to be drawn, or can be vectors with the \code{group} argument specifying how to break them up into individual polygons.
# # template par-coloralpha
# # template par-hover
# # template par-url
# # template par-lnamegroup
# # template dots-fillline
# #' @family layer functions
# #' @export
# ly_polygons <- function(
#   fig, xs, ys, group = NULL, data = figure_data(fig),
#   color = NULL, alpha = 1,
#   hover = NULL, url = NULL, # legend = NULL,
#   lname = NULL, lgroup = NULL, visible = TRUE, ...
# ) {

#   validate_fig(fig, "ly_polygons")

#   args <- sub_names(fig, data,
#     grab(
#       xs,
#       ys,
#       group,
#       color,
#       alpha,
#       hover,
#       url,
#       lname,
#       lgroup,
#       visible,
#       dots = lazy_dots(...)
#     )
#   )

#   # pull out manually, as they are repeatedly customized
#   xs <- args$data$xs
#   ys <- args$data$ys
#   group <- args$info$group

#   if (missing(alpha)) {
#     args$params$alpha <- NULL
#   }

#   # if color is not a valid color then we want to group on it too
#   if (needs_map_fns[["color"]](args$params$color))
#     group <- args$params$color

#   if (!is.null(group)) {
#     if (is.factor(group)) {
#       group <- as.character(group)
#     }
#     idx <- unname(split(seq_along(group), group))
#     xs <- lapply(idx, function(x) xs[x])
#     ys <- lapply(idx, function(x) ys[x])

#     # data for hover and url will only be one row for each group
#     data <- data[sapply(idx, "[", 1), ]

#     ns <- lapply(args$params, length)
#     bad_ind <- which(!ns %in% c(0, 1, length(idx), length(group)))
#     if (length(bad_ind) > 0) {
#       message(
#         "The following arguments do not have length the same as the number of groups ",
#         "or the total number of observations for ly_polygons() and will be ignored: ",
#         paste(names(args$params[bad_ind]), collapse = ", "))
#       args$params[bad_ind] <- NULL
#     }

#     full_length <- which(ns == length(group))
#     for (ii in full_length) {
#       args$params[[ii]] <- sapply(idx, function(x) args$params[[ii]][x[1]])
#     }
#   }

#   ## translate different x, y types to vectors
#   if (is.atomic(xs) && !is.list(xs)) {
#     xs <- list(xs)
#   }

#   if (is.atomic(ys) && !is.list(ys)) {
#     ys <- list(ys)
#   }

#   if (!(is.list(xs) && is.list(ys))) {
#     stop(
#       "For ly_polygons, xs and ys must be lists or specified through a data frame ",
#       "through 'data' argument.")
#   }

#   args$params <- resolve_color_alpha(args$params, has_line = TRUE, has_fill = TRUE,
#     fig$x$spec$layers[[args$info$lgroup]], theme = fig$x$spec$theme)

#   ## see if any options won't be used and give a message
#   check_opts(args$params, "patches", formals = names(formals(ly_polygons)))

#   if (is.null(args$params$fill_alpha)) {
#     args$params$fill_alpha <- 0.5
#   }

#   axis_type_range <- get_glyph_axis_type_range(unlist(xs), unlist(ys))

#   mc <- lapply(match.call(), deparse)

#   make_glyph(
#     fig, type = "patches", data = list(xs = unname(xs), ys = unname(ys)),
#     args = args$params, axis_type_range = axis_type_range,
#     xname = args$info$x_name, yname = args$info$y_name,
#     lname = args$info$lname, lgroup = args$info$lgroup, hover = args$info$hover,
#     url = args$info$url,
#     ly_call = mc
#   )
# }



# #' Add an "annular_wedge" layer to a Bokeh figure
# #' @param fig figure to modify
# #' @param x values or field name of center x coordinates
# #' @param y values or field name of center y coordinates
# #' @param data an optional data frame, providing the source for inputs x, y, and other glyph properties
# #' @param inner_radius values or field name of inner radii
# #' @param outer_radius values or field name of outer radii
# #' @param start_angle the angles to start the annular wedges, in radians, as measured from the horizontal
# #' @param end_angle the angles to end the annular wedges, in radians, as measured from the horizontal
# #' @param direction direction to turn between starting and ending angles ("anticlock", "clock")
# # template par-coloralpha
# # template par-hover
# # template par-url
# # template par-legend
# # template par-lnamegroup
# # template dots-fillline
# #' @examples
# #' rescale <- function(x)
# #'   (x - min(x)) / diff(range(x))
# #' figure() %>%
# #'   ly_annular_wedge(Sepal.Length, Sepal.Width, data = iris,
# #'     end_angle = rescale(Petal.Length) * 2 * pi, color = Species,
# #'     inner_radius = 0.1, outer_radius = 0.15, alpha = 0.5,
# #'     hover = Species)
# #' @family layer functions
# #' @export
# ly_annular_wedge <- function(
#   fig, x, y = NULL, data = figure_data(fig),
#   inner_radius = 0.1, outer_radius = 0.3,
#   start_angle = 0, end_angle = 2 * pi, direction = "anticlock",
#   color = NULL, alpha = 1,
#   hover = NULL, url = NULL, legend = NULL,
#   lname = NULL, lgroup = NULL, visible = TRUE, ...
# ) {

#   validate_fig(fig, "ly_annular_wedge")

#   args <- sub_names(fig, data,
#     grab(
#       x,
#       y,
#       inner_radius,
#       outer_radius,
#       start_angle,
#       end_angle,
#       direction,
#       color,
#       alpha,
#       hover,
#       url,
#       legend,
#       lname,
#       lgroup,
#       visible,
#       dots = lazy_dots(...)
#     )
#   )
#   args$params$glyph <- "annular_wedge"

#   if (missing(alpha))
#     args$params$alpha <- NULL

#   args$params <- resolve_color_alpha(args$params, has_line = TRUE, has_fill = TRUE,
#     fig$x$spec$layers[[args$info$lgroup]], theme = fig$x$spec$theme)

#   ## see if any options won't be used and give a message
#   check_opts(args$params, "annular_wedge", names(formals(ly_annular_wedge)))

#   axis_type_range <- get_glyph_axis_type_range(args$data$x, args$data$y,
#     assert_x = "numeric", assert_y = "numeric")

#   mc <- lapply(match.call(), deparse)

#   make_glyph(fig, type = "annular_wedge", lname = args$info$lname,
#     lgroup = args$info$lgroup,
#     data = args$data, data_sig = ifelse(is.null(data), NA, digest(data)),
#     args = args$params, axis_type_range = axis_type_range,
#     hover = args$info$hover, url = args$info$url, legend = args$info$legend,
#     xname = args$info$x_name, yname = args$info$y_name, ly_call = mc)
# }

# #' Add an "annulus" layer to a Bokeh figure
# #' @param fig figure to modify
# #' @param x values or field name of center x coordinates
# #' @param y values or field name of center y coordinates
# #' @param data an optional data frame, providing the source for inputs x, y, and other glyph properties
# #' @param inner_radius values or field name of inner radii
# #' @param outer_radius values or field name of outer radii
# # template par-coloralpha
# # template par-hover
# # template par-url
# # template par-legend
# # template par-lnamegroup
# # template dots-fillline
# #' @examples
# #' rescale <- function(x)
# #'   (x - min(x)) / diff(range(x))
# #' figure() %>%
# #'   ly_annulus(Sepal.Length, Sepal.Width, data = iris,
# #'     color = Species, hover = Species, alpha = 0.5,
# #'     outer_radius = rescale(Petal.Length) * 0.3,
# #'     inner_radius = rescale(Petal.Length) * 0.1)
# #' @family layer functions
# #' @export
# ly_annulus <- function(
#   fig, x, y = NULL, data = figure_data(fig),
#   inner_radius = 0.1, outer_radius = 0.2,
#   color = NULL, alpha = 1,
#   hover = NULL, url = NULL, legend = NULL,
#   lname = NULL, lgroup = NULL, visible = TRUE,
#   ...
# ) {

#   validate_fig(fig, "ly_annulus")

#   args <- sub_names(fig, data,
#     grab(
#       x,
#       y,
#       inner_radius,
#       outer_radius,
#       color,
#       alpha,
#       hover,
#       url,
#       legend,
#       lname,
#       lgroup,
#       visible,
#       dots = lazy_dots(...)
#     )
#   )
#   args$params$glyph <- "annulus"

#   if (missing(alpha)) {
#     args$params$alpha <- NULL
#   }

#   args$params <- resolve_color_alpha(args$params, has_line = TRUE, has_fill = TRUE,
#     fig$x$spec$layers[[args$info$lgroup]], theme = fig$x$spec$theme)

#   ## see if any options won't be used and give a message
#   check_opts(args$params, "annulus", formals = names(formals(ly_annulus)))

#   axis_type_range <- get_glyph_axis_type_range(args$data$x, args$data$y,
#     assert_x = "numeric", assert_y = "numeric")

#   mc <- lapply(match.call(), deparse)

#   make_glyph(fig, type = "annulus", lname = args$info$lname, lgroup = args$info$lgroup,
#     data = args$data, data_sig = ifelse(is.null(data), NA, digest(data)),
#     args = args$params, axis_type_range = axis_type_range,
#     hover = args$info$hover, url = args$info$url, legend = args$info$legend,
#     xname = args$info$x_name, yname = args$info$y_name,
#     ly_call = mc)
# }

# #' Add an "arc" layer to a Bokeh figure
# #' @param fig figure to modify
# #' @param x values or field name of center x coordinates
# #' @param y values or field name of center y coordinates
# #' @param data an optional data frame, providing the source for inputs x, y, and other glyph properties
# # template par-lineprops
# #' @param radius values or field name of arc radii
# #' @param start_angle values or field name of starting angles
# #' @param end_angle values or field name of ending angles
# #' @param direction direction to turn between starting and ending angles ("anticlock", "clock")
# # template par-legend
# # template par-lnamegroup
# # template dots-line
# #' @examples
# #' rescale <- function(x)
# #'   (x - min(x)) / diff(range(x))
# #' figure() %>%
# #'   ly_arc(Sepal.Length, Sepal.Width, data = iris,
# #'     end_angle = rescale(Petal.Length) * 2 * pi, color = Species,
# #'     alpha = 0.5)
# #' @family layer functions
# #' @export
# ly_arc <- function(
#   fig, x, y = NULL, data = figure_data(fig),
#   color = NULL, alpha = 1, width = 2, type = 1,
#   radius = 0.2,
#   start_angle = 0, end_angle = 2 * pi, direction = "anticlock",
#   legend = NULL, lname = NULL, lgroup = NULL, visible = TRUE,
#   ...
# ) {

#   validate_fig(fig, "ly_arc")

#   args <- sub_names(fig, data,
#     grab(
#       x,
#       y,
#       color,
#       alpha,
#       width,
#       type,
#       radius,
#       start_angle,
#       end_angle,
#       direction,
#       # hover,
#       # url,
#       legend,
#       lname,
#       lgroup,
#       visible,
#       dots = lazy_dots(...)
#     )
#   )
#   args$params$glyph <- "arc"
#   args$params <- resolve_line_args(fig, args$params)

#   ## see if any options won't be used and give a message
#   check_opts(args$params, "arc", formals = names(formals(ly_arc)))

#   axis_type_range <- get_glyph_axis_type_range(args$data$x, args$data$y,
#     assert_x = "numeric", assert_y = "numeric")

#   mc <- lapply(match.call(), deparse)

#   make_glyph(
#     fig, type = "arc", lname = args$info$lname, lgroup = args$info$lgroup,
#     data = args$data, data_sig = ifelse(is.null(data), NA, digest(data)),
#     args = args$params, axis_type_range = axis_type_range,
#     hover = args$info$hover, url = args$info$url,
#     legend = args$info$legend, xname = args$info$x_name,
#     yname = args$info$y_name, ly_call = mc)
# }

# #' Add a "wedge" layer to a Bokeh figure
# #' @param fig figure to modify
# #' @param x values or field name of center x coordinates
# #' @param y values or field name of center y coordinates
# #' @param data an optional data frame, providing the source for inputs x, y, and other glyph properties
# #' @param radius values or field name of wedge radii
# #' @param start_angle the angles to start the wedges, in radians, as measured from the horizontal
# #' @param end_angle the angles to end the wedges, in radians, as measured from the horizontal
# #' @param direction direction to turn between starting and ending angles ("anticlock", "clock")
# # template par-coloralpha
# # template par-hover
# # template par-url
# # template par-legend
# # template par-lnamegroup
# # template dots-fillline
# #' @examples
# #' rescale <- function(x)
# #'   (x - min(x)) / diff(range(x))
# #' figure() %>%
# #'   ly_wedge(Sepal.Length, Sepal.Width, data = iris,
# #'     end_angle = rescale(Petal.Length) * 2 * pi, color = Species,
# #'     radius = 0.15, alpha = 0.5,
# #'     hover = Species)
# #' @family layer functions
# #' @export
# ly_wedge <- function(
#   fig, x, y = NULL, data = figure_data(fig),
#   radius = 0.3, start_angle = 0, end_angle = 2 * pi, direction = "anticlock",
#   color = NULL, alpha = 1,
#   hover = NULL, url = NULL, legend = NULL,
#   lname = NULL, lgroup = NULL, visible = TRUE,
#   ...
# ) {

#   validate_fig(fig, "ly_wedge")

#   args <- sub_names(fig, data,
#     grab(
#       x,
#       y,
#       color,
#       alpha,
#       radius,
#       start_angle,
#       end_angle,
#       direction,
#       hover,
#       url,
#       legend,
#       lname,
#       lgroup,
#       visible,
#       dots = lazy_dots(...)
#     )
#   )
#   args$params$glyph <- "wedge"

#   if (missing(alpha)) {
#     args$params$alpha <- NULL
#   }

#   args$params <- resolve_color_alpha(args$params, has_line = TRUE, has_fill = TRUE,
#     fig$x$spec$layers[[args$info$lgroup]], theme = fig$x$spec$theme)

#   ## see if any options won't be used and give a message
#   check_opts(args$params, "wedge", formals = names(formals(ly_wedge)))

#   check_arc_direction(direction)

#   axis_type_range <- get_glyph_axis_type_range(args$data$x, args$data$y,
#     assert_x = "numeric", assert_y = "numeric")

#   mc <- lapply(match.call(), deparse)

#   make_glyph(
#     fig, type = "wedge", lname = args$info$lname, lgroup = args$info$lgroup,
#     data = args$data, data_sig = ifelse(is.null(data), NA, digest(data)),
#     args = args$params, axis_type_range = axis_type_range,
#     hover = args$info$hover, url = args$info$url, legend = args$info$legend,
#     xname = args$info$x_name, yname = args$info$y_name, ly_call = mc
#   )
# }

# #' Add a "segments" layer to a Bokeh figure
# #'
# #' Draws line segments with the given starting and ending coordinates.
# #' @param fig figure to modify
# #' @param x0 values or field name of starting x coordinates
# #' @param y0 values or field name of starting y coordinates
# #' @param x1 values or field name of ending x coordinates
# #' @param y1 values or field name of ending y coordinates
# #' @param data an optional data frame, providing the source for inputs x, y, and other glyph properties
# # template par-lineprops
# # template par-legend
# # template par-lnamegroup
# # template dots-line
# #' @family layer functions
# #' @export
# ly_segments <- function(fig, x0, y0, x1, y1, data = figure_data(fig),
#   color = "black", alpha = 1, width = 1, type = 1,
#   legend = NULL, lname = NULL, lgroup = NULL, visible = TRUE, ...) {

#   validate_fig(fig, "ly_segments")

#   args <- sub_names(fig, data,
#     grab(
#       x0,
#       y0,
#       x1,
#       y1,
#       color,
#       alpha,
#       width,
#       type,
#       legend,
#       lname,
#       lgroup,
#       visible,
#       dots = lazy_dots(...)
#     )
#   )
#   args$params$glyph <- "segment"

#   if (missing(color) && !is.null(args$params$line_color))
#     args$params$color <- NULL

#   args$params <- resolve_line_args(fig, args$params)

#   axis_type_range <- get_glyph_axis_type_range(
#     c(args$data$x0, args$data$x1),
#     c(args$data$y0, args$data$y1)
#   )

#   ## see if any options won't be used and give a message
#   check_opts(args$params, "segment", formals = names(formals(ly_segments)))

#   mc <- lapply(match.call(), deparse)

#   make_glyph(
#     fig, type = "segment",
#     data = args$data,
#     xname = args$info$x_name, yname = args$info$y_name,
#     args = args$params, axis_type_range = axis_type_range,
#     legend = args$info$legend, lname = args$info$lname, lgroup = args$info$lgroup,
#     ly_call = mc
#   )
# }

# #' Add an "abline" layer to a Bokeh figure
# #'
# #' Draws one or more straight lines.
# #' @param fig figure to modify
# #' @param a,b the intercept and slope of the line(s) to draw
# #' @param v the x value(s) for vertical lines
# #' @param h the y value(s) for horizontal lines
# #' @param coef a vector of length two giving the intercept and slope
# # template par-lineprops
# # template par-legend
# # template par-lnamegroup
# # template dots-line
# #' @example man-roxygen/ex-lines.R
# #' @examples
# #' # abline with mixed axes for h and v
# #' figure() %>%
# #'   ly_points(1:26, letters) %>%
# #'   ly_abline(h = "j") %>%
# #'   ly_abline(v = 10)
# #'
# #' # multiple hv lines
# #' figure() %>%
# #'   ly_points(1:10) %>%
# #'   ly_abline(v = 1:10) %>%
# #'   ly_abline(h = 1:10)
# #'
# #' # multiple ab lines
# #' figure() %>%
# #'   ly_points(0:10) %>%
# #'   ly_abline(0, seq(0, 1, by = 0.1))
# #' @family layer functions
# #' @export
# ly_abline <- function(
#   fig, a = NULL, b = NULL, v = NULL, h = NULL, coef = NULL,
#   color = "black", alpha = NULL, width = 1, type = 1,
#   legend = NULL, lname = NULL, lgroup = NULL, visible = TRUE,
#   ...) {

#   validate_fig(fig, "ly_abline")

#   args <- sub_names(fig, data = NULL,
#     grab(
#       color,
#       alpha,
#       width,
#       type,
#       legend,
#       lname,
#       lgroup,
#       visible,
#       dots = lazy_dots(...),
#       null_data = TRUE
#     )
#   )
#   args$params$glyph <- "segment"

#   if (missing(color) && !is.null(args$params$line_color)) {
#     args$params$color <- NULL
#   }

#   ## see if any options won't be used and give a message
#   check_opts(args$params, "segment", formals = names(formals(ly_abline)))

#   args$params <- resolve_line_args(fig, args$params)

#   x_axis_type <- "numeric"
#   y_axis_type <- "numeric"
#   if (!is.null(h) || !is.null(v)) {
#     x_axis_type <- fig$x$spec$x_axis_type
#     y_axis_type <- fig$x$spec$y_axis_type
#   }

#   # manage data
#   if (!is.null(coef) || inherits(a, "lm")) {
#     if (is.null(coef))
#       coef <- a
#     if (inherits(coef, "lm"))
#       coef <- coef(coef)
#     coef <- as.numeric(coef)
#     a <- coef[1]
#     b <- coef[2]
#   }

#   if (!is.null(a) && !is.null(b)) {
#     nn <- max(c(length(a), length(b)))
#     if (length(a) < nn)
#       a <- rep(a, nn)[1:nn]
#     if (length(b) < nn)
#       b <- rep(b, nn)[1:nn]
#     x0 <- rep(0, nn)
#     y0 <- a
#     x1 <- rep(1, nn)
#     y1 <- b * x1 + a
#   } else if (!is.null(h)) {
#     if (inherits(h, c("Date", "POSIXt"))) {
#       y_axis_type <- "datetime"
#       h <- to_epoch(h)
#     }
#     nn <- length(h)
#     x0 <- rep(0, nn)
#     y0 <- h
#     x1 <- rep(1, nn)
#     y1 <- h
#   } else if (!is.null(v)) {
#     if (inherits(v, c("Date", "POSIXt"))) {
#       x_axis_type <- "datetime"
#       v <- to_epoch(v)
#     }
#     nn <- length(v)
#     x0 <- v
#     y0 <- rep(0, nn)
#     x1 <- v
#     y1 <- rep(1, nn)
#   }

#   defer_fn <- function(data, xlim, ylim) {
#     if (length(data$x0) == 1) {
#       if (data$x0 == "x0")
#         return(data)
#     } else if (length(data$x0) == 0) {
#       return(data)
#     }
#     if (is.list(data$x0))
#       data <- unlist(data, recursive = FALSE)
#     if (all(data$x0 == data$x1)) {
#       ## vertical lines
#       lo <- head(ylim, 1)
#       up <- tail(ylim, 1)
#       if (is.character(lo)) {
#         lo <- paste0(lo, ":0")
#         up <- paste0(up, ":1")
#       }
#       data$y0 <- rep(lo, length(data$y0))
#       data$y1 <- rep(up, length(data$y1))
#     } else if (all(data$y0 == data$y1)) {
#       ## horizontal line
#       lo <- head(xlim, 1)
#       up <- tail(xlim, 1)
#       if (is.character(lo)) {
#         lo <- paste0(lo, ":0")
#         up <- paste0(up, ":1")
#       }
#       data$x0 <- rep(lo, length(data$x0))
#       data$x1 <- rep(up, length(data$x1))
#     } else {
#       ## line
#       b <- (data$y1 - data$y0) / (data$x1 - data$x0)
#       a <- data$y1 - b * data$x1
#       nn <- length(a)
#       data$x0 <- rep(head(xlim, 1), nn)
#       data$x1 <- rep(tail(xlim, 1), nn)
#       data$y0 <- data$x0 * b + a
#       data$y1 <- data$x1 * b + a
#     }
#     # now below wrap each result with list so json encoding is happy
#     if (length(data$x0) == 1) {
#       data$x0 <- list(data$x0)
#       data$x1 <- list(data$x1)
#       data$y0 <- list(data$y0)
#       data$y1 <- list(data$y1)
#     }
#     data
#   }

#   axis_type_range <- list(
#     x_axis_type = x_axis_type, y_axis_type = y_axis_type,
#     x_range = NULL, y_range = NULL)

#   mc <- lapply(match.call(), deparse)

#   make_glyph(
#     fig, type = "segment",
#     data = list(x0 = x0, y0 = y0, x1 = x1, y1 = y1, defer = defer_fn),
#     legend = args$info$legend,
#     lname = args$info$lname, lgroup = args$info$lgroup,
#     xname = args$info$x_name, yname = args$info$y_name,
#     args = args$params, axis_type_range = axis_type_range,
#     ly_call = mc
#   )
# }

# #' Add a "curve" layer to a Bokeh figure
# #'
# #' Draws a curve corresponding to a function over the interval \code{[from, to]}.
# #' @param fig figure to modify
# #' @param expr,from,to,n parameters sent to \code{\link[graphics]{curve}}
# # template par-lineprops
# # template par-legend
# # template par-lnamegroup
# # template dots-line
# #' @examples
# #' \donttest{
# #' chippy <- function(x) sin(cos(x)*exp(-x/2))
# #' figure(width = 800) %>%
# #'   ly_curve(chippy, -8, 7, n = 2001)
# #' }
# #' @family layer functions
# #' @export
# ly_curve <- function(
#   fig, expr, from = NULL, to = NULL, n = 101,
#   color = "black", alpha = 1, width = 1, type = 1,
#   legend = NULL, lname = NULL, lgroup = NULL, visible = TRUE,
#   ...
# ) {

#   validate_fig(fig, "ly_curve")

#   xname <- "x"
#   sexpr <- substitute(expr)
#   if (is.name(sexpr)) {
#     yname <- paste(deparse(sexpr), "(x)", sep = "")
#     expr <- call(as.character(sexpr), as.name(xname))
#   } else {
#     yname <- deparse(sexpr)
#     chk1 <- is.call(sexpr) || is.expression(sexpr)
#     chk <- !(chk1 && xname %in% all.vars(sexpr))
#     if (chk)
#       stop(
#         gettextf("'expr' must be a function, or a call or an expression containing '%s'",
#           xname), domain = NA)
#     expr <- sexpr
#   }

#   x <- seq.int(from, to, length.out = n)
#   ll <- list(x = x)
#   names(ll) <- xname
#   y <- eval(expr, envir = ll, enclos = parent.frame())

#   args <- sub_names(fig, data = NULL,
#     grab(
#       color,
#       alpha,
#       width,
#       type,
#       legend,
#       lname,
#       lgroup,
#       visible,
#       dots = lazy_dots(...),
#       null_data = TRUE
#     )
#   )

#   ## see if any options won't be used and give a message
#   check_opts(args$params, "line", formals = names(formals(ly_curve)))

#   if (missing(color) && !is.null(args$params$line_color))
#     args$params$color <- NULL

#   args$params <- resolve_line_args(fig, args$params)

#   do.call(ly_lines,
#     c(
#       list(
#         fig = fig,
#         x = x, y = y,
#         legend = args$info$legend, lname = args$info$lname, lgroup = args$info$lgroup,
#         xlab = xname, ylab = yname
#       ),
#       args$params
#     )
#   )
# }

# #' Add a "contour" layer to a Bokeh figure
# #'
# #' Computes and draws contour lines.
# #' @param fig figure to modify
# #' @param z a matrix containing the values to compute contour lines for
# #' @param x,y locations of grid lines at which the values in \code{image} are measured (see \code{\link[grDevices]{contourLines}})
# #' @param nlevels,levels parameters sent to \code{\link[grDevices]{contourLines}})
# # template par-lineprops
# # template par-lnamegroup
# # template dots-line
# #' @example man-roxygen/ex-image.R
# #' @family layer functions
# #' @export
# ly_contour <- function(
#   fig, z,
#   x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, length.out = ncol(z)),
#   nlevels = 10, levels = pretty(range(z, na.rm = TRUE), nlevels),
#   color = "black", alpha = 1, width = 1, type = 1,
#   lname = NULL, lgroup = NULL, visible = TRUE,
#   ...
# ) {

#   validate_fig(fig, "ly_contour")
#   ## see if any options won't be used and give a message

#   args <- sub_names(fig, data = NULL,
#     grab(
#       color,
#       alpha,
#       width,
#       type,
#       lname,
#       lgroup,
#       visible,
#       dots = lazy_dots(...),
#       null_data = TRUE
#     )
#   )

#   args$params <- resolve_line_args(fig, args$params)

#   contr <- do.call(grDevices::contourLines,
#     list(x = x, y = y, z = z, nlevels = nlevels, levels = levels))

#   xs <- lapply(contr, "[[", 2)
#   ys <- lapply(contr, "[[", 3)

#   check_opts(args$params, "multi_line", formals = names(formals(ly_contour)))

#   axis_type_range <- get_glyph_axis_type_range(x, y, assert_x = "numeric", assert_y = "numeric")

#   mc <- lapply(match.call(), deparse)

#   make_glyph(
#     fig, type = "multi_line",
#     lname = args$info$lname, lgroup = args$info$lgroup,
#     xname = args$info$x_name, yname = args$info$y_name,
#     data = list(xs = xs, ys = ys),
#     args = args$params, axis_type_range = axis_type_range,
#     ly_call = mc
#   )
# }

# #' Add a "ray" layer to a Bokeh figure
# #'
# #' Draws line segments starting at the given coordinate and extending the given length at the given angle.
# #' @param fig figure to modify
# #' @param x values or field name of center x coordinates
# #' @param y values or field name of center y coordinates
# #' @param data an optional data frame, providing the source for inputs x, y, and other glyph properties
# #' @param length values or field name of ray lengths in screen units
# #' @param angle values or field name of ray angles
# # template par-lineprops
# # template par-legend
# # template par-lnamegroup
# # template dots-line
# #' @family layer functions
# #' @export
# ly_ray <- function(
#   fig, x, y = NULL, data = figure_data(fig),
#   length = NULL, angle = 0,
#   color = "black", type = 1, width = 1, alpha = NULL,
#   legend = NULL, lname = NULL, lgroup = NULL, visible = TRUE,
#   ...
# ) {

#   validate_fig(fig, "ly_ray")

#   args <- sub_names(fig, data,
#     grab(
#       x, y,
#       length,
#       angle,
#       color,
#       alpha,
#       width,
#       type,
#       legend,
#       lname,
#       lgroup,
#       visible,
#       dots = lazy_dots(...)
#     )
#   )

#   if (missing(color) && !is.null(args$params$line_color)) {
#     args$params$color <- NULL
#   }

#   ## see if any options won't be used and give a message
#   check_opts(args$params, "ray", formals = names(formals(ly_ray)))

#   args$params <- resolve_line_args(fig, args$params)

#   axis_type_range <- get_glyph_axis_type_range(args$data$x, args$data$y)

#   mc <- lapply(match.call(), deparse)

#   make_glyph(
#     fig, type = "ray",
#     xname = args$info$x_name, yname = args$info$y_name,
#     data = args$data, legend = args$info$legend,
#     args = args$params, axis_type_range = axis_type_range,
#     lname = args$info$lname, lgroup = args$info$lgroup,
#     ly_call = mc
#   )
# }


# #' Add a "bezier" layer to a Bokeh figure
# #'
# #' Draws Bezier curves with the given starting, ending, and control points.
# #' @param fig figure to modify
# #' @param x0 values or field name of starting x coordinates
# #' @param y0 values or field name of starting y coordinates
# #' @param x1 values or field name of ending x coordinates
# #' @param y1 values or field name of ending y coordinates
# #' @param cx0 values or field name of first control point x coordinates
# #' @param cy0 values or field name of first control point y coordinates
# #' @param cx1 values or field name of second control point x coordinates
# #' @param cy1 values or field name of second control point y coordinates
# #' @param data an optional data frame, providing the source for start, end, and control point intputs, as well as other glyph properties
# # template par-lineprops
# # template par-legend
# # template par-lnamegroup
# # template dots-line
# #' @family layer functions
# #' @export
# ly_bezier <- function(
#   fig,
#   x0, y0, x1, y1, cx0, cy0, cx1, cy1,
#   data = figure_data(fig),
#   color = "black", alpha = 1, width = 1, type = 1,
#   legend = NULL, lname = NULL, lgroup = NULL, visible = TRUE,
#   ...
# ) {

#   validate_fig(fig, "ly_bezier")

#   args <- sub_names(fig, data,
#     grab(
#       x0, y0, x1, y1, cx0, cy0, cx1, cy1,
#       color,
#       alpha,
#       width,
#       type,
#       legend,
#       lname,
#       lgroup,
#       visible,
#       dots = lazy_dots(...)
#     )
#   )
#   args$params$glyph <- "bezier"

#   if (missing(color) && !is.null(args$params$line_color)) {
#     args$params$color <- NULL
#   }

#   ## see if any options won't be used and give a message
#   check_opts(args$params, "bezier", formals = names(formals(ly_bezier)))

#   args$params <- resolve_line_args(fig, args$params)

#   axis_type_range <- get_glyph_axis_type_range(
#     c(args$data$x0, args$data$x1),
#     c(args$data$y0, args$data$y1),
#     assert_x = "numeric", assert_y = "numeric"
#   )

#   mc <- lapply(match.call(), deparse)

#   make_glyph(
#     fig, type = "bezier",
#     lname = args$info$lname, lgroup = args$info$lgroup,
#     xname = args$info$x_name, yname = args$info$y_name,
#     data = args$data,
#     args = args$params, axis_type_range = axis_type_range,
#     ly_call = mc
#   )
# }

# #' Add a "quadratic" layer to a Bokeh figure
# #'
# #' Draws quadratic curves with the given starting, ending, and control points.
# #' @param fig figure to modify
# #' @param x0 values or field name of starting x coordinates
# #' @param y0 values or field name of starting y coordinates
# #' @param x1 values or field name of ending x coordinates
# #' @param y1 values or field name of ending y coordinates
# #' @param cx values or field name of control point x coordinates
# #' @param cy values or field name of control point y coordinates
# #' @param data an optional data frame, providing the source for start, end, and control point intputs, as well as other glyph properties
# # template par-lineprops
# # template par-legend
# # template par-lnamegroup
# # template dots-fillline
# #' @family layer functions
# #' @export
# ly_quadratic <- function(
#   fig,
#   x0, y0, x1, y1, cx, cy,
#   data = figure_data(fig),
#   color = "black", alpha = 1, width = 1, type = 1,
#   legend = NULL, lname = NULL, lgroup = NULL, visible = TRUE,
#   ...
# ) {

#   validate_fig(fig, "ly_quadratic")

#   args <- sub_names(fig, data,
#     grab(
#       x0, y0, x1, y1, cx, cy,
#       color,
#       alpha,
#       width,
#       type,
#       legend,
#       lname,
#       lgroup,
#       visible,
#       dots = lazy_dots(...)
#     )
#   )
#   args$params$glyph <- "quadratic"

#   if (missing(color) && !is.null(args$params$line_color)) {
#     args$color <- NULL
#   }

#   ## see if any options won't be used and give a message
#   check_opts(args$params, "quadratic", formals = names(formals(ly_quadratic)))

#   args$params <- resolve_line_args(fig, args$params)

#   axis_type_range <- get_glyph_axis_type_range(
#     c(args$data$x0, args$data$x1),
#     c(args$data$y0, args$data$y1),
#     assert_x = "numeric", assert_y = "numeric"
#   )

#   mc <- lapply(match.call(), deparse)

#   make_glyph(
#     fig, type = "quadratic",
#     lname = args$info$lname, lgroup = args$info$lgroup,
#     xname = args$info$x_name, yname = args$info$y_name,
#     data = args$data,
#     args = args$params, axis_type_range = axis_type_range,
#     ly_call = mc
#   )
# }

# ## a common thing to do is make a layer with both points and lines (type = "b")
# # ly_pointsline <- function()

# #' Add a "multi_line" layer to a Bokeh figure
# #'
# #' Draws multiple lines with the given lists of coordinates.
# #' @param fig figure to modify
# #' @param xs list of vectors of x coordinates
# #' @param ys list of vectors of y coordinates
# # template par-lineprops
# # template par-lnamegroup
# # template dots-line
# #' @family layer functions
# #' @export
# ly_multi_line <- function(
#   fig,
#   xs, ys,
#   color = "black", alpha = 1, width = 1, type = 1,
#   lname = NULL, lgroup = NULL, visible = TRUE,
#   ...
# ) {

#   validate_fig(fig, "ly_multi_line")

#   args <- sub_names(fig, data = NULL,
#     grab(
#       xs, ys,
#       color,
#       alpha,
#       width,
#       type,
#       # no legend?
#       lname,
#       lgroup,
#       visible,
#       dots = lazy_dots(...)
#     )
#   )
#   args$params$glyph <- "line"

#   if (missing(color) && !is.null(args$params$line_color)) {
#     args$color <- NULL
#   }

#   ## see if any options won't be used and give a message
#   # can't pass in color, alpha, width, or type
#   good_names <- names(args$params)
#   good_names <- good_names[! (good_names %in% c("color", "alpha", "width", "type"))]
#   check_opts(args$params[good_names], "multi_line")

#   args$params <- resolve_line_args(fig, args$params)

#   axis_type_range <- get_glyph_axis_type_range(unlist(args$data$xs), unlist(args$data$ys))

#   mc <- lapply(match.call(), deparse)

#   make_glyph(
#     fig, type = "multi_line",
#     data = args$data, args = args$params,
#     xname = args$info$x_name, yname = args$info$y_name,
#     lname = args$info$lname, lgroup = args$info$lgroup,
#     axis_type_range = axis_type_range,
#     ly_call = mc
#   )
# }



# #' Add an "image" layer to a Bokeh figure
# #'
# #' Draws a grid of rectangles with colors corresponding to the values in \code{z}
# #' @param fig figure to modify
# #' @param z matrix or vector of image values
# #' @param rows if \code{z} is a vector, how many rows should be used in treating it as a matrix
# #' @param byrow if \code{z} is a vector, should it be turned into a matrix by row
# #' @param x lower left x coordinates
# #' @param y lower left y coordinates
# #' @param dw image width distances
# #' @param dh image height distances
# #' @param palette name of color palette to use for color ramp (see \href{http://bokeh.pydata.org/en/latest/docs/reference/palettes.html}{here} for acceptable values)
# #' @param dilate logical - whether to dilate pixel distance computations when drawing
# # template par-lnamegroup
# #' @example man-roxygen/ex-image.R
# #' @family layer functions
# #' @export
# ly_image <- function(fig, z, rows, byrow = TRUE, x = 0, y = 0, dw = 1, dh = 1,
#   palette = "Spectral10", dilate = FALSE,
#   lname = NULL, lgroup = NULL, visible = TRUE) {

#   validate_fig(fig, "ly_image")
#   ## see if any options won't be used and give a message
#   # check_opts(list(...), "image")

#   args <- sub_names(fig, data = NULL,
#     grab(
#       x,
#       y,
#       lname,
#       lgroup,
#       visible,
#       dots = lazy_dots()
#     )
#   )

#   axis_type_range <- get_glyph_axis_type_range(c(x, dw), c(y, dh),
#     assert_x = "numeric", assert_y = "numeric")

#   if (is.vector(z)) {
#     z <- matrix(z, nrow = rows, byrow = byrow)
#   } else if (is.matrix(z)) {
#     z <- t(z)
#   } else {
#     stop("argument 'z' to ly_image must be a matrix or vector", call. = FALSE)
#   }

#   # really ugly nested if else
#   # palette checker / transformer from layer_hexbin minus function
#   #   plus added check for length 1
#   if ( is.character(palette) && length(palette) == 1 ) {
#     if (valid_color(palette)) {
#       stop(
#         "'palette' specified in ly_image is a single color; please supply a ",
#         "vector of colors or name of a bokeh palette - see here: ",
#         "http://bokeh.pydata.org/en/latest/docs/reference/palettes.html",
#         call. = FALSE)
#     } else {
#       if (!palette %in% bk_gradient_palette_names){
#         stop(
#           "'palette' specified in ly_image is not a valid color name or palette - ",
#           "see here: http://bokeh.pydata.org/en/latest/docs/reference/palettes.html",
#           call. = FALSE)
#       } else {
#         palette <- bk_gradient_palettes[[palette]]
#       }
#     }
#   } else if ( is.character(palette) && length(palette) > 1 ) {
#     # check for valid colors in the palette
#     if (!valid_color(palette)){
#       stop(
#         "'palette' specified in ly_image is not a valid color name or palette - ",
#         "see here: http://bokeh.pydata.org/en/latest/docs/reference/palettes.html",
#         call. = FALSE)
#     }
#   } else {
#     stop(
#       "'palette' specified in ly_image is not a valid color name or palette - ",
#       "see here: http://bokeh.pydata.org/en/latest/docs/reference/palettes.html",
#       call. = FALSE)
#   }

#   mc <- lapply(match.call(), deparse)

#   make_glyph(fig, type = "image", lname = args$info$lname, lgroup = args$info$lgroup,
#     data = list(image = list(z), palette = palette),
#     args = list(x = x, y = y, dw = dw, dh = dh, dilate = dilate),
#     axis_type_range = axis_type_range, ly_call = mc)
# }

# #' Add an "image_url" layer to a Bokeh figure
# #'
# #' Renders raster images from URLs at provided coordinates
# #' @param fig figure to modify
# #' @param x x coordinates
# #' @param y y coordinates
# #' @param data an optional data frame, providing the source for inputs x, y, and other properties
# #' @param w,h values or field names of width and height of image
# #' @param image_url values or field name of image URLs
# #' @param dilate logical - whether to dilate pixel distance computations when drawing
# #' @param anchor where the image is anchored to with respect to \code{x} and \code{y}
# #' @param angle values or field name of the angle to rotate the image, in radians
# # template par-lnamegroup
# #' @family layer functions
# #' @example man-roxygen/ex-image_url.R
# #' @export
# ly_image_url <- function(
#   fig, x = 0, y = 0, data = figure_data(fig), w = 10, h = 10,
#   image_url, dilate = TRUE, anchor = "top_left", angle = 0,
#   lname = NULL, lgroup = NULL, visible = TRUE
# ) {

#   validate_fig(fig, "ly_image_url")

#   anchor_opts <- c("top_left", "top_center", "top_right", "right_center",
#     "bottom_right", "bottom_center", "bottom_left", "left_center", "center")
#   if (! anchor %in% anchor_opts) {
#     stop("anchor must be one of: ", paste(anchor_opts, collapse = ", "), call. = FALSE)
#   }

#   args <- sub_names(fig, data,
#     grab(
#       x,
#       y,
#       w,
#       h,
#       image_url,
#       dilate,
#       anchor,
#       angle,
#       lname,
#       lgroup,
#       visible,
#       dots = lazy_dots()
#     )
#   )

#   # TODO: this url is not a "url" - it is data, not a parameter
#   args$params$url <- args$params$image_url
#   args$params$image_url <- NULL

#   if (missing(x)) {
#     args$info$x_name <- "x"
#   }
#   if (missing(y)) {
#     args$info$y_name <- "y"
#   }

#   # pull out values, as they are used a lot
#   x <- args$data$x
#   y <- args$data$y
#   h <- args$params$h
#   w <- args$params$w

#   ## range stuff
#   if (grepl("left", anchor)) {
#     x2 <- max(x + w)
#   } else if (grepl("right", anchor)) {
#     x2 <- min(x - w)
#   } else if (anchor %in% c("top_center", "bottom_center", "center")) {
#     x2 <- range(c(x +  w / 2, x - w / 2))
#   }
#   if (grepl("top", anchor)) {
#     y2 <- min(y - h)
#   } else if (grepl("bottom", anchor)) {
#     y2 <- max(y + h)
#   } else if (anchor %in% c("left_center", "right_center", "center")) {
#     y2 <- range(c(y + h / 2, y - h / 2))
#   }
#   # can this have "categorical" axes?
#   axis_type_range <- get_glyph_axis_type_range(c(x, x2), c(y, y2))

#   mc <- lapply(match.call(), deparse)

#   make_glyph(
#     fig, type = "image_URL",
#     xname = args$info$x_name, yname = args$info$y_name,
#     lname = args$info$lname, lgroup = args$info$lgroup,
#     data = args$data, args = args$params,
#     axis_type_range = axis_type_range, ly_call = mc
#   )
# }

