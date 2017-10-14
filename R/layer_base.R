#' Add a "points" layer to a Bokeh figure
#' Draws points with the given coordinates.
#' @param fig Figure to modify.
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
#' @param fig Figure to modify.
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
ly_lines <- function(
  fig, x = NULL, y = NULL,
  data = figure_data(fig),
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
#' @param fig Figure to modify.
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
    # legend = legend,
    use_all_data = FALSE), # for now (TODO)
    quos(...))

  add_layer(fig, spec, lgroup, lname)
}

#' Add a "rect" layer to a Bokeh figure
#' @param fig Figure to modify.
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
#' @param fig Figure to modify.
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
#' @param fig Figure to modify.
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
  # legend = TRUE,
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
    # legend = legend, # TODO: check back in on this...
    use_all_data = FALSE),
    quos(...))

  add_layer(fig, spec, lgroup, lname)
}

#' Add a "patch" layer to a Bokeh figure
#' @param fig Figure to modify.
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


#' Add an "annular_wedge" layer to a Bokeh figure
#' @param fig Figure to modify.
#' @param x values or field name of center x coordinates
#' @param y values or field name of center y coordinates
#' @param data an optional data frame, providing the source for inputs x, y, and other glyph properties
#' @param inner_radius values or field name of inner radii
#' @param outer_radius values or field name of outer radii
#' @param start_angle the angles to start the annular wedges, in radians, as measured from the horizontal
#' @param end_angle the angles to end the annular wedges, in radians, as measured from the horizontal
#' @param direction direction to turn between starting and ending angles ("anticlock", "clock")
# template par-coloralpha
# template par-hover
# template par-url
# template par-legend
# template par-lnamegroup
# template dots-fillline
#' @examples
#' rescale <- function(x)
#'   (x - min(x)) / diff(range(x))
#' figure() %>%
#'   ly_annular_wedge(Sepal.Length, Sepal.Width, data = iris,
#'     end_angle = rescale(Petal.Length) * 2 * pi, color = Species,
#'     inner_radius = 0.1, outer_radius = 0.15, alpha = 0.5,
#'     hover = Species)
#' @family layer functions
#' @export
ly_annular_wedge <- function(
  fig,
  x, y = NULL, data = figure_data(fig),
  inner_radius = 0.1, outer_radius = 0.3,
  start_angle = 0, end_angle = 2 * pi, direction = "anticlock",
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
    inner_radius = enquo(inner_radius), outer_radius = enquo(outer_radius),
    start_angle = enquo(start_angle), end_angle = enquo(end_angle),
    direction = enquo(direction),
    color = enquo(color), alpha = enquo(alpha),
    hov_color = enquo(hov_color), hov_alpha = enquo(hov_alpha),
    ns_color = enquo(ns_color), ns_alpha = enquo(ns_alpha),
    sel_color = enquo(sel_color), sel_alpha = enquo(sel_alpha),
    hover = enquo(hover), url = enquo(url),
    model = "AnnularWedge",
    type = "glyph",
    data = data,
    legend = legend,
    use_all_data = FALSE),
    quos(...))

  add_layer(fig, spec, lgroup, lname)
}

#' Add an "annulus" layer to a Bokeh figure
#' @param fig Figure to modify.
#' @param x values or field name of center x coordinates
#' @param y values or field name of center y coordinates
#' @param data an optional data frame, providing the source for inputs x, y, and other glyph properties
#' @param inner_radius values or field name of inner radii
#' @param outer_radius values or field name of outer radii
# template par-coloralpha
# template par-hover
# template par-url
# template par-legend
# template par-lnamegroup
# template dots-fillline
#' @examples
#' rescale <- function(x)
#'   (x - min(x)) / diff(range(x))
#' figure() %>%
#'   ly_annulus(Sepal.Length, Sepal.Width, data = iris,
#'     color = Species, hover = Species, alpha = 0.5,
#'     outer_radius = rescale(Petal.Length) * 0.3,
#'     inner_radius = rescale(Petal.Length) * 0.1)
#' @family layer functions
#' @export
ly_annulus <- function(
  fig,
  x, y = NULL, data = figure_data(fig),
  inner_radius = 0.1, outer_radius = 0.3,
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
    inner_radius = enquo(inner_radius), outer_radius = enquo(outer_radius),
    color = enquo(color), alpha = enquo(alpha),
    hov_color = enquo(hov_color), hov_alpha = enquo(hov_alpha),
    ns_color = enquo(ns_color), ns_alpha = enquo(ns_alpha),
    sel_color = enquo(sel_color), sel_alpha = enquo(sel_alpha),
    hover = enquo(hover), url = enquo(url),
    model = "Annulus",
    type = "glyph",
    data = data,
    legend = legend,
    use_all_data = FALSE),
    quos(...))

  add_layer(fig, spec, lgroup, lname)
}

#' Add an "arc" layer to a Bokeh figure
#' @param fig Figure to modify.
#' @param x values or field name of center x coordinates
#' @param y values or field name of center y coordinates
#' @param data an optional data frame, providing the source for inputs x, y, and other glyph properties
# template par-lineprops
#' @param radius values or field name of arc radii
#' @param start_angle values or field name of starting angles
#' @param end_angle values or field name of ending angles
#' @param direction direction to turn between starting and ending angles ("anticlock", "clock")
# template par-legend
# template par-lnamegroup
# template dots-line
#' @examples
#' rescale <- function(x)
#'   (x - min(x)) / diff(range(x))
#' figure() %>%
#'   ly_arc(Sepal.Length, Sepal.Width, data = iris,
#'     end_angle = rescale(Petal.Length) * 2 * pi, color = Species,
#'     alpha = 0.5)
#' @family layer functions
#' @export
ly_arc <- function(
  fig,
  x, y = NULL, data = figure_data(fig),
  radius = 0.2,
  start_angle = 0, end_angle = 2 * pi, direction = "anticlock",
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
    radius = enquo(radius),
    start_angle = enquo(start_angle), end_angle = enquo(end_angle),
    direction = enquo(direction),
    color = enquo(color), alpha = enquo(alpha),
    hov_color = enquo(hov_color), hov_alpha = enquo(hov_alpha),
    ns_color = enquo(ns_color), ns_alpha = enquo(ns_alpha),
    sel_color = enquo(sel_color), sel_alpha = enquo(sel_alpha),
    hover = enquo(hover), url = enquo(url),
    model = "Arc",
    type = "glyph",
    data = data,
    legend = legend,
    use_all_data = FALSE),
    quos(...))

  add_layer(fig, spec, lgroup, lname)
}

#' Add a "wedge" layer to a Bokeh figure
#' @param fig Figure to modify.
#' @param x values or field name of center x coordinates
#' @param y values or field name of center y coordinates
#' @param data an optional data frame, providing the source for inputs x, y, and other glyph properties
#' @param radius values or field name of wedge radii
#' @param start_angle the angles to start the wedges, in radians, as measured from the horizontal
#' @param end_angle the angles to end the wedges, in radians, as measured from the horizontal
#' @param direction direction to turn between starting and ending angles ("anticlock", "clock")
# template par-coloralpha
# template par-hover
# template par-url
# template par-legend
# template par-lnamegroup
# template dots-fillline
#' @examples
#' rescale <- function(x)
#'   (x - min(x)) / diff(range(x))
#' figure() %>%
#'   ly_wedge(Sepal.Length, Sepal.Width, data = iris,
#'     end_angle = rescale(Petal.Length) * 2 * pi, color = Species,
#'     radius = 0.15, alpha = 0.5,
#'     hover = Species)
#' @family layer functions
#' @export
ly_wedge <- function(
  fig,
  x, y = NULL, data = figure_data(fig),
  radius = 0.2,
  start_angle = 0, end_angle = 2 * pi, direction = "anticlock",
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
    radius = enquo(radius),
    start_angle = enquo(start_angle), end_angle = enquo(end_angle),
    direction = enquo(direction),
    color = enquo(color), alpha = enquo(alpha),
    hov_color = enquo(hov_color), hov_alpha = enquo(hov_alpha),
    ns_color = enquo(ns_color), ns_alpha = enquo(ns_alpha),
    sel_color = enquo(sel_color), sel_alpha = enquo(sel_alpha),
    hover = enquo(hover), url = enquo(url),
    model = "Wedge",
    type = "glyph",
    data = data,
    legend = legend,
    use_all_data = FALSE),
    quos(...))

  add_layer(fig, spec, lgroup, lname)
}

#' Add a "segments" layer to a Bokeh figure
#'
#' Draws line segments with the given starting and ending coordinates.
#' @param fig Figure to modify.
#' @param x0 values or field name of starting x coordinates
#' @param y0 values or field name of starting y coordinates
#' @param x1 values or field name of ending x coordinates
#' @param y1 values or field name of ending y coordinates
#' @param data an optional data frame, providing the source for inputs x, y, and other glyph properties
# template par-lineprops
# template par-legend
# template par-lnamegroup
# template dots-line
#' @family layer functions
#' @export
ly_segments <- function(
  fig,
  x0, y0, x1, y1,
  data = figure_data(fig),
  type = 1, width = 1,
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
    x0 = enquo(x0), y0 = enquo(y0),
    x1 = enquo(x1), y1 = enquo(y1),
    color = enquo(color), alpha = enquo(alpha),
    line_width = enquo(width), line_dash = enquo(type),
    hov_color = enquo(hov_color), hov_alpha = enquo(hov_alpha),
    ns_color = enquo(ns_color), ns_alpha = enquo(ns_alpha),
    sel_color = enquo(sel_color), sel_alpha = enquo(sel_alpha),
    hover = enquo(hover), url = enquo(url),
    model = "Segment",
    type = "glyph",
    data = data,
    legend = legend,
    use_all_data = FALSE), # for now (TODO)
    quos(...))

  add_layer(fig, spec, lgroup, lname)
}

#' Add a "ray" layer to a Bokeh figure
#'
#' Draws line segments starting at the given coordinate and extending the given length at the given angle.
#' @param fig Figure to modify.
#' @param x values or field name of center x coordinates
#' @param y values or field name of center y coordinates
#' @param data an optional data frame, providing the source for inputs x, y, and other glyph properties
#' @param length values or field name of ray lengths in screen units
#' @param angle values or field name of ray angles
# template par-lineprops
# template par-legend
# template par-lnamegroup
# template dots-line
#' @family layer functions
#' @export
ly_ray <- function(
  fig,
  x, y = NULL, data = figure_data(fig),
  length = NULL, angle = NULL,
  type = 1, width = 1, color = NULL, alpha = NULL,
  hover = NULL, url = NULL,
  hov_color = NULL, hov_alpha = NULL,
  ns_color = NULL, ns_alpha = NULL,
  sel_color = NULL, sel_alpha = NULL,
  legend = TRUE, lname = NULL, lgroup = NULL, visible = TRUE,
  ...
) {

  spec <- c(list(
    x = enquo(x), y = enquo(y),
    length = enquo(length), angle = enquo(angle),
    line_width = enquo(width), line_dash = enquo(type),
    color = enquo(color), alpha = enquo(alpha),
    hov_color = enquo(hov_color), hov_alpha = enquo(hov_alpha),
    ns_color = enquo(ns_color), ns_alpha = enquo(ns_alpha),
    sel_color = enquo(sel_color), sel_alpha = enquo(sel_alpha),
    hover = enquo(hover), url = enquo(url),
    model = "Ray",
    type = "glyph",
    data = data,
    legend = legend,
    use_all_data = FALSE),
    quos(...))

  add_layer(fig, spec, lgroup, lname)
}

#' Add a "bezier" layer to a Bokeh figure
#'
#' Draws Bezier curves with the given starting, ending, and control points.
#' @param fig Figure to modify.
#' @param x0 values or field name of starting x coordinates
#' @param y0 values or field name of starting y coordinates
#' @param x1 values or field name of ending x coordinates
#' @param y1 values or field name of ending y coordinates
#' @param cx0 values or field name of first control point x coordinates
#' @param cy0 values or field name of first control point y coordinates
#' @param cx1 values or field name of second control point x coordinates
#' @param cy1 values or field name of second control point y coordinates
#' @param data an optional data frame, providing the source for start, end, and control point intputs, as well as other glyph properties
# template par-lineprops
# template par-legend
# template par-lnamegroup
# template dots-line
#' @family layer functions
#' @export
ly_bezier <- function(
  fig,
  x0, y0, x1, y1, cx0, cy0, cx1, cy1,
  data = figure_data(fig),
  color = NULL, alpha = NULL,
  type = 1, width = 1,
  hov_color = NULL, hov_alpha = NULL,
  ns_color = NULL, ns_alpha = NULL,
  sel_color = NULL, sel_alpha = NULL,
  hover = NULL, url = NULL,
  legend = TRUE,
  lname = NULL, lgroup = NULL,
  ...
) {

  spec <- c(list(
    x0 = enquo(x0), y0 = enquo(y0),
    x1 = enquo(x1), y1 = enquo(y1),
    cx0 = enquo(x0), cy0 = enquo(y0),
    cx1 = enquo(x1), cy1 = enquo(y1),
    color = enquo(color), alpha = enquo(alpha),
    line_width = enquo(width), line_dash = enquo(type),
    hov_color = enquo(hov_color), hov_alpha = enquo(hov_alpha),
    ns_color = enquo(ns_color), ns_alpha = enquo(ns_alpha),
    sel_color = enquo(sel_color), sel_alpha = enquo(sel_alpha),
    hover = enquo(hover), url = enquo(url),
    model = "Bezier",
    type = "glyph",
    data = data,
    legend = legend,
    use_all_data = FALSE), # for now (TODO)
    quos(...))

  add_layer(fig, spec, lgroup, lname)
}

#' Add a "quadratic" layer to a Bokeh figure
#'
#' Draws quadratic curves with the given starting, ending, and control points.
#' @param fig Figure to modify.
#' @param x0 values or field name of starting x coordinates
#' @param y0 values or field name of starting y coordinates
#' @param x1 values or field name of ending x coordinates
#' @param y1 values or field name of ending y coordinates
#' @param cx values or field name of control point x coordinates
#' @param cy values or field name of control point y coordinates
#' @param data an optional data frame, providing the source for start, end, and control point intputs, as well as other glyph properties
# template par-lineprops
# template par-legend
# template par-lnamegroup
# template dots-fillline
#' @family layer functions
#' @export
ly_quadratic <- function(
  fig,
  x0, y0, x1, y1, cx, cy,
  data = figure_data(fig),
  color = NULL, alpha = NULL,
  type = 1, width = 1,
  hov_color = NULL, hov_alpha = NULL,
  ns_color = NULL, ns_alpha = NULL,
  sel_color = NULL, sel_alpha = NULL,
  hover = NULL, url = NULL,
  legend = TRUE,
  lname = NULL, lgroup = NULL,
  ...
) {

  spec <- c(list(
    x0 = enquo(x0), y0 = enquo(y0),
    x1 = enquo(x1), y1 = enquo(y1),
    cx = enquo(x0), cy = enquo(y0),
    color = enquo(color), alpha = enquo(alpha),
    line_width = enquo(width), line_dash = enquo(type),
    hov_color = enquo(hov_color), hov_alpha = enquo(hov_alpha),
    ns_color = enquo(ns_color), ns_alpha = enquo(ns_alpha),
    sel_color = enquo(sel_color), sel_alpha = enquo(sel_alpha),
    hover = enquo(hover), url = enquo(url),
    model = "Quadratic",
    type = "glyph",
    data = data,
    legend = legend,
    use_all_data = FALSE), # for now (TODO)
    quos(...))

  add_layer(fig, spec, lgroup, lname)
}


#' Add a "multi_line" layer to a Bokeh figure
#'
#' Draws multiple lines with the given lists of coordinates.
#' @param fig Figure to modify.
#' @param xs list of vectors of x coordinates
#' @param ys list of vectors of y coordinates
# template par-lineprops
# template par-lnamegroup
# template dots-line
#' @family layer functions
#' @export
ly_multi_line <- function(
  fig,
  xs = NULL, ys = NULL,
  data = figure_data(fig),
  type = 1, width = 1, color = NULL, alpha = NULL,
  hover = NULL, legend = TRUE,
  hov_color = NULL, hov_alpha = NULL, ns_color = NULL, ns_alpha = NULL,
  lgroup = NULL, lname = NULL,
  ...
) {

  spec <- c(list(
    xs = enquo(xs), ys = enquo(ys), color = enquo(color), alpha = enquo(alpha),
    hov_color = enquo(hov_color), hov_alpha = enquo(hov_alpha),
    ns_color = enquo(ns_color), ns_alpha = enquo(ns_alpha), hover = enquo(hover),
    line_width = enquo(width), line_dash = enquo(type),
    model = "MultiLine",
    type = "glyph",
    data = data,
    legend = legend,
    use_all_data = FALSE), # for now (TODO)
    quos(...))

  add_layer(fig, spec, lgroup, lname)
}

#' Add a "polygons" layer to a Bokeh figure
#' @param fig Figure to modify.
#' @param xs vector or list of values or field name of polygon x coordinates - see details
#' @param ys vector or list of values or field name of polygon y coordinates - see details
#' @param group vector or field name of grouping variable - see details
#' @param data an optional data frame, providing the source for inputs xs, ys, group, and other glyph properties
#' @details \code{xs} and \code{ys} can be a list of vectors, each element for one polygon to be drawn, or can be vectors with the \code{group} argument specifying how to break them up into individual polygons.
# template par-coloralpha
# template par-hover
# template par-url
# template par-lnamegroup
# template dots-fillline
#' @family layer functions
#' @export
ly_polygons <- function(
  fig,
  xs = NULL, ys = NULL,
  data = figure_data(fig),
  type = 1, width = 1, color = NULL, alpha = NULL,
  hover = NULL, legend = TRUE,
  hov_color = NULL, hov_alpha = NULL, ns_color = NULL, ns_alpha = NULL,
  lgroup = NULL, lname = NULL,
  ...) {

  spec <- c(list(
    xs = enquo(xs), ys = enquo(ys), color = enquo(color), alpha = enquo(alpha),
    hov_color = enquo(hov_color), hov_alpha = enquo(hov_alpha),
    ns_color = enquo(ns_color), ns_alpha = enquo(ns_alpha), hover = enquo(hover),
    line_width = enquo(width), line_dash = enquo(type),
    model = "Patches",
    type = "glyph",
    data = data,
    legend = legend,
    use_all_data = FALSE), # for now (TODO)
    quos(...))

  add_layer(fig, spec, lgroup, lname)

  # if (!is.null(group)) {
  #   if (is.factor(group)) {
  #     group <- as.character(group)
  #   }
  #   idx <- unname(split(seq_along(group), group))
  #   xs <- lapply(idx, function(x) xs[x])
  #   ys <- lapply(idx, function(x) ys[x])

  #   # data for hover and url will only be one row for each group
  #   data <- data[sapply(idx, "[", 1), ]

  #   ns <- lapply(args$params, length)
  #   bad_ind <- which(!ns %in% c(0, 1, length(idx), length(group)))
  #   if (length(bad_ind) > 0) {
  #     message(
  #       "The following arguments do not have length the same as the number of groups ",
  #       "or the total number of observations for ly_polygons() and will be ignored: ",
  #       paste(names(args$params[bad_ind]), collapse = ", "))
  #     args$params[bad_ind] <- NULL
  #   }

  #   full_length <- which(ns == length(group))
  #   for (ii in full_length) {
  #     args$params[[ii]] <- sapply(idx, function(x) args$params[[ii]][x[1]])
  #   }
  # }

  # ## translate different x, y types to vectors
  # if (is.atomic(xs) && !is.list(xs)) {
  #   xs <- list(xs)
  # }

  # if (is.atomic(ys) && !is.list(ys)) {
  #   ys <- list(ys)
  # }

  # if (!(is.list(xs) && is.list(ys))) {
  #   stop(
  #     "For ly_polygons, xs and ys must be lists or specified through a data frame ",
  #     "through 'data' argument.")
  # }
}

#' Add an "image" layer to a Bokeh figure
#'
#' Draws a grid of rectangles with colors corresponding to the values in \code{z}
#' @param fig Figure to modify.
#' @param z matrix or vector of image values
#' @param rows if \code{z} is a vector, how many rows should be used in treating it as a matrix
#' @param byrow if \code{z} is a vector, should it be turned into a matrix by row
#' @param x lower left x coordinates
#' @param y lower left y coordinates
#' @param dw image width distances
#' @param dh image height distances
#' @param palette name of color palette to use for color ramp (see \href{http://bokeh.pydata.org/en/latest/docs/reference/palettes.html}{here} for acceptable values)
#' @param dilate logical - whether to dilate pixel distance computations when drawing
# template par-lnamegroup
# @example man-roxygen/ex-image.R
#' @family layer functions
#' @export
ly_image <- function(
  fig,
  z,
  rows, byrow = TRUE, x = 0, y = 0, dw = 1, dh = 1,
  palette = "Spectral10", dilate = FALSE,
  lname = NULL, lgroup = NULL,
  ...
  ) {

  # TODO: is there hover behavior for this?

  if (is.vector(z)) {
    z <- matrix(z, nrow = rows, byrow = byrow)
  } else if (is.matrix(z)) {
    z <- t(z)
  } else {
    stop("argument 'z' to ly_image must be a matrix or vector", call. = FALSE)
  }

  spec <- c(list(
    image = quo(z), x = x, y = y, dh = dh, dw = dw, dilate = dilate,
    # color_mapper,
    # dh_units, dw_units,
    model = "Image",
    type = "glyph",
    data = NULL,
    legend = NULL,
    use_all_data = FALSE), # for now (TODO)
    quos(...))

  add_layer(fig, spec, lgroup, lname)


  # # palette checker / transformer from layer_hexbin
  # #   plus added check for length 1
  # if ( is.character(palette) && length(palette) == 1 ) {
  #   if (valid_color(palette)) {
  #     stop(
  #       "'palette' specified in ly_image is a single color; please supply a ",
  #       "vector of colors or name of a bokeh palette - see here: ",
  #       "http://bokeh.pydata.org/en/latest/docs/reference/palettes.html",
  #       call. = FALSE)
  #   } else {
  #     if (!palette %in% bk_gradient_palette_names){
  #       stop(
  #         "'palette' specified in ly_image is not a valid color name or palette - ",
  #         "see here: http://bokeh.pydata.org/en/latest/docs/reference/palettes.html",
  #         call. = FALSE)
  #     } else {
  #       palette <- bk_gradient_palettes[[palette]]
  #     }
  #   }
  # } else if ( is.character(palette) && length(palette) > 1 ) {
  #   # check for valid colors in the palette
  #   if (!valid_color(palette)){
  #     stop(
  #       "'palette' specified in ly_image is not a valid color name or palette - ",
  #       "see here: http://bokeh.pydata.org/en/latest/docs/reference/palettes.html",
  #       call. = FALSE)
  #   }
  # } else {
  #   stop(
  #     "'palette' specified in ly_image is not a valid color name or palette - ",
  #     "see here: http://bokeh.pydata.org/en/latest/docs/reference/palettes.html",
  #     call. = FALSE)
  # }
}


#' Add an "image_url" layer to a Bokeh figure
#'
#' Renders raster images from URLs at provided coordinates
#' @param fig Figure to modify.
#' @param x x coordinates
#' @param y y coordinates
#' @param data an optional data frame, providing the source for inputs x, y, and other properties
#' @param w,h values or field names of width and height of image
#' @param image_url values or field name of image URLs
#' @param dilate logical - Whether to always round fractional pixel locations in such a way as to make the images bigger. This setting may be useful if pixel rounding errors are causing images to have a gap between them, when they should appear flush.
#' @param anchor Where the image is anchored to with respect to \code{x} and \code{y}. One of 'top_left', 'top_center', 'top_right', 'center_left', 'center', 'center_right', 'bottom_left', 'bottom_center', 'bottom_right'.
#' @param angle values or field name of the angle to rotate the image, in radians
#' @param global_alpha An overall opacity that each image is rendered with (in addition to any inherent alpha values in the image itself).
#' @param retry_attempts Number of attempts to retry loading the images from the specified URL.
#' @param retry_timeout Timeout (in ms) between retry attempts to load the image from the specified URL.
# template par-lnamegroup
#' @family layer functions
# @example man-roxygen/ex-image_url.R
#' @export
ly_image_url <- function(
  fig, x = NULL, y = NULL,
  url,
  w = 10, h = 10,
  data = figure_data(fig),
  dilate = TRUE, anchor = "top_left", angle = 0,
  w_units = "screen", h_units = "screen", global_alpha = 1,
  retry_attempts = 0, retry_timeout = 0,
  lgroup = NULL, lname = NULL, ...) {

  spec <- c(list(
    x = enquo(x), y = enquo(y), url = enquo(url),
    w = enquo(w), h = enquo(h), angle = enquo(angle), dilate = dilate, anchor = anchor,
    global_alpha = global_alpha,
    retry_attempts = retry_attempts, retry_timeout = retry_timeout,
    model = "ImageURL",
    type = "glyph",
    data = data,
    legend = legend,
    use_all_data = FALSE), # for now (TODO)
    quos(...))

  add_layer(fig, spec, lgroup, lname)
}

# ly_ellipse <- function() {

# }
