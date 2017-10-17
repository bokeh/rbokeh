#' Add a "points" layer to a Bokeh figure
#' @param fig Figure to modify.
#' @param x Values or field name / expression indicating x coordinates of points.
#' @param y Values or field name / expression indicationg y coordinates of points.
#' @param data An optional data frame, providing the source for inputs x, y, and other glyph properties.
#' @param glyph Value or field name of the glyph to use. Either an integer value corresponding to acceptable \code{\link{pch}} values or one of "asterisk", "circle", "circle_cross", "circle_x", "cross", "diamond", "diamond_cross", "inverted_triangle", "square", "square_cross", "square_x", "triangle", "x".
#' @template par-coloralpha
#' @param size Size of the glyph in screen units.
#' @template par-hover
#' @template par-legend
#' @template par-coloralpha-extra
#' @template par-lnamegroup
#' @template dots-fillline
#' @family layer functions
#' @examples
#' figure(data = iris) %>%
#'   ly_points(x = Sepal.Width, y = Sepal.Length, color = Species,
#'   hover = iris)
#' @export
ly_points <- function(fig, x = NULL, y = NULL, data = figure_data(fig),
  glyph = 21, color = NULL, alpha = NULL, size = NULL,
  hover = NULL, legend = TRUE,
  hov_color = NULL, hov_alpha = NULL,
  ns_color = NULL, ns_alpha = NULL,
  sel_color = NULL, sel_alpha = NULL,
  lgroup = NULL, lname = NULL, ...) {

  # TODO: check glyph to see if it varies and figure out how to handle that

  # wait until print time to resolve everything
  # because data sources and themes can change
  # so just store quosures
  # TODO: should still validate that ... only contains valid parameter names
  spec <- c(list(
    x = enquo(x), y = enquo(y), color = enquo(color), alpha = enquo(alpha),
    size = enquo(size), hov_color = enquo(hov_color), hov_alpha = enquo(hov_alpha),
    ns_color = enquo(ns_color), ns_alpha = enquo(ns_alpha),
    sel_color = enquo(sel_color), sel_alpha = enquo(sel_alpha),
    hover = enquo(hover),
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
#' @param x Values or field name / expression indicating x coordinates of lines.
#' @param y Values or field name / expression indicationg y coordinates of lines.
#' @param data An optional data frame, providing the source for inputs x, y, and other glyph properties.
# @param group Values or field name of a grouping variable to break lines up by.
#' @template par-lineprops
#' @template par-hover
#' @template par-legend
#' @template par-lineprops-extra
#' @template par-lnamegroup
#' @template dots-line
#' @family layer functions
#' @examples
#' d <- data.frame(x = 1:100, y = sin(c(1:100) / 10))
#' figure(data = d) %>%
#'   ly_lines(x = x, y = y, width = 10, hover = d)
#' @export
ly_lines <- function(
  fig, x = NULL, y = NULL,
  data = figure_data(fig),
  type = 1, width = 1, color = NULL, alpha = NULL,
  hover = NULL, legend = TRUE,
  hov_color = NULL, hov_alpha = NULL,
  ns_color = NULL, ns_alpha = NULL,
  sel_color = NULL, sel_alpha = NULL,
  lgroup = NULL, lname = NULL, ...) {

  spec <- c(list(
    x = enquo(x), y = enquo(y), color = enquo(color), alpha = enquo(alpha),
    hov_color = enquo(hov_color), hov_alpha = enquo(hov_alpha),
    ns_color = enquo(ns_color), ns_alpha = enquo(ns_alpha),
    sel_color = enquo(sel_color), sel_alpha = enquo(sel_alpha),
    hover = enquo(hover),
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
#' @param x The x coordinates of text anchors.
#' @param y The y coordinates of text anchors.
#' @param text Text values to render.
#' @param data An optional data frame, providing the source for inputs x, y, text, and other glyph properties.
#' @param color Text color values for the text.
#' @param alpha Text alpha values for the text.
#' @param hov_color Text color values for the text when hovered.
#' @param hov_alpha Text alpha values for the text when hovered.
#' @param ns_color Text color values for the text when not selected.
#' @param ns_alpha Text alpha values for the text when not selected.
#' @param sel_color Text color values for the text when selected.
#' @param sel_alpha Text alpha values for the text when selected.
#' @param angle Angle to rotate the text in radians.
#' @param align Text align values for the text. One of ("left", "right", "center").
#' @param baseline Text baseline values for the text. One of ("top", "middle", "bottom", "alphabetic", "hanging").
#' @param font Text font values for the text.
#' @param font_size Text font size values for the text.
#' @param font_style Text font style values for the text ("normal", "italic", "bold").
#' @param x_offset Offset values to apply to the x-coordinates.
#' @param y_offset Offset values to apply to the y-coordinates.
#' @template par-legend
#' @template par-lnamegroup
# example man-roxygen/ex-elements.R
#' @family layer functions
#' @examples
#' figure(title = "Periodic Table", data = elements,
#'   xgrid = FALSE, ygrid = FALSE, xlab = "", ylab = "",
#'   xlim = as.character(1:18), ylim = c("   ", "  ", " ", as.character(7:1)),
#'   height = 700, width = 1200) %>%
#'   ly_crect(cat_offset(group, group_offset), period, 0.9, 0.9,
#'     color = group_block, fill_alpha = 0.6, legend = FALSE,
#'     hover = list(name, atomic_number, group_block, atomic_mass,
#'       electronic_configuration)) %>%
#'   ly_text(cat_offset(group, sym_offset), period, text = symbol,
#'     font_style = "bold", font_size = "15pt",
#'     align = "left", baseline = "middle") %>%
#'   ly_text(cat_offset(group, sym_offset), cat_offset(period, 0.3), text = atomic_number_p,
#'     font_size = "9pt", align = "left", baseline = "middle") %>%
#'   ly_text(cat_offset(group, sym_offset), cat_offset(period, -0.2), text = name,
#'     font_size = "6pt", align = "left", baseline = "middle") %>%
#'   ly_text(cat_offset(group, sym_offset), cat_offset(period, -0.35), text = atomic_mass,
#'     font_size = "6pt", align = "left", baseline = "middle") %>%
#'   x_axis(axis = axis_spec(visible = FALSE)) %>%
#'   y_axis(axis = axis_spec(visible = FALSE))
#' @export
ly_text <- function(fig,
  x, y = NULL, text = NULL, color = NULL, alpha = NULL, angle = NULL,
  data = figure_data(fig),
  x_offset = NULL, y_offset = NULL,
  font = NULL, font_size = NULL, font_style = NULL,
  align = NULL, baseline = NULL, legend = TRUE,
  hov_color = NULL, hov_alpha = NULL,
  ns_color = NULL, ns_alpha = NULL,
  sel_color = NULL, sel_alpha = NULL,
  lgroup = NULL, lname = NULL) {

  spec <- c(list(
    x = enquo(x), y = enquo(y), text = enquo(text),
    text_color = enquo(color), text_alpha = enquo(alpha), angle = enquo(angle),
    x_offset = enquo(x_offset), y_offset = enquo(y_offset),
    text_font = font, text_font_size = enquo(font_size),
    text_font_style = enquo(font_style),
    text_align = enquo(align), text_baseline = enquo(baseline),
    hov_color = enquo(hov_color), hov_alpha = enquo(hov_alpha),
    ns_color = enquo(ns_color), ns_alpha = enquo(ns_alpha),
    sel_color = enquo(sel_color), sel_alpha = enquo(sel_alpha),
    model = "Text",
    type = "glyph",
    data = data,
    legend = legend,
    use_all_data = FALSE), # for now (TODO)
    quos(...))

  add_layer(fig, spec, lgroup, lname)
}

#' Add a "rect" layer to a Bokeh figure
#' @param fig Figure to modify.
#' @param xleft Values or field name / expression indicating location of left edges.
#' @param ybottom Values or field name / expression indicating location of bottom edges.
#' @param xright Values or field name / expression indicating location of right edges.
#' @param ytop values or field name / expression indicating location of top edges
#' @param data An optional data frame, providing the source for inputs xleft, ybottom, xright, ytop, and other glyph properties.
#' @template par-coloralpha
#' @template par-coloralpha-extra
#' @template par-hover
#' @template par-legend
#' @template par-lnamegroup
#' @template dots-fillline
#' @family layer functions
#' @examples
#' d <- data.frame(
#'   x1 = c(-2, -2, 1, 1),
#'   x2 = c(-1, -1, 2, 2),
#'   y1 = c(1, -2, -2, 1),
#'   y2 = c(2, -1, -1, 2))
#'
#' figure() %>%
#'   ly_rect(x1, y1, x2, y2, color = x1, data = d, hover = d)
#'
#' figure() %>%
#'   ly_rect(x1, y1, x2, y2, color = letters[1:4], data = d, hover = d,
#'   legend = "color")
#' @export
ly_rect <- function(
  fig,
  xleft, ybottom, xright, ytop,
  data = figure_data(fig),
  color = NULL, alpha = NULL,
  hov_color = NULL, hov_alpha = NULL,
  ns_color = NULL, ns_alpha = NULL,
  sel_color = NULL, sel_alpha = NULL,
  hover = NULL,
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
    hover = enquo(hover),
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
#' @param x Values or field name / expression indicating location of center x coordinates.
#' @param y Values or field name / expression indicating location of center y coordinates.
#' @param data An optional data frame, providing the source for inputs xleft, ybottom, xright, ytop, and other glyph properties.
#' @param width Values or field name of widths.
#' @param height Values or field name of heights.
#' @param angle Values or field name of rotation angles.
#' @param dilate Logical indicating whether to dilate pixel distance computations when drawing.
#' @template par-coloralpha
#' @template par-coloralpha-extra
#' @template par-hover
#' @template par-legend
#' @template par-lnamegroup
#' @template dots-fillline
# example man-roxygen/ex-elements.R
#' @family layer functions
#' @export
#' @examples
#' figure(
#'   width = 1000,
#'   xlim = as.character(1:18),
#'   ylim = c("   ", "  ", " ", as.character(7:1))
#' ) %>%
#'   ly_crect(cat_offset(group, group_offset), period, 0.9, 0.9, data = elements,
#'     color = group_block, fill_alpha = 0.6,
#'     hover = list(name, atomic_number, group_block, atomic_mass,
#'       electronic_configuration), legend = FALSE)
ly_crect <- function(
  fig,
  x, y = NULL, width = 1, height = 1, angle = 0,
  data = figure_data(fig),
  color = NULL, alpha = NULL,
  hov_color = NULL, hov_alpha = NULL,
  ns_color = NULL, ns_alpha = NULL,
  sel_color = NULL, sel_alpha = NULL,
  dilate = NULL,
  hover = NULL,
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
    hover = enquo(hover),
    dilate = dilate,
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
#' @param x Values or field name / expression indicating location of center x coordinates.
#' @param y Values or field name / expression indicating location of center y coordinates.
#' @param data An optional data frame, providing the source for inputs x, y, and other glyph properties.
#' @param width Values or field name / expression indicating widths
#' @param height Values or field name / expression indicating heights
#' @param angle vValues or field name / expression indicating rotation angles
#' @template par-coloralpha
# @template par-coloralpha-extra
# @template par-hover
# @template par-legend
#' @template par-lnamegroup
#' @template dots-fillline
#' @family layer functions
#' @export
#' @examples
#' figure() %>%
#'   ly_oval(Sepal.Length, Sepal.Width,
#'     width = spec(15, units = "screen"),
#'     height = spec(30, units = "screen"),
#'     angle = runif(150) * 10,
#'     hov_color = "red",
#'     data = iris, color = Species, hover = iris)
ly_oval <- function(
  fig,
  x, y = NULL, width = 1, height = 1, angle = 0,
  data = figure_data(fig),
  color = NULL, alpha = NULL,
  # hov_color = NULL, hov_alpha = NULL,
  # ns_color = NULL, ns_alpha = NULL,
  # sel_color = NULL, sel_alpha = NULL,
  # hover = NULL,
  # legend = TRUE,
  lname = NULL, lgroup = NULL,
  ...
) {
  spec <- c(list(
    x = enquo(x), y = enquo(y), width = enquo(width), height = enquo(height),
    angle = enquo(angle),
    color = enquo(color), alpha = enquo(alpha),
    # hov_color = enquo(hov_color), hov_alpha = enquo(hov_alpha),
    # ns_color = enquo(ns_color), ns_alpha = enquo(ns_alpha),
    # sel_color = enquo(sel_color), sel_alpha = enquo(sel_alpha),
    # hover = enquo(hover),
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
#' @param x Values or field name / expression indicating patch x coordinates
#' @param y Values or field name / expression indicating patch y coordinates
#' @param data An optional data frame, providing the source for inputs x, y, and other glyph properties.
#' @template par-coloralpha
# @template par-coloralpha-extra
# @template par-hover
#' @template par-legend
#' @template par-lnamegroup
#' @template dots-fillline
#' @family layer functions
#' @note This function is included for completeness as it maps to Bokeh's \code{patch} glyph, but the same and more functionality can be obtained with \code{\link{ly_polygons}}.
#' @export
ly_patch <- function(
  fig,
  x, y = NULL, data = figure_data(fig),
  color = NULL, alpha = NULL,
  # hov_color = NULL, hov_alpha = NULL,
  # ns_color = NULL, ns_alpha = NULL,
  # sel_color = NULL, sel_alpha = NULL,
  # hover = NULL,
  legend = TRUE,
  lname = NULL, lgroup = NULL,
  ...
) {

  spec <- c(list(
    x = enquo(x), y = enquo(y),
    color = enquo(color), alpha = enquo(alpha),
    # hov_color = enquo(hov_color), hov_alpha = enquo(hov_alpha),
    # ns_color = enquo(ns_color), ns_alpha = enquo(ns_alpha),
    # sel_color = enquo(sel_color), sel_alpha = enquo(sel_alpha),
    # hover = enquo(hover),
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
#' @param x Values or field name / expression indicating location of center x coordinates.
#' @param y Values or field name / expression indicating location of center y coordinates.
#' @param data An optional data frame, providing the source for inputs x, y, and other glyph properties.
#' @param inner_radius Values or field name / expression indicating inner radii
#' @param outer_radius Values or field name / expression indicating outer radii
#' @param start_angle The angles to start the annular wedges, in radians, as measured from the horizontal.
#' @param end_angle The angles to end the annular wedges, in radians, as measured from the horizontal.
#' @param direction Direction to turn between starting and ending angles ("anticlock", "clock").
#' @template par-coloralpha
#' @template par-coloralpha-extra
#' @template par-hover
#' @template par-legend
#' @template par-lnamegroup
#' @template dots-fillline
#' @examples
#' rescale <- function(x)
#'   (x - min(x)) / diff(range(x))
#' figure() %>%
#'   ly_annular_wedge(Sepal.Length, Sepal.Width, data = iris,
#'     end_angle = rescale(Petal.Length) * 2 * pi, color = Species,
#'     inner_radius = 0.1, outer_radius = 0.15, alpha = 0.5,
#'     hover = iris)
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
  hover = NULL,
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
    hover = enquo(hover),
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
#' @param x Values or field name / expression indicating location of center x coordinates.
#' @param y Values or field name / expression indicating location of center y coordinates.
#' @param data An optional data frame, providing the source for inputs x, y, and other glyph properties.
#' @param inner_radius Values or field name / expression indicating inner radii.
#' @param outer_radius Values or field name / expression indicating outer radii.
#' @template par-coloralpha
# @template par-coloralpha-extra
# @template par-hover
#' @template par-legend
#' @template par-lnamegroup
#' @template dots-fillline
#' @examples
#' rescale <- function(x)
#'   (x - min(x)) / diff(range(x))
#' figure() %>%
#'   ly_annulus(Sepal.Length, Sepal.Width, data = iris,
#'     color = Species, hover = iris, alpha = 0.5,
#'     outer_radius = rescale(Petal.Length) * 0.3,
#'     inner_radius = rescale(Petal.Length) * 0.1)
#' @family layer functions
#' @export
ly_annulus <- function(
  fig,
  x, y = NULL, data = figure_data(fig),
  inner_radius = 0.1, outer_radius = 0.3,
  color = NULL, alpha = NULL,
  # hov_color = NULL, hov_alpha = NULL,
  # ns_color = NULL, ns_alpha = NULL,
  # sel_color = NULL, sel_alpha = NULL,
  # hover = NULL,
  legend = TRUE,
  lname = NULL, lgroup = NULL,
  ...
) {

  spec <- c(list(
    x = enquo(x), y = enquo(y),
    inner_radius = enquo(inner_radius), outer_radius = enquo(outer_radius),
    color = enquo(color), alpha = enquo(alpha),
    # hov_color = enquo(hov_color), hov_alpha = enquo(hov_alpha),
    # ns_color = enquo(ns_color), ns_alpha = enquo(ns_alpha),
    # sel_color = enquo(sel_color), sel_alpha = enquo(sel_alpha),
    # hover = enquo(hover),
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
#' @param x Values or field name / expression indicating location of center x coordinates.
#' @param y Values or field name / expression indicating location of center y coordinates.
#' @param data An optional data frame, providing the source for inputs x, y, and other glyph properties.
#' @template par-lineprops
# @template par-lineprops-extra
# @template par-hover
#' @param radius values or field name / expression indicating arc radii.
#' @param start_angle values or field name / expression indicating starting angles.
#' @param end_angle values or field name / expression indicating ending angles.
#' @param direction Direction to turn between starting and ending angles ("anticlock", "clock").
#' @template par-legend
#' @template par-lnamegroup
#' @template dots-line
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
  type = 1, width = 1,
  # hov_color = NULL, hov_alpha = NULL,
  # ns_color = NULL, ns_alpha = NULL,
  # sel_color = NULL, sel_alpha = NULL,
  # hover = NULL,
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
    width = enquo(width), type = enquo(type),
    # hov_color = enquo(hov_color), hov_alpha = enquo(hov_alpha),
    # ns_color = enquo(ns_color), ns_alpha = enquo(ns_alpha),
    # sel_color = enquo(sel_color), sel_alpha = enquo(sel_alpha),
    # hover = enquo(hover),
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
#' @param x Values or field name / expression indicating location of center x coordinates.
#' @param y Values or field name / expression indicating location of center y coordinates.
#' @param data An optional data frame, providing the source for inputs x, y, and other glyph properties.
#' @param radius Values or field name / expression indicating wedge radii.
#' @param start_angle The angles to start the wedges, in radians, as measured from the horizontal.
#' @param end_angle The angles to end the wedges, in radians, as measured from the horizontal.
#' @param direction Direction to turn between starting and ending angles ("anticlock", "clock").
#' @template par-coloralpha
#' @template par-coloralpha-extra
#' @template par-hover
#' @template par-legend
#' @template par-lnamegroup
#' @template dots-fillline
#' @examples
#' rescale <- function(x)
#'   (x - min(x)) / diff(range(x))
#' figure() %>%
#'   ly_wedge(Sepal.Length, Sepal.Width, data = iris,
#'     end_angle = rescale(Petal.Length) * 2 * pi, color = Species,
#'     radius = 0.15, alpha = 0.5,
#'     hov_line_color = "red",
#'     hov_line_width = 4,
#'     hover = iris)
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
  hover = NULL,
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
    hover = enquo(hover),
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
#' @param x0 Values or field name / expression indicating starting x coordinates.
#' @param y0 Values or field name / expression indicating starting y coordinates.
#' @param x1 Values or field name / expression indicating ending x coordinates.
#' @param y1 Values or field name / expression indicating ending y coordinates.
#' @param data An optional data frame, providing the source for inputs x, y, and other glyph properties.
#' @template par-lineprops
#' @template par-lineprops-extra
#' @template par-hover
#' @template par-legend
#' @template par-lnamegroup
#' @template dots-line
#' @family layer functions
#' @export
#' @examples
#' wa_cancer <- data.frame(
#'   LCL95.male = c(237, 233.1, 266, 219.8, 227.5, 239.7, 245.4, 237.5, 208, 216.2),
#'   UCL95.male = c(466, 471.6, 316.8, 347.2, 303, 283.4, 263.3, 268.1, 300.3, 290.5),
#'   rate.male = c(332, 329.8, 290.5, 276.4, 263, 260.8, 254.2, 252.4, 250.6, 250.3),
#'   county = c("Columbia", "Wahkiakum", "Grays Harbor", "Pend Oreille", "Franklin",
#'     "Cowlitz", "Pierce", "Thurston", "Klickitat", "Pacific"),
#'   stringsAsFactors = FALSE)
#' ## y axis sorted by male rate
#' ylim <- levels(with(wa_cancer, reorder(county, rate.male)))
#'
#' figure(ylim = ylim, data = wa_cancer) %>%
#'   ly_segments(LCL95.male, county, UCL95.male,
#'     county, width = 2, color = "steelblue") %>%
#'   ly_points(rate.male, county, glyph = 16, color = "steelblue", hover = wa_cancer)
ly_segments <- function(
  fig,
  x0, y0, x1, y1,
  data = figure_data(fig),
  type = 1, width = 1,
  color = NULL, alpha = NULL,
  hov_color = NULL, hov_alpha = NULL,
  ns_color = NULL, ns_alpha = NULL,
  sel_color = NULL, sel_alpha = NULL,
  hover = NULL,
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
    hover = enquo(hover),
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
#' @param x Values or field name / expression indicating location of center x coordinates.
#' @param y Values or field name / expression indicating location of center y coordinates.
#' @param data An optional data frame, providing the source for inputs x, y, and other glyph properties.
#' @param length alues or field name / expression indicating ray lengths in screen units
#' @param angle values or field name / expression indicating ray angles
#' @template par-lineprops
# @template par-lineprops-extra
#' @template par-legend
#' @template par-lnamegroup
#' @template dots-line
#' @family layer functions
#' @export
#' @examples
#' # this plot does not serve any practical purpose but illustrates ly_ray
#' figure() %>%
#'   ly_ray(Sepal.Length, Sepal.Width,
#'     data = iris, length = runif(150),
#'     angle = runif(150, max = 2 * pi),
#'     hover = iris,
#'     color = Species)
ly_ray <- function(
  fig,
  x, y = NULL, data = figure_data(fig),
  length = NULL, angle = NULL,
  type = 1, width = 1, color = NULL, alpha = NULL,
  # hover = NULL,
  # hov_color = NULL, hov_alpha = NULL,
  # ns_color = NULL, ns_alpha = NULL,
  # sel_color = NULL, sel_alpha = NULL,
  legend = TRUE, lname = NULL, lgroup = NULL,
  ...
) {

  spec <- c(list(
    x = enquo(x), y = enquo(y),
    length = enquo(length), angle = enquo(angle),
    line_width = enquo(width), line_dash = enquo(type),
    color = enquo(color), alpha = enquo(alpha),
    # hov_color = enquo(hov_color), hov_alpha = enquo(hov_alpha),
    # ns_color = enquo(ns_color), ns_alpha = enquo(ns_alpha),
    # sel_color = enquo(sel_color), sel_alpha = enquo(sel_alpha),
    # hover = enquo(hover),
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
#' @param x0 Values or field name / expression indicating starting x coordinates.
#' @param y0 Values or field name / expression indicating starting y coordinates.
#' @param x1 Values or field name / expression indicating ending x coordinates.
#' @param y1 Values or field name / expression indicating ending y coordinates.
#' @param cx0 Values or field name / expression indicating first control point x coordinates.
#' @param cy0 Values or field name / expression indicating first control point y coordinates.
#' @param cx1 Values or field name / expression indicating second control point x coordinates.
#' @param cy1 Values or field name / expression indicating second control point y coordinates.
#' @param data An optional data frame, providing the source for start, end, and control point intputs, as well as other glyph properties.
#' @template par-lineprops
# @template par-lineprops-extra
# @template par-hover
#' @template par-legend
#' @template par-lnamegroup
#' @template dots-line
#' @family layer functions
#' @export
#' @examples
#' # this plot does not serve any practical purpose but illustrates ly_bezier
#' figure() %>%
#'   ly_bezier(
#'     x0 = Sepal.Length,
#'     x1 = Sepal.Length + runif(150),
#'     cx0 = Sepal.Length + runif(150),
#'     cx1 = Sepal.Length + runif(150),
#'     y0 = Sepal.Width,
#'     y1 = Sepal.Width + runif(150),
#'     cy0 = Sepal.Width + runif(150),
#'     cy1 = Sepal.Width + runif(150),
#'     color = Species,
#'     data = iris,
#'     hover = iris)
ly_bezier <- function(
  fig,
  x0, y0, x1, y1, cx0, cy0, cx1, cy1,
  data = figure_data(fig),
  color = NULL, alpha = NULL,
  type = 1, width = 1,
  # hov_color = NULL, hov_alpha = NULL,
  # ns_color = NULL, ns_alpha = NULL,
  # sel_color = NULL, sel_alpha = NULL,
  # hover = NULL,
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
    # hov_color = enquo(hov_color), hov_alpha = enquo(hov_alpha),
    # ns_color = enquo(ns_color), ns_alpha = enquo(ns_alpha),
    # sel_color = enquo(sel_color), sel_alpha = enquo(sel_alpha),
    # hover = enquo(hover),
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
#' @param x0 Values or field name / expression indicating starting x coordinates.
#' @param y0 Values or field name / expression indicating starting y coordinates.
#' @param x1 Values or field name / expression indicating ending x coordinates.
#' @param y1 Values or field name / expression indicating ending y coordinates.
#' @param cx Values or field name / expression indicating control point x coordinates.
#' @param cy Values or field name / expression indicating control point y coordinates.
#' @param data an optional data frame, providing the source for start, end, and control point intputs, as well as other glyph properties
#' @template par-lineprops
#' @template par-lineprops-extra
#' @template par-legend
#' @template par-lnamegroup
#' @template dots-fillline
#' @family layer functions
#' @export
#' @examples
#' # this plot does not serve any practical purpose but illustrates ly_quadratic
#' figure() %>%
#'   ly_quadratic(
#'     x0 = Sepal.Length,
#'     x1 = Sepal.Length + runif(150),
#'     cx = Sepal.Length + runif(150),
#'     y0 = Sepal.Width,
#'     y1 = Sepal.Width + runif(150),
#'     cy = Sepal.Width + runif(150),
#'     color = Species,
#'     data = iris,
#'     hover = iris)
ly_quadratic <- function(
  fig,
  x0, y0, x1, y1, cx, cy,
  data = figure_data(fig),
  color = NULL, alpha = NULL,
  type = 1, width = 1,
  hov_color = NULL, hov_alpha = NULL,
  ns_color = NULL, ns_alpha = NULL,
  sel_color = NULL, sel_alpha = NULL,
  # hover = NULL,
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
    # hover = enquo(hover),
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
#' @param xs List of vectors of x coordinates.
#' @param ys List of vectors of y coordinates.
#' @template par-lineprops
#' @template par-lineprops-extra
#' @template par-lnamegroup
#' @template par-hover
#' @template par-legend
#' @template dots-line
#' @family layer functions
#' @export
#' @examples
#' xs <- list()
#' ys <- list()
#' for (i in 1:500) {
#'   count <- sample(1:10, 1)
#'   angles <- runif(count + 1, 0, 2 * pi)
#'   x_dists <- (1 / 2) ^ (0:count) * cos(angles)
#'   y_dists <- (1 / 2) ^ (0:count) * sin(angles)
#'   xs[[length(xs) + 1]] <- c(cumsum(x_dists))
#'   ys[[length(ys) + 1]] <- c(cumsum(y_dists))
#' }
#'
#' figure() %>%
#'   ly_multi_line(xs = xs, ys = ys, hover = data.frame(a = 1:500))
#'
#' figure() %>%
#'   ly_multi_line(xs = xs, ys = ys,
#'     color = sample(c("a", "b"), 500, replace = TRUE))
#'
#' figure() %>%
#'   ly_multi_line(xs = xs, ys = ys,
#'     color = asis(sample(c("red", "blue"), 500, replace = TRUE)))
ly_multi_line <- function(
  fig,
  xs = NULL, ys = NULL,
  # data = figure_data(fig),
  type = 1, width = 1, color = NULL, alpha = NULL,
  hov_color = NULL, hov_alpha = NULL,
  ns_color = NULL, ns_alpha = NULL,
  sel_color = NULL, sel_alpha = NULL,
  hover = NULL, legend = TRUE,
  lgroup = NULL, lname = NULL,
  ...
) {

  spec <- c(list(
    xs = enquo(xs), ys = enquo(ys), color = enquo(color), alpha = enquo(alpha),
    hov_color = enquo(hov_color), hov_alpha = enquo(hov_alpha),
    ns_color = enquo(ns_color), ns_alpha = enquo(ns_alpha),
    sel_color = enquo(sel_color), sel_alpha = enquo(sel_alpha),
    hover = enquo(hover),
    line_width = enquo(width), line_dash = enquo(type),
    model = "MultiLine",
    type = "glyph",
    data = NULL,
    hover = enquo(hover),
    legend = legend,
    use_all_data = FALSE), # for now (TODO)
    quos(...))

  add_layer(fig, spec, lgroup, lname)
}

#' Add a "polygons" layer to a Bokeh figure
#' @param fig Figure to modify.
#' @param xs Vector or list of values or field name of polygon x coordinates. See details.
#' @param ys Vector or list of values or field name of polygon y coordinates. See details.
# @param group vector or field name of grouping variable - see details.
#' @param data An optional data frame, providing the source for inputs xs, ys, group, and other glyph properties.
#' @param width Stroke width in units of pixels.
#' @param type An integer between 1 and 6 matching the \code{lty} property in \code{\link[graphics]{par}} or an array of integer pixel distances that describe the on-off pattern of dashing to use.
#' @template par-coloralpha
#' @template par-coloralpha-extra
#' @template par-hover
#' @template par-legend
#' @template par-lnamegroup
#' @template dots-fillline
#' @details \code{xs} and \code{ys} can be a list of vectors, each element for one polygon to be drawn, or can be vectors with the \code{group} argument specifying how to break them up into individual polygons.
#' @family layer functions
#' @export
#' @examples
#' xs <- list()
#' ys <- list()
#' for (i in 1:500) {
#'   count <- sample(1:10, 1)
#'   angles <- runif(count + 1, 0, 2 * pi)
#'   x_dists <- (1 / 2) ^ (0:count) * cos(angles)
#'   y_dists <- (1 / 2) ^ (0:count) * sin(angles)
#'   xs[[length(xs) + 1]] <- c(cumsum(x_dists))
#'   ys[[length(ys) + 1]] <- c(cumsum(y_dists))
#' }
#'
#' figure() %>%
#'   ly_polygons(xs = xs, ys = ys, hover = data.frame(a = 1:500))
#'
#' figure(xaxes = FALSE, yaxes = FALSE, xgrid = FALSE, ygrid = FALSE, tools = NULL) %>%
#'   ly_polygons(xs = xs, ys = ys,
#'     color = sample(c("a", "b", "c", "d"), 500, replace = TRUE), legend = FALSE,
#'     line_alpha = 0.5)
#'
#' figure() %>%
#'   ly_polygons(xs = xs, ys = ys,
#'     color = asis(sample(c("red", "blue"), 500, replace = TRUE)))
ly_polygons <- function(
  fig,
  xs = NULL, ys = NULL,
  data = figure_data(fig),
  type = 1, width = 1, color = NULL, alpha = NULL,
  hover = NULL, legend = TRUE,
  hov_color = NULL, hov_alpha = NULL,
  ns_color = NULL, ns_alpha = NULL,
  sel_color = NULL, sel_alpha = NULL,
  lgroup = NULL, lname = NULL,
  ...) {

  # TODO: make it work with 'group'

  spec <- c(list(
    xs = enquo(xs), ys = enquo(ys), color = enquo(color), alpha = enquo(alpha),
    hov_color = enquo(hov_color), hov_alpha = enquo(hov_alpha),
    ns_color = enquo(ns_color), ns_alpha = enquo(ns_alpha),
    sel_color = enquo(sel_color), sel_alpha = enquo(sel_alpha),
    hover = enquo(hover),
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
#' @param z A matrix or vector of image values.
#' @param rows If \code{z} is a vector, how many rows should be used in treating it as a matrix.
#' @param byrow If \code{z} is a vector, should it be turned into a matrix by row.
#' @param x Lower left x coordinates.
#' @param y Lower left y coordinates.
#' @param dw Image width distances.
#' @param dh Image height distances.
#' @param palette Name of color palette to use for color ramp (see \href{http://bokeh.pydata.org/en/latest/docs/reference/palettes.html}{here} for acceptable values).
#' @param dilate Logical indicating whether to dilate pixel distance computations when drawing.
#' @template par-lnamegroup
# @example man-roxygen/ex-image.R
#' @family layer functions
#' @export
#' @examples
#' figure() %>%
#'   ly_image(volcano)
ly_image <- function(
  fig,
  z,
  rows, byrow = TRUE, x = 0, y = 0, dw = 1, dh = 1,
  palette = "Spectral10", dilate = FALSE,
  lname = NULL, lgroup = NULL) {

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
    use_all_data = FALSE)) # for now (TODO)

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
#' @param x The image x coordinates.
#' @param y The image y coordinates.
#' @param data An optional data frame, providing the source for inputs x, y, and other properties.
#' @param w,h Values or field names of width and height of image.
#' @param w_units,h_units One of "screen", "data".
#' @param url Values or field name of image URLs.
#' @param dilate Logical indicating whether to always round fractional pixel locations in such a way as to make the images bigger. This setting may be useful if pixel rounding errors are causing images to have a gap between them, when they should appear flush.
#' @param anchor Where the image is anchored to with respect to \code{x} and \code{y}. One of 'top_left', 'top_center', 'top_right', 'center_left', 'center', 'center_right', 'bottom_left', 'bottom_center', 'bottom_right'.
#' @param angle Values or field name / expression indicating the angle to rotate the image, in radians.
#' @param global_alpha An overall opacity that each image is rendered with (in addition to any inherent alpha values in the image itself).
#' @param retry_attempts Number of attempts to retry loading the images from the specified URL.
#' @param retry_timeout Timeout (in ms) between retry attempts to load the image from the specified URL.
#' @template par-lnamegroup
#' @family layer functions
#' @export
#' @examples
#' url <- c("  http://bokeh.pydata.org/en/latest/_static/images/logo.png",
#'   "http://developer.r-project.org/Logo/Rlogo-4.png")
#' ss <- seq(0, 2 * pi, length = 13)[-1]
#' ws <- runif(12, 2.5, 5) * rep(c(1, 0.8), 6)
#' imgdat <- data.frame(
#'   x = sin(ss) * 10, y = cos(ss) * 10,
#'   w = ws, h = ws * rep(c(1, 0.76), 6),
#'   url = rep(url, 6),
#'   angle = runif(12, 0, 2 * pi))
#'
#' figure(xlab = "x", ylab = "y", xlim = c(-12, 12), ylim = c(-12, 12)) %>%
#'   ly_image_url(x, y, w = w, h = h, url = url, data = imgdat,
#'     anchor = "center", global_alpha = 0.5)
ly_image_url <- function(
  fig, x = NULL, y = NULL,
  url,
  w = 10, h = 10,
  data = figure_data(fig),
  dilate = TRUE, anchor = "top_left", angle = 0,
  w_units = "screen", h_units = "screen", global_alpha = 1,
  retry_attempts = 0, retry_timeout = 0,
  lgroup = NULL, lname = NULL) {

  spec <- c(list(
    x = enquo(x), y = enquo(y), url = enquo(url),
    w = enquo(w), h = enquo(h), angle = enquo(angle), dilate = dilate, anchor = anchor,
    global_alpha = global_alpha,
    retry_attempts = retry_attempts, retry_timeout = retry_timeout,
    model = "ImageURL",
    type = "glyph",
    data = data,
    legend = legend,
    use_all_data = FALSE)) # for now (TODO)

  add_layer(fig, spec, lgroup, lname)
}

# ly_ellipse <- function() {

# }
