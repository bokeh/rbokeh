
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
  # visible = TRUE,
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
    data = data,
    legend = legend,
    use_all_data = FALSE), # for now (TODO)
    quos(...))

  add_layer(fig, spec, lgroup, lname)
}
