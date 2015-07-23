
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
#' @template par-legend
#' @template par-lnamegroup
#' @example man-roxygen/ex-elements.R
#' @family layer functions
#' @export
ly_text <- function(fig, x, y = NULL, text = NULL, data = NULL,
  color = "black", alpha = 1,
  angle = 0, align = NULL, baseline = NULL,
  font = NULL, font_size = NULL, font_style = NULL,
  x_offset = NULL, y_offset = NULL,
  legend = NULL, lname = NULL, lgroup = NULL) {

  validate_fig(fig, "ly_text")

  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))

  if(!is.null(data)) {
    x           <- v_eval(substitute(x), data)
    y           <- v_eval(substitute(y), data)
    text        <- v_eval(substitute(text), data)
    color       <- v_eval(substitute(color), data)
    angle       <- v_eval(substitute(angle), data)
    font_size   <- v_eval(substitute(font_size), data)
  }
  xy_names <- get_xy_names(x, y, xname, yname, NULL)
  xy <- get_xy_data(x, y)
  lgroup <- get_lgroup(lgroup, fig)

  args <- list(glyph = "text")
  args$text_color <- color
  args$angle <- angle
  args$text_align <- align
  args$text_alpha <- alpha
  args$text_baseline <- baseline
  args$text_font <- font
  args$text_font_size <- font_size
  args$text_font_style <- font_style
  args$x_offset <- x_offset
  args$y_offset <- y_offset

  if(is.null(text))
    text <- seq_along(xy$x)

  axis_type_range <- get_glyph_axis_type_range(xy$x, xy$y)

  mc <- lapply(match.call(), deparse)

  make_glyph(fig, type = "text", lname = lname, lgroup = lgroup,
    data = c(xy, list(text = text, angle = angle)),
    legend = legend,
    xname = xy_names$x, yname = xy_names$y,
    args = args, axis_type_range = axis_type_range,
    ly_call = mc)
}
