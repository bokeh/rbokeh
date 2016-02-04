
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
ly_text <- function(
  fig,
  x, y = NULL,
  text = NULL, data = figure_data(fig),
  color = "black", alpha = 1,
  angle = 0, align = NULL, baseline = NULL,
  font = NULL, font_size = NULL, font_style = NULL,
  x_offset = NULL, y_offset = NULL,
  legend = NULL, lname = NULL, lgroup = NULL, visible = TRUE
) {

  validate_fig(fig, "ly_text")

  args <- sub_names(fig, data,
    grab(
      x, y, text,
      color,
      alpha,
      angle,
      align,
      baseline,
      font,
      font_size,
      font_style,
      x_offset,
      y_offset,
      legend,
      lname,
      lgroup,
      visible,
      dots = lazy_dots()
    )
  )

  arg_params <- list(glyph = "text")

  arg_params$glyph <- "text"
  arg_params$text_color <- args$params$color
  arg_params$angle <- args$params$angle
  arg_params$text_align <- args$params$align
  arg_params$text_alpha <- args$params$alpha
  arg_params$text_baseline <- args$params$baseline
  arg_params$text_font <- args$params$font
  arg_params$text_font_size <- args$params$font_size
  arg_params$text_font_style <- args$params$font_style
  arg_params$x_offset <- args$params$x_offset
  arg_params$y_offset <- args$params$y_offset

  if(is.null(args$params$text)) {
    args$params$text <- seq_along(args$data$x)
  }

  axis_type_range <- get_glyph_axis_type_range(args$data$x, args$data$y)

  mc <- lapply(match.call(), deparse)

  make_glyph(
    fig, type = "text",
    lname = args$info$lname, lgroup = args$info$lgroup,
    data = c(args$data, list(text = args$params$text, angle = args$params$angle)),
    legend = args$info$legend,
    xname = args$info$x_name, yname = args$info$y_name,
    args = arg_params, axis_type_range = axis_type_range,
    ly_call = mc
  )
}
