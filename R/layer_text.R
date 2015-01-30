

# x The x-coordinates to locate the text anchors.
# y The y-coordinates to locate the text anchors.
# text The text values to render.
# align The text align values for the text ("left", "right", "center")
# alpha The text alpha values for the text.
# baseline The text baseline values for the text ("top", "middle", "bottom", "alphabetic", "hanging")
# color The text color values for the text.
# font The text font values for the text.
# font_size The text font size values for the text.
# font_style The text font style values for the text ("normal", "italic", "bold")
# x_offset Offset values to apply to the x-coordinates.
# y_offset Offset values to apply to the y-coordinates.

#' @export
ly_text <- function(fig, x, y = NULL, text = NULL, data = NULL,
  color = "black", alpha = NULL,
  angle = 0, align = NULL, baseline = NULL,
  font = NULL, font_size = NULL, font_style = NULL,
  x_offset = NULL, y_offset = NULL,
  legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validateFig(fig, "ly_text")
  ## see if any options won't be used and give a message
  checkOpts(list(...), "text")

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
  xyNames <- getXYNames(x, y, xname, yname, list(...))
  xy <- getXYData(x, y)
  lgroup <- getLgroup(lgroup, fig)

  args <- list(glyph = "text", ...)
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

  axisTypeRange <- getGlyphAxisTypeRange(xy$x, xy$y)
  make_glyph(fig, type = "text", lname = lname, lgroup = lgroup,
    data = c(xy, list(text = text, angle = angle)),
    legend = legend,
    xname = xyNames$x, yname = xyNames$y,
    args = args, axisTypeRange = axisTypeRange)
}
