

#' @export
lay_text <- function(fig, x, y = NULL, text = NULL, data = NULL, text_color = "black", angle = 0, name = NULL, ...) {

  if(!is.null(data)) {
    x           <- getVarData(data, substitute(x))
    y           <- getVarData(data, substitute(y))
    text        <- getVarData(data, substitute(text))
    text_color  <- getVarData(data, substitute(text_color))
    angle       <- getVarData(data, substitute(angle))
  }
  xy <- getXYData(x, y)

  opts <- list(...)
  opts$text_color <- text_color

  if(is.null(text))
    text <- seq_along(xy$x)
  
  axisTypeRange <- getGlyphAxisTypeRange(xy$x, xy$y)
  makeGlyph(fig, type = "text", name = name,
    data = c(xy, list(text = text, angle = angle)),
    args = opts, axisTypeRange = axisTypeRange)
}

#' @export
lay_annular_wedge <- function(fig, x, y, inner_radius = 0.3, outer_radius = 0.7, start_angle = 0, end_angle = 2*pi, direction = "anticlock", name = NULL, ...) {
  checkArcDirection(direction)
  axisTypeRange <- getGlyphAxisTypeRange(x, y, assertX = "numeric", assertY = "numeric")
  makeGlyph(fig, type = "annular_wedge", name = name,
    data = list(x = x, y = y, inner_radius = inner_radius, outer_radius = outer_radius, start_angle = start_angle, end_angle = end_angle, direction = direction),
    args = list(...), axisTypeRange = axisTypeRange)
}

#' @export
lay_annulus <- function(fig, x, y, 
  inner_radius = 0.3, outer_radius = 0.7, 
  name = NULL, ...) {
  axisTypeRange <- getGlyphAxisTypeRange(x, y, assertX = "numeric", assertY = "numeric")
  makeGlyph(fig, type = "annulus", name = name,
    data = list(x = x, y = y, inner_radius = inner_radius, outer_radius = outer_radius),
    args = list(...), axisTypeRange = axisTypeRange)
}

#' @export
lay_arc <- function(fig, x, y, radius = 0.5, start_angle = 0, end_angle = 2*pi, direction = "anticlock", name = NULL, ...) {
  checkArcDirection(direction)
  axisTypeRange <- getGlyphAxisTypeRange(x, y, assertX = "numeric", assertY = "numeric")
  makeGlyph(fig, type = "arc", name = name,
    data = list(x = x, y = y, radius = radius, start_angle = start_angle, end_angle = end_angle, direction = direction),
    args = list(...), axisTypeRange = axisTypeRange)
}

#' @export
lay_bezier <- function(fig, x0, y0, x1, y1, cx0, cy0, cx1, cy1, name = NULL, ...) {
  axisTypeRange <- getGlyphAxisTypeRange(c(x0, x1), c(y0, y1), assertX = "numeric", assertY = "numeric")
  makeGlyph(fig, type = "bezier", name = name,
    data = list(x0 = x0, y0 = y0, x1 = x1, y1 = y1, cx0 = cx0, cy0 = cy0, cx1 = cx1, cy1 = cy1),
    args = list(...), axisTypeRange = axisTypeRange)
}

#' @export
lay_image <- function(fig, image, rows, cols, x = 0, y = 0, dw = 1, dh = 1, palette = "Spectral-10", name = NULL, ...) {
  axisTypeRange <- getGlyphAxisTypeRange(c(x, dw), c(y, dh), assertX = "numeric", assertY = "numeric")

  if(is.matrix(image)) {
    cols <- nrow(image)
    rows <- ncol(image)
    image <- array(image)
  }
  makeGlyph(fig, type = "image", name = name,
    data = list(image = image, rows = rows, cols = cols, x = x, y = y, dw = dw, dh = dh, palette = palette),
    args = list(...), axisTypeRange = axisTypeRange)
}

#' @export
lay_image_rgba <- function(fig, image, rows, cols, x, y, dw, dh, name = NULL, ...) {
  axisTypeRange <- getGlyphAxisTypeRange(c(x, dw), c(y, dh), assertX = "numeric", assertY = "numeric")
  makeGlyph(fig, type = "image", name = name,
    data = list(image = image, rows = rows, cols = cols, x = x, y = y, dw = dw, dh = dh),
    args = list(...), axisTypeRange = axisTypeRange)
}

#' @export
lay_image_url <- function(fig, x, y, url, angle = 0, name = NULL, ...) {
  # can this have "categorical" axes?
  axisTypeRange <- getGlyphAxisTypeRange(x, y)
  makeGlyph(fig, type = "image_url", name = name,
    data = list(x = x, y = y, url = url, angle = angle),
    args = list(...), axisTypeRange = axisTypeRange)
}

#' @export
lay_oval <- function(fig, x, y, width, height, angle, name = NULL, ...) {
  axisTypeRange <- getGlyphAxisTypeRange(x, y)
  makeGlyph(fig, type = "oval", name = name,
    data = list(x = x, y = y, width = width, height = height, angle = angle), args = list(...), axisTypeRange = axisTypeRange)
}

#' @export
lay_quadratic <- function(fig, x0, y0, x1, y1, cx, cy, name = NULL, ...) {
  axisTypeRange <- getGlyphAxisTypeRange(c(x0, x1), c(y0, y1), assertX = "numeric", assertY = "numeric")
  makeGlyph(fig, type = "quadratic", name = name,
    data = list(x0 = x0, y0 = y0, x1 = x1, y1 = y1, cx = cx, cy = cy),
    args = list(...), axisTypeRange = axisTypeRange)
}

#' @export
lay_ray <- function(fig, x, y, length, angle = 0, name = NULL, ...) {
  axisTypeRange <- getGlyphAxisTypeRange(x, y)
  makeGlyph(fig, type = "ray", name = name,
    data = list(x = x, y = y, length = length, angle = angle),
    args = list(...), axisTypeRange = axisTypeRange)
}

# centered rect


#' @export
lay_wedge <- function(fig, x, y, radius = 0.7, start_angle = 0, end_angle = 2*pi, direction = "anticlock", name = NULL, ...) {
  checkArcDirection(direction)
  axisTypeRange <- getGlyphAxisTypeRange(x, y, assertX = "numeric", assertY = "numeric")
  makeGlyph(fig, type = "wedge", name = name,
    data = list(x = x, y = y, radius = radius, start_angle = start_angle, end_angle = end_angle, direction = direction),
    args = list(...), axisTypeRange = axisTypeRange)
}


