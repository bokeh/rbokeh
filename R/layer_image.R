#' @export
lay_image <- function(fig, image, rows, cols, x = 0, y = 0, dw = 1, dh = 1, palette = "Spectral-10", hover = NULL, legend = NULL, lname = NULL, lgroup = NULL, ...) {

  ##### boilerplate
  validateFig(fig, "lay_image")
  ## see if any options won't be used and give a message
  checkOpts(list(...), "image")

  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    x          <- v_eval(substitute(x), data)
    y          <- v_eval(substitute(y), data)
    size       <- v_eval(substitute(size), data)
    glyph      <- v_eval(substitute(glyph), data)
    color      <- v_eval(substitute(color), data)
    line_color <- v_eval(substitute(line_color), data)
    fill_color <- v_eval(substitute(fill_color), data)
  }

  hover <- getHover(substitute(hover), data)
  xyNames <- getXYNames(x, y, xname, yname, list(...))
  ## translate different x, y types to vectors
  xy <- getXYData(x, y)
  lgroup <- getLgroup(lgroup, fig)
  ##### boilerplate

  axisTypeRange <- getGlyphAxisTypeRange(c(x, dw), c(y, dh), assertX = "numeric", assertY = "numeric")

  if(is.matrix(image)) {
    cols <- nrow(image)
    rows <- ncol(image)
    image <- array(image)
  }

  make_glyph(fig, type = "image", lname = lname, lgroup = lgroup,
    data = list(image = list(image), rows = rows, cols = cols, x = x, y = y, dw = dw, dh = dh, palette = palette),
    args = list(...), axisTypeRange = axisTypeRange)
}

# lay_image_rgba <- function(fig, image, rows, cols, x = 0, y = 0, dw = 1, dh = 1, lname = NULL, lgroup = NULL, ...) {
#   axisTypeRange <- getGlyphAxisTypeRange(c(x, dw), c(y, dh), assertX = "numeric", assertY = "numeric")

#   if(is.matrix(image)) {
#     cols <- nrow(image)
#     rows <- ncol(image)
#     image <- array(image)
#   }

#   make_glyph(fig, type = "image_RGBA", lname = lname, lgroup = lgroup,
#     data = list(image = list(image), rows = rows, cols = cols, x = x, y = y, dw = dw, dh = dh),
#     args = list(...), axisTypeRange = axisTypeRange)
# }

# lay_image_url <- function(fig, x, y, url, angle = 0, lname = NULL, lgroup = NULL, ...) {
#   # can this have "categorical" axes?
#   axisTypeRange <- getGlyphAxisTypeRange(x, y)
#   make_glyph(fig, type = "image_URL", lname = lname, lgroup = lgroup,
#     data = list(x = x, y = y, url = url, angle = angle),
#     args = list(...), axisTypeRange = axisTypeRange)
# }
