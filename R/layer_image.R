#' @export
ly_image <- function(fig, image, rows, cols, x = 0, y = 0, dw = 1, dh = 1, palette = "Spectral-10", hover = NULL, legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validateFig(fig, "ly_image")
  ## see if any options won't be used and give a message
  checkOpts(list(...), "image")

  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))

  xyNames <- getXYNames(NULL, NULL, "x", "y", list(...))
  ## translate different x, y types to vectors
  lgroup <- getLgroup(lgroup, fig)

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

#' @export
ly_image_url <- function(fig, x = 0, y = 0, data = NULL, w = 10, h = 10,
  url, dilate = TRUE, anchor = "top_left", angle = 0,
  lname = NULL, lgroup = NULL, ...) {

  validateFig(fig, "ly_image_url")

  anchorOpts <- c("top_left", "top_center", "top_right", "right_center",
    "bottom_right", "bottom_center", "bottom_left", "left_center", "center")
  if(! anchor %in% anchorOpts)
    stop("anchor must be one of: ", paste(anchorOpts, collapse = ", "), call. = FALSE)

  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    x      <- v_eval(substitute(x), data)
    y      <- v_eval(substitute(y), data)
    url    <- v_eval(substitute(url), data)
    w      <- v_eval(substitute(w), data)
    h      <- v_eval(substitute(h), data)
    anchor <- v_eval(substitute(anchor), data)
    angle  <- v_eval(substitute(angle), data)
  }

  hover <- getHover(substitute(hover), data)
  xyNames <- getXYNames(x, y, xname, yname, list(...))
  ## translate different x, y types to vectors
  xy <- getXYData(x, y)
  lgroup <- getLgroup(lgroup, fig)

  args <- list(url = url, w = w, h = h, anchor = anchor,
    angle = angle, dilate = dilate, ...)

  ## range stuff
  if(grepl("left", anchor)) {
    x2 <- max(x + w)
  } else if(grepl("right", anchor)) {
    x2 <- min(x - w)
  } else if(anchor %in% c("top_center", "bottom_center", "center")) {
    x2 <- range(x + c(-1, 1) * w / 2)
  }
  if(grepl("top", anchor)) {
    y2 <- min(y - h)
  } else if(grepl("bottom", anchor)) {
    y2 <- max(y + h)
  } else if(anchor %in% c("left_center", "right_center", "center")) {
    y2 <- range(y + c(-1, 1) * h / 2)
  }
  # can this have "categorical" axes?
  axisTypeRange <- getGlyphAxisTypeRange(c(x, x2), c(y, y2))

  make_glyph(fig, type = "image_URL",
    lname = lname, lgroup = lgroup,
    data = xy, args = args,
    axisTypeRange = axisTypeRange)
}


# ly_image_rgba <- function(fig, image, rows, cols, x = 0, y = 0, dw = 1, dh = 1, lname = NULL, lgroup = NULL, ...) {
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

