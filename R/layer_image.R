
#' Add an "image" layer to a Bokeh figure
#'
#' Draws a grid of rectangles with colors corresponding to the values in \code{z}
#' @param fig figure to modify
#' @param z matrix or vector of image values
#' @param rows if \code{z} is a vector, how many rows should be used in treating it as a matrix
#' @param cols if \code{z} is a vector, how many columns should be used in treating it as a matrix
#' @param x lower left x coordinates
#' @param y lower left y coordinates
#' @param dw image width distances
#' @param dh image height distances
#' @param palette name of color palette to use for color ramp (see \href{http://bokeh.pydata.org/en/latest/docs/reference/palettes.html}{here} for acceptable values)
#' @param dilate logical - whether to dilate pixel distance computations when drawing
#' @template par-lnamegroup
#' @example man-roxygen/ex-image.R
#' @family layer functions
#' @export
ly_image <- function(fig, z, rows, cols, x = 0, y = 0, dw = 1, dh = 1,
  palette = "Spectral10", dilate = FALSE,
  lname = NULL, lgroup = NULL) {

  validate_fig(fig, "ly_image")
  ## see if any options won't be used and give a message
  # check_opts(list(...), "image")

  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))

  lgroup <- get_lgroup(lgroup, fig)

  axis_type_range <- get_glyph_axis_type_range(c(x, dw), c(y, dh), assert_x = "numeric", assert_y = "numeric")

  if(is.matrix(z)) {
    cols <- nrow(z)
    rows <- ncol(z)
    z <- c(z)  # coerce the matrix to a vector
  }

  # really ugly nested if else
  # palette checker / transformer from layer_hexbin minus function
  #   plus added check for length 1
  if( is.character(palette) && length(palette) == 1 ) {
    if(valid_color(palette)) {
      stop("'palette' specified in ly_image is a single color; please supply a vector of colors or name of a bokeh palette - see here: http://bokeh.pydata.org/en/latest/docs/reference/palettes.html", call. = FALSE)
    } else {
      if(!palette %in% bk_gradient_palette_names){
        stop("'palette' specified in ly_image is not a valid color name or palette - see here: http://bokeh.pydata.org/en/latest/docs/reference/palettes.html", call. = FALSE)
      } else {
        palette <- bk_gradient_palettes[[palette]]
      }
    }
  } else if( is.character(palette) && length(palette) > 1 ) {
    # check for valid colors in the palette
    if(!valid_color(palette)){
      stop("'palette' specified in ly_image is not a valid color name or palette - see here: http://bokeh.pydata.org/en/latest/docs/reference/palettes.html", call. = FALSE)
    }
  } else {
    stop("'palette' specified in ly_image is not a valid color name or palette - see here: http://bokeh.pydata.org/en/latest/docs/reference/palettes.html", call. = FALSE)
  }

  mc <- lapply(match.call(), deparse)

  make_glyph(fig, type = "image", lname = lname, lgroup = lgroup,
    data = list(image = list(z), rows = rows, cols = cols,
      x = x, y = y, dw = dw, dh = dh, palette = palette, dilate = dilate),
    args = NULL, axis_type_range = axis_type_range, ly_call = mc)
}

#' Add an "image_url" layer to a Bokeh figure
#'
#' Renders raster images from URLs at provided coordinates
#' @param fig figure to modify
#' @param x x coordinates
#' @param y y coordinates
#' @param data an optional data frame, providing the source for inputs x, y, and other properties
#' @param w,h values or field names of width and height of image
#' @param url values or field name of image URLs
#' @param dilate logical - whether to dilate pixel distance computations when drawing
#' @param anchor where the image is anchored to with respect to \code{x} and \code{y}
#' @param angle values or field name of the angle to rotate the image, in radians
#' @template par-lnamegroup
#' @family layer functions
#' @example man-roxygen/ex-image_url.R
#' @export
ly_image_url <- function(fig, x = 0, y = 0, data = NULL, w = 10, h = 10,
  url, dilate = TRUE, anchor = "top_left", angle = 0,
  lname = NULL, lgroup = NULL) {

  validate_fig(fig, "ly_image_url")

  anchor_opts <- c("top_left", "top_center", "top_right", "right_center",
    "bottom_right", "bottom_center", "bottom_left", "left_center", "center")
  if(! anchor %in% anchor_opts)
    stop("anchor must be one of: ", paste(anchor_opts, collapse = ", "), call. = FALSE)

  if(missing(x)) {
    xname <- "x"
  } else {
    xname <- deparse(substitute(x))
  }
  if(missing(y)) {
    yname <- "y"
  } else {
    yname <- deparse(substitute(y))
  }

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    x      <- v_eval(substitute(x), data)
    y      <- v_eval(substitute(y), data)
    url    <- v_eval(substitute(url), data)
    w      <- v_eval(substitute(w), data)
    h      <- v_eval(substitute(h), data)
    angle  <- v_eval(substitute(angle), data)
  }

  xy_names <- get_xy_names(x, y, xname, yname, NULL)
  ## translate different x, y types to vectors
  xy <- get_xy_data(x, y)
  lgroup <- get_lgroup(lgroup, fig)

  args <- list(url = url, w = w, h = h, anchor = anchor,
    angle = angle, dilate = dilate)

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
  axis_type_range <- get_glyph_axis_type_range(c(x, x2), c(y, y2))

  mc <- lapply(match.call(), deparse)

  make_glyph(fig, type = "image_URL",
    xname = xy_names$x, yname = xy_names$y,
    lname = lname, lgroup = lgroup,
    data = xy, args = args,
    axis_type_range = axis_type_range, ly_call = mc)
}


# ly_image_rgba <- function(fig, image, rows, cols, x = 0, y = 0, dw = 1, dh = 1, lname = NULL, lgroup = NULL, ...) {
#   axis_type_range <- get_glyph_axis_type_range(c(x, dw), c(y, dh), assert_x = "numeric", assert_y = "numeric")

#   if(is.matrix(image)) {
#     cols <- nrow(image)
#     rows <- ncol(image)
#     image <- array(image)
#   }

#   make_glyph(fig, type = "image_RGBA", lname = lname, lgroup = lgroup,
#     data = list(image = list(image), rows = rows, cols = cols, x = x, y = y, dw = dw, dh = dh),
#     args = list(...), axis_type_range = axis_type_range)
# }

