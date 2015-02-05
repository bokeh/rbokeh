#' @export
ly_image <- function(fig, image, rows, cols, x = 0, y = 0, dw = 1, dh = 1, palette = "Spectral-10", hover = NULL, legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validate_fig(fig, "ly_image")
  ## see if any options won't be used and give a message
  check_opts(list(...), "image")

  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))

  xy_names <- get_xy_names(NULL, NULL, "x", "y", list(...))
  ## translate different x, y types to vectors
  lgroup <- get_lgroup(lgroup, fig)

  axis_type_range <- get_glyph_axis_type_range(c(x, dw), c(y, dh), assert_x = "numeric", assert_y = "numeric")

  if(is.matrix(image)) {
    cols <- nrow(image)
    rows <- ncol(image)
    image <- array(image)
  }

  make_glyph(fig, type = "image", lname = lname, lgroup = lgroup,
    data = list(image = list(image), rows = rows, cols = cols, x = x, y = y, dw = dw, dh = dh, palette = palette),
    args = list(...), axis_type_range = axis_type_range)
}

#' @export
ly_image_url <- function(fig, x = 0, y = 0, data = NULL, w = 10, h = 10,
  url, dilate = TRUE, anchor = "top_left", angle = 0,
  lname = NULL, lgroup = NULL, ...) {

  validate_fig(fig, "ly_image_url")

  anchor_opts <- c("top_left", "top_center", "top_right", "right_center",
    "bottom_right", "bottom_center", "bottom_left", "left_center", "center")
  if(! anchor %in% anchor_opts)
    stop("anchor must be one of: ", paste(anchor_opts, collapse = ", "), call. = FALSE)

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

  hover <- get_hover(substitute(hover), data)
  xy_names <- get_xy_names(x, y, xname, yname, list(...))
  ## translate different x, y types to vectors
  xy <- get_xy_data(x, y)
  lgroup <- get_lgroup(lgroup, fig)

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
  axis_type_range <- get_glyph_axis_type_range(c(x, x2), c(y, y2))

  make_glyph(fig, type = "image_URL",
    lname = lname, lgroup = lgroup,
    data = xy, args = args,
    axis_type_range = axis_type_range)
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

