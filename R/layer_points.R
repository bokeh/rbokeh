# major attribute specification:
# type: what type of glyph to plot at each point
# type can be any of the following:
# 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25
# these match R's pch setting (see point_types())
# except 11 and 14 are missing, and 16, 19, 20 are the same
# or asterisk, circle, circle_cross, circle_x, cross, diamond, diamond_cross, inverted_triangle, square, square_cross, square_x, triangle, x
# the integer-based types simply map to any of these named types but with different line and/or fill properties

#' Add a "points" layer to a Bokeh figure
#' @param fig figure to modify
#' @param x values or field name of center x coordinates
#' @param y values or field name of center y coordinates
#' @param data an optional data frame, providing the source for inputs x, y, and other glyph properties
#' @param glyph value(s) or field name of the glyph to use (see \code{\link{point_types}})
#' @template par-coloralpha
#' @param size size of the glyph in screen units
#' @template par-hover
#' @template par-url
#' @template par-legend
#' @template par-lnamegroup
#' @template dots-fillline
#' @example man-roxygen/ex-points.R
#' @example man-roxygen/ex-lines.R
#' @family layer functions
#' @export
# @example
# ir <- iris
# ir$glyph_val <- as.numeric(ir$Species)
# ir$glyph_col <- c("red", "green", "blue")[ ir$glyph_val ]
ly_points <- function(
  fig, x, y = NULL, data = figure_data(fig),
  glyph = 21, color = NULL, alpha = 1, size = 10,
  hover = NULL, url = NULL, legend = NULL,
  lname = NULL, lgroup = NULL, visible = TRUE,
  ...
) {

  validate_fig(fig, "ly_points")

  mc <- attr(fig, "ly_call")
  if(is.null(mc))
    mc <- lapply(match.call(), deparse)

  args <- sub_names(fig, data,
    grab(
      x,
      y,
      glyph,
      color,
      alpha,
      size,
      hover,
      url,
      legend,
      lname,
      lgroup,
      visible,
      dots = lazy_dots(...)
    )
  )
  if(is.null(args$params$glyph)) {
    args$params$glyph <- "circle"
  }

  # #TODO
  # # if color wasn't specified, it should be the same for all glyphs
  # if(length(unique(args$glyph)) != 1) {
  #   # if color wasn't specified, it should be the same for all glyphs
  #   if(is.null(args$color))
  #     args$color <- fig$x$spec$theme[["discrete"]][["fill_color"]](1)
  # }

  if(length(args$params$glyph) == 1) {
    args$params$glyph <- rep(args$params$glyph, length(args$data$x))
  }
  if(is.character(args$params$glyph)) {
    args$params$glyph <- factor(args$params$glyph)
  }

  # split data up for each glyph
  split_list <- split(seq_along(args$params$glyph), args$params$glyph)
  for (ii in seq_along(split_list)) {
    arg_obj <- subset_arg_obj(args, split_list[[ii]])

    arg_obj$params$glyph <- arg_obj$params$glyph[1]

    arg_obj$params <- resolve_color_alpha(
      arg_obj$params,
      has_line = TRUE, has_fill = TRUE,
      ly    = fig$x$spec$layers[[arg_obj$info$lgroup]],
      solid = arg_obj$params$glyph %in% as.character(15:20),
      theme = fig$x$spec$theme
    )

    arg_obj$params <- resolve_glyph_props(arg_obj$params$glyph, arg_obj$params, arg_obj$info$lgroup)

    ## see if any options won't be used and give a message
    if(valid_glyph(arg_obj$params$glyph)) {
      check_opts(arg_obj$params, arg_obj$params$glyph, formals = names(formals(ly_points)))
    }

    axis_type_range <- get_glyph_axis_type_range(arg_obj$data$x,
      arg_obj$data$y, glyph = arg_obj$params$glyph)

    fig <- make_glyph(
      fig, arg_obj$params$glyph, lname = arg_obj$info$lname, lgroup = arg_obj$info$lgroup,
      data = arg_obj$data, data_sig = ifelse(is.null(data), NA, digest(data)),
      args = arg_obj$params, axis_type_range = axis_type_range,
      hover = arg_obj$info$hover, url = arg_obj$info$url, legend = arg_obj$info$legend,
      xname = arg_obj$info$x_name, yname = arg_obj$info$y_name, ly_call = mc
    )
  }
  fig
}
