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
# ir$glyphVal <- as.numeric(ir$Species)
# ir$glyphCol <- c("red", "green", "blue")[ ir$glyphVal ]
# load_all(); a <- figure() %>% ly_points(Sepal.Length, data = ir, fill_color = glyphCol); a
ly_points <- function(
  fig, x, y = NULL, data = NULL,
  glyph = 21, color = NULL, alpha = 1, size = 10,
  hover = NULL, url = NULL, legend = NULL,
  lname = NULL, lgroup = NULL, ...
) {

  validate_fig(fig, "ly_points")

  mc <- attr(fig, "ly_call")
  if(is.null(mc))
    mc <- lapply(match.call(), deparse)

  dots <- substitute(list(...))
  args <- sub_names(fig, data,
    grab(
      sb(x),
      sb(y),
      sb(glyph),
      sb(color),
      sb(alpha),
      sb(size),
      sb(hover),
      sb(url),
      sb(legend),
      sb(lname),
      sb(lgroup),
      dots
    )
  )
  # cat("\n\n"); print(args)

  if(is.null(args$glyph)) {
    args$glyph <- "circle"
  }

  # #TODO
  # # if color wasn't specified, it should be the same for all glyphs
  # if(length(unique(args$glyph)) != 1) {
  #   # if color wasn't specified, it should be the same for all glyphs
  #   if(is.null(args$color))
  #     args$color <- fig$x$spec$theme[["discrete"]][["fill_color"]](1)
  # }
  if(length(args$glyph) == 1) {
    args$glyph <- rep(args$glyph, length(args$x))
  }
  if(is.character(args$glyph)) {
    args$glyph <- factor(args$glyph)
  }

  # split data up for each glyph
  splitList <- split(seq_along(args$glyph), args$glyph)
  for (ii in seq_along(splitList)) {
    argObj <- subset_arg_obj(args, splitList[[ii]])

    argObj$glyph <- argObj$glyph[1]

    argObj <- resolve_color_alpha(
      argObj,
      has_line = TRUE, has_fill = TRUE,
      ly    = fig$x$spec$layers[[argObj$lgroup]],
      solid = argObj$glyph %in% as.character(15:20),
      theme = fig$x$spec$theme
    )

    argObj <- resolve_glyph_props(argObj$glyph, argObj, argObj$lgroup)

    ## see if any options won't be used and give a message
    if(valid_glyph(argObj$glyph)) {
      check_opts(argObj, argObj$glyph, formals = names(formals(ly_points)))
    }

    axis_type_range <- get_glyph_axis_type_range(argObj$x, argObj$y, glyph = argObj$glyph)

    fig <- make_glyph(
      fig, argObj$glyph, lname = argObj$lname, lgroup = argObj$lgroup,
      data = argObj[c("x", "y")], data_sig = ifelse(is.null(data), NA, digest(data)),
      args = argObj, axis_type_range = axis_type_range,
      hover = argObj$hover, url = argObj$url, legend = argObj$legend,
      xname = argObj$xName, yname = argObj$yName, ly_call = mc
    )
  }
  fig
}
