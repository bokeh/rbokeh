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
ly_points <- function(fig, x, y = NULL, data = NULL,
  glyph = 21, color = NULL, alpha = 1, size = 10,
  hover = NULL, url = NULL, legend = NULL,
  lname = NULL, lgroup = NULL, ...) {

  validate_fig(fig, "ly_points")

  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    dots  <- substitute(list(...))[-1]
    args  <- lapply(dots, function(x) v_eval(x, data))
    x     <- v_eval(substitute(x), data)
    y     <- v_eval(substitute(y), data)
    size  <- v_eval(substitute(size), data)
    glyph <- v_eval(substitute(glyph), data)
    color <- v_eval(substitute(color), data)
  } else {
    args <- list(...)
  }

  hover <- get_hover(substitute(hover), data)
  url <- get_url(substitute(url), data)

  xy_names <- get_xy_names(x, y, xname, yname, args)
  ## translate different x, y types to vectors
  xy <- get_xy_data(x, y)
  lgroup <- get_lgroup(lgroup, fig)

  if(is.null(glyph))
    glyph <- "circle"

  args <- c(args, list(glyph = glyph, color = color,
    alpha = alpha, size = size))

  # if glyph is not unique, we need to split the data
  # and call make_glyph several times
  # otherwise we can just vary the values of things
  # and call make_glyph just once...
  if(length(unique(glyph)) > 1) {
    gl <- args$glyph
    args$glyph <- NULL

    lns <- sapply(args, length)
    idx <- which(lns == length(xy$x))

    df_args <- args[idx]

    if(is.character(gl))
      gl <- factor(gl)

    df_split <- split(seq_along(gl), gl)
    for(ii in seq_along(df_split)) {
      cur_idx <- df_split[[ii]]
      # cur_glyph <- paste("glyph_")

      cur_hover <- hover
      cur_hover$data <- cur_hover$data[cur_idx, , drop = FALSE]

      fig <- do.call(ly_points,
        c(lapply(df_args, function(x) subset_with_attributes(x, cur_idx)), args[-idx],
          list(fig = fig, x = xy$x[cur_idx], y = xy$y[cur_idx],
            glyph = subset_with_attributes(gl, cur_idx[1]), lgroup = lgroup,
            lname = ii, hover = cur_hover, legend = legend,
            xlab = xy_names$x, ylab = xy_names$y)))
    }
    return(fig)
  }

  args <- resolve_color_alpha(args, has_line = TRUE, has_fill = TRUE, fig$layers[[lgroup]],
    solid = as.character(glyph) %in% as.character(15:20))

  args <- resolve_glyph_props(glyph, args, lgroup)
  args <- fix_args(args, length(xy$x))

  ## see if any options won't be used and give a message
  if(args$glyph %in% names(marker_dict))
    check_opts(args, args$glyph, formals = names(formals(ly_points)))

  axis_type_range <- get_glyph_axis_type_range(xy$x, xy$y, glyph = glyph)

  make_glyph(fig, args$glyph, lname = lname, lgroup = lgroup,
    data = xy, data_sig = ifelse(is.null(data), NA, digest(data)),
    args = args, axis_type_range = axis_type_range,
    hover = hover, url = url, legend = legend,
    xname = xy_names$x, yname = xy_names$y)
}
