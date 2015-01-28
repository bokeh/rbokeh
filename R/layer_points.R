

# major aesthetic specification:
# type: what type of glyph to plot at each point
# color: the color
#   when using a type that has only an outline, this will be the color of the outline
#   when using a type that has outline and fill, this will be the color of the outline and the fill will be a slightly less-saturated value of the same color
# alpha: the alpha of both the line and the fill
# size: the size of the glyph
# type can be any of the following:
# 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25
# these matche R's pch setting (see point_types())
# except 11 and 14 are missing, and 16, 19, 20 are the same
# or asterisk, circle, circle_cross, circle_x, cross, diamond, diamond_cross, inverted_triangle, square, square_cross, square_x, triangle, x
# the integer-based types simply map to any of these named types but with different line and/or fill properties

# in addition, lower-level control can be specified over

# grouping:
# any of the following properties can be specified as a "grouping" variable, with length as long as x and y, for which colors from the specified theme will be assigned to groups based on the unique values provided.  either a vector of valid values for that field can be given (e.g. for colors, a vector of valid css color names or hex codes) or a vector of factor levels which will be used to assign attributes based on the theme
# grouping variables: type, color, line_color, fill_color

#' @export
lay_points <- function(fig, x, y = NULL, data = NULL, glyph = 21, color = NULL, alpha = NULL, size = 10, hover = NULL, legend = NULL, line_color = NULL, line_alpha = 1, line_width = 1, fill_color = NULL, fill_alpha = NULL, lname = NULL, lgroup = NULL, lsubgroup = NULL, ...) {

  validateFig(fig, "lay_points")

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

  if(is.null(glyph))
    glyph <- "circle"

  args <- list(glyph = glyph, color = color, size = size, line_color = line_color, line_alpha = line_alpha,  line_width = line_width, fill_color = fill_color, fill_alpha = fill_alpha, ...)

  # if glyph is not unique, we need to split the data
  # and call make_glyph several times
  # otherwise we can just vary the values of things
  # and call make_glyph just once...
  if(length(unique(glyph)) > 1) {
    gl <- args$glyph
    args$glyph <- NULL

    ## for now, handle fill alpha here (assumes theme will always have line and fill)
    ## otherwise, need to handle this in print() when we find new glyph from theme
    if(is.null(args$fill_alpha)) {
      args$fill_alpha <- 0.5
    }

    lns <- sapply(args, length)
    idx <- which(lns == length(xy$x))

    dfArgs <- args[idx]

    if(is.character(gl))
      gl <- factor(gl)

    dfSplit <- split(seq_along(gl), gl)
    for(ii in seq_along(dfSplit)) {
      curIdx <- dfSplit[[ii]]
      curGlyph <- paste("glyph_")

      fig <- do.call(lay_points,
        c(lapply(dfArgs, function(x) subset_with_attributes(x, curIdx)), args[-idx],
          list(fig = fig, x = xy$x[curIdx], y = xy$y[curIdx],
            glyph = subset_with_attributes(gl, curIdx[1]), lgroup = lgroup,
            lname = ii, hover = hover$data[curIdx, , drop = FALSE])))
    }
    return(fig)
  }

  args <- resolveColorAlpha(args, hasLine = TRUE, hasFill = TRUE, fig$layers[[lgroup]])

  ## see if any options won't be used and give a message
  if(glyph %in% names(markerDict))
    checkOpts(list(...), glyph)

  args <- resolveGlyphProps(glyph, args, lgroup)
  args <- fixArgs(args, length(xy$x))

  axisTypeRange <- getGlyphAxisTypeRange(xy$x, xy$y, glyph = glyph)

  make_glyph(fig, args$glyph, lname = lname, lgroup = lgroup,
    data = c(xy, list(size = size)),
    args = args, axisTypeRange = axisTypeRange,
    hover = hover, legend = legend,
    xname = xyNames$x, yname = xyNames$y)
}
