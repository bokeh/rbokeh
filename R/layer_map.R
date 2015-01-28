#' @import maps
#' @export
lay_map <- function(fig, database = "world", regions = ".", line_color = "black", line_alpha = 1, line_width = 1, fill_color = NULL, fill_alpha = NULL, ...) {

  validateFig(fig, "lay_map")

  ##### boilerplate
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
  ##### boilerplate

  dd <- map2df(map(database = database,
    regions = regions, fill = TRUE, plot = FALSE))

  opts <- c(list(line_color = line_color, line_alpha = line_alpha,
    line_width = line_width, fill_color = fill_color,
    fill_alpha = fill_alpha), list(...))

  do.call(lay_polygons, c(list(fig = fig, xs = dd$lon, ys = dd$lat, group = dd$group), opts))
}

