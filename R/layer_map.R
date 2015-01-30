#' @import maps
#' @export
ly_map <- function(fig, database = "world", regions = ".",
  color = NULL, alpha = NULL,
  line_color = NULL, line_alpha = 1, line_width = 1,
  fill_color = NULL, fill_alpha = NULL,
  lname = NULL, lgroup = NULL, ...) {

  validateFig(fig, "ly_map")

  lgroup <- getLgroup(lgroup, fig)

  xname <- "longitude"
  yname <- "latitude"

  xyNames <- getXYNames(NULL, NULL, xname, yname, list(...))

  dd <- map2df(map(database = database,
    regions = regions, fill = TRUE, plot = FALSE))

  args <- list(color = color, alpha = alpha,
    line_color = line_color, line_alpha = line_alpha, line_width = line_width,
    fill_color = fill_color, fill_alpha = fill_alpha, ...)

  args <- resolveColorAlpha(args, hasLine = TRUE, hasFill = TRUE, fig$layers[[lgroup]])

  do.call(ly_polygon, c(list(fig = fig, xs = dd$lon, ys = dd$lat, group = dd$group, lname = lname, lgroup = lgroup, xlab = xyNames$x, ylab = xyNames$y), args))
}

