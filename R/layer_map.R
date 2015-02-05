#' @import maps
#' @export
ly_map <- function(fig, database = "world", regions = ".",
  color = NULL, alpha = NULL,
  line_color = NULL, line_alpha = 1, line_width = 1,
  fill_color = NULL, fill_alpha = NULL,
  lname = NULL, lgroup = NULL, ...) {

  validate_fig(fig, "ly_map")

  lgroup <- get_lgroup(lgroup, fig)

  xname <- "longitude"
  yname <- "latitude"

  xy_names <- get_xy_names(NULL, NULL, xname, yname, list(...))

  dd <- map2df(map(database = database,
    regions = regions, fill = TRUE, plot = FALSE))

  args <- list(color = color, alpha = alpha,
    line_color = line_color, line_alpha = line_alpha, line_width = line_width,
    fill_color = fill_color, fill_alpha = fill_alpha, ...)

  args <- resolve_color_alpha(args, has_line = TRUE, has_fill = TRUE, fig$layers[[lgroup]])

  do.call(ly_polygon, c(list(fig = fig, xs = dd$lon, ys = dd$lat, group = dd$group, lname = lname, lgroup = lgroup, xlab = xy_names$x, ylab = xy_names$y), args))
}

