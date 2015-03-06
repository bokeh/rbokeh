
#' Add a "map" layer to a Bokeh figure
#'
#' Draws lines and polygons as specified by a map database
#' @param fig figure to modify
#' @param database,regions parameters passed to \code{\link[maps]{map}}
#' @template par-coloralpha
#' @template par-lnamegroup
#' @template dots-fillline
#' @family layer functions
#' @import maps
#' @export
ly_map <- function(fig, database = "world", regions = ".",
  color = NULL, alpha = 1, lname = NULL, lgroup = NULL, ...) {

  validate_fig(fig, "ly_map")

  lgroup <- get_lgroup(lgroup, fig)

  xname <- "longitude"
  yname <- "latitude"

  dd <- map2df(map(database = database,
    regions = regions, fill = TRUE, plot = FALSE))

  args <- list(color = color, alpha = alpha, ...)

  args <- resolve_color_alpha(args, has_line = TRUE, has_fill = TRUE, fig$x$spec$layers[[lgroup]])

  do.call(ly_polygons, c(list(fig = fig, xs = dd$lon, ys = dd$lat, group = dd$group,
    lname = lname, lgroup = lgroup, xlab = xname, ylab = yname), args))
}

