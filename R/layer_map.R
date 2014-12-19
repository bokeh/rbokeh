#' @import maps
#' @export
lay_map <- function(fig, database = "world", regions = ".", line_color = "black", line_alpha = 1, line_width = 1, fill_color = NULL, fill_alpha = NULL, ...) {

  validateFig(fig, "lay_map")

  dd <- map2df(map(database = database, 
    regions = regions, fill = TRUE, plot = FALSE))

  opts <- c(list(line_color = line_color, line_alpha = line_alpha, 
    line_width = line_width, fill_color = fill_color, 
    fill_alpha = fill_alpha), list(...))

  do.call(lay_polygons, c(list(fig = fig, xs = dd$lon, ys = dd$lat, group = dd$group), opts))
}

