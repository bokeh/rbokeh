
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
  color = NULL, alpha = 1, lname = NULL, lgroup = NULL, visible = TRUE, ...) {

  validate_fig(fig, "ly_map")

  args <- sub_names(fig, data = NULL,
    grab(
      color,
      alpha,
      lname,
      lgroup,
      visible,
      dots = lazy_dots(...),
      null_data = TRUE
    )
  )
  args$info$x_name <- "longitude"
  args$info$y_name <- "latitude"

  dd <- map2df(map(database = database,
    regions = regions, fill = TRUE, plot = FALSE))

  args$params <- resolve_color_alpha(args$params, has_line = TRUE, has_fill = TRUE, fig$x$spec$layers[[args$info$lgroup]], theme = fig$x$spec$theme)

  do.call(ly_polygons,
    c(
      list(
        fig = fig,
        xs = dd$lon, ys = dd$lat, group = dd$group,
        lname = args$info$lname, lgroup = args$info$lgroup,
        xlab = args$info$x_name, ylab = args$info$y_name
      ),
      args$params
    )
  )
}
