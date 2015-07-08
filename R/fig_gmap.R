## GMapPlot model is the same as Plot except for map_options

## when rendering maybe use something
## like this to approximate zoom level
## for a given glyph_x_ranges and glyph_y_ranges
# http://stackoverflow.com/questions/6048975/google-maps-v3-how-to-calculate-the-zoom-level-for-a-given-bounds

#' Initialize a Bokeh Google Map plot
#'
#' @param lat latitude where the map should be centered
#' @param lng longitude where the map should be centered
#' @param zoom initial \href{https://developers.google.com/maps/documentation/staticmaps/\#Zoomlevels}{zoom level} to use when displaying the map
#' @param map_type \href{https://developers.google.com/maps/documentation/staticmaps/\#MapTypes}{map type} to use for the plot - one of "hybrid", "satellite", "roadmap", "terrain"
#' @template dots-figure
#' @note This can be used in the same way as \code{\link{figure}}, adding layers on top of the Google Map.
#' @inheritParams figure
#' @example man-roxygen/ex-gmap.R
#' @export
gmap <- function(lat = 0, lng = 0, zoom = 0,
  map_type = "hybrid",
  map_style = NULL,
  width = 480,
  height = 480,
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  xlim = NULL,
  ylim = NULL,
  padding_factor = 0.07,
  plot_width = NULL,
  plot_height = NULL,
  xgrid = FALSE,
  ygrid = FALSE,
  xaxes = "below",
  yaxes = "left",
  tools = c("pan", "wheel_zoom", "box_zoom", "resize", "reset", "save"),
  theme = getOption("bokeh_theme"),
  ...) {

  fig <- figure(
    map_type = map_type,
    width = width,
    height = height,
    title = title,
    xlab = xlab,
    ylab = ylab,
    xlim = xlim,
    ylim = ylim,
    padding_factor = padding_factor,
    plot_width = plot_width,
    plot_height = plot_height,
    xgrid = xgrid,
    ygrid = ygrid,
    xaxes = xaxes,
    yaxes = yaxes,
    tools = tools,
    theme = theme,
    type = "GMapPlot",
    ...
  )

  fig$x$spec$model$plot$type <- "GMapPlot"
  fig$x$spec$ref$type <- "GMapPlot"

  fig$x$spec$model$plot$attributes$map_options <- list(
    lat      = lat,
    lng      = lng,
    zoom     = zoom,
    map_type = map_type
  )

  if(!is.null(map_style)) {
    fig$x$spec$model$plot$attributes$map_options$map_type <- NULL
    fig$x$spec$model$plot$attributes$map_options$styles <- map_style
  }

  fig$x$spec$glyph_x_ranges[["dummy_map_layer"]] <- c(lng, lat)
  fig$x$spec$glyph_y_ranges[["dummy_map_layer"]] <- c(lng, lat)
  fig$x$spec$x_axis_type <- "numeric"
  fig$x$spec$y_axis_type <- "numeric"

  fig
}
