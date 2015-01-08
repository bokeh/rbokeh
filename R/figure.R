## width and height map more naturally to R
## equivalent in bokeh is dims = (width, height)
## which we convert to just prior to plotting
## similar for xrange (xlim in R) and yrange (ylim in R)

#' Start a Bokeh Figure
#' 
#' @param width figure width in pixels
#' @param height figure width in pixels
#' @param title a title to display above the plot. - "title" is also the prefix for a set of Text Properties, so you can set the font for the title with the parameter text_font.
#' @param xlab label for x axis
#' @param ylab label for y axis
#' @param xlim the extent of the plotting area in the x-dimension (will be computed automatically if not specified). 
#' @param ylim the extent of the plotting area in the y-dimension (will be computed automatically if not specified).
#' @param padding_factor if limits are not specified, by what factor should the extents of the data be padded
#' @param plot_width,plot_height width and height of the entire plot in pixels, including border space
#' @param xaxes where to put x axis labels
#' @param yaxes where to put y axis labels
#' @param tools interactivity tools options
#' @param theme an rbokeh theme to use (tableau by default)
#' @export
#' @import htmlwidgets
#' @import methods
figure <- function(
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
  xaxes = "below",
  yaxes = "left",
  tools = c("pan", "wheel_zoom", "box_zoom", "resize", "reset", "save"),
  theme = getOption("bokeh_theme")
) {
  if(is.null(xlab) && !missing(xlab))
    xlab <- ""

  if(is.null(ylab) && !missing(ylab))
    ylab <- ""

  tt <- Sys.time()
  id <- genId(list(time = tt), "Plot")

  fig <- structure(list(
    width = width, height = height, title = title, 
    xlab = xlab, ylab = ylab,
    xlim = xlim, ylim = ylim, padding_factor = padding_factor,
    plot_width = plot_width, plot_height = plot_height,
    xaxes = xaxes, yaxes = yaxes, 
    tools = tools, theme = theme,
    model = figModelSkeleton(id, title, width, height),
    ref = list(
      type    = "Plot",
      subtype = "Figure",
      id      =  id
    ),
    ## place to store spec, data, and function for deferred glyphs
    glyphDeferSpecs = list(),
    glyphDeferData = list(),
    glyphDefer = list(),
    glyphLayers = list(),
    ## keep track of x and y range of each glyph
    glyphXRanges = list(),
    glyphYRanges = list(),
    ## keep track of the axes ('cat' or 'num')
    xAxisType = NULL,
    yAxisType = NULL
  ), class = "BokehFigure")

  ## check and add tools
  toolList <- tools[tools %in% c("pan", "wheel_zoom", "box_zoom", "resize", "reset", "save")]
  notUsed <- setdiff(toolList, tools)
  if(length(notUsed) > 0)
    message("Note: tools not used: ", paste(notUsed, collapse = ", "))
  for(tl in toolList)
    fig <- eval(parse(text = paste("tool_", tl, "(fig)", sep = "")))

  fig
}

figModelSkeleton <- function(id, title, width = 480, height = 480) {
  model <- list(plot = list(
    type       = "Plot",
    subtype    = "Figure",
    id         =  id,
    attributes = list(
      title = title,
      id = id,
      plot_width = width,
      plot_height = height,
      x_range = list(),
      y_range = list(),
      left = list(),
      below = list(),
      right = list(),
      above = list(),
      renderers = list(),
      tools = list(),
      tool_events = list(),
      extra_y_ranges = structure(list(), .Names = character(0)),
      extra_x_ranges = structure(list(), .Names = character(0)),
      tags = list(),
      doc = NULL
    )
  ))
}


# #' @param x_axis_type,y_axis_type can be set to "datetime" to create datetime axis
# #' @param x_mapper_type,y_mapper_type can be set to "log" to specifically set the mapper used for the axis
# #' @param background_fill a color to fill the inner plot area with
# #' @param border_fill a color to fill the border region around the plot area with.
# #' @param min_border a minimum size in pixels for the border. This applies to all sides of the plot.
# #' @param min_border_left set left border individually
# #' @param min_border_right set right border individually
# #' @param min_border_top set top border individually
# #' @param min_border_bottom set bottom border individually
# #' @param h_symmetry,v_symmetry whether to symmetrize plot borders on opposite horizontal or vertical sides of the plot.
# #' @param outline_line_color Line Properties that controls the appearance of an outline around the plot, for instance you can set the color of the outline with this

# x_axis_type = x_axis_type, y_axis_type = y_axis_type, 
# x_mapper_type = x_mapper_type, y_mapper_type = y_mapper_type, 
# background_fill = background_fill, border_fill = border_fill, 
# min_border = min_border, min_border_left = min_border_left, 
# min_border_right = min_border_right, min_border_top = min_border_top, 
# min_border_bottom = min_border_bottom, h_symmetry = h_symmetry, 
# v_symmetry = v_symmetry, outline_line_color = outline_line_color, 
