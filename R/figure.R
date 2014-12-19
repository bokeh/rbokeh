## width and height map more naturally to R
## equivalent in bokeh is dims = (width, height)
## which we convert to just prior to plotting
## similar for xrange (xlim in R) and yrange (ylim in R)

#' Start a Bokeh Figure
#' 
#' @param width figure width in pixels
#' @param height figure width in pixels
#' @param title a title to display above the plot. - "title" is also the prefix for a set of Text Properties, so you can set the font for the title with the parameter text_font.
#' @param xlim the extent of the plotting area in the x-dimension (will be computed automatically if not specified). 
#' @param ylim the extent of the plotting area in the y-dimension (will be computed automatically if not specified).
#' @param padding_factor if limits are not specified, by what factor should the extents of the data be padded
#' @param plot_width,plot_height width and height of the entire plot in pixels, including border space
#' @param x_axis_type,y_axis_type can be set to "datetime" to create datetime axis
#' @param x_mapper_type,y_mapper_type can be set to "log" to specifically set the mapper used for the axis
#' @param background_fill a color to fill the inner plot area with
#' @param border_fill a color to fill the border region around the plot area with.
#' @param min_border a minimum size in pixels for the border. This applies to all sides of the plot.
#' @param min_border_left set left border individually
#' @param min_border_right set right border individually
#' @param min_border_top set top border individually
#' @param min_border_bottom set bottom border individually
#' @param h_symmetry,v_symmetry whether to symmetrize plot borders on opposite horizontal or vertical sides of the plot.
#' @param outline_line_color Line Properties that controls the appearance of an outline around the plot, for instance you can set the color of the outline with this
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
  xlim = NULL,
  ylim = NULL,
  padding_factor = 0.07,
  plot_width = NULL,
  plot_height = NULL,
  x_axis_type = NULL,
  y_axis_type = NULL,
  x_mapper_type = NULL,
  y_mapper_type = NULL,
  background_fill = NULL,
  border_fill = NULL,
  min_border = NULL,
  min_border_left = NULL,
  min_border_right = NULL,
  min_border_top = NULL,
  min_border_bottom = NULL,
  h_symmetry = NULL,
  v_symmetry = NULL,
  outline_line_color = NULL,
  xaxes = "below",
  yaxes = "left",
  tools = NULL,
  theme = getOption("bokeh_theme")
) {
  structure(list(
    width = width, height = height, title = title, 
    xlim = xlim, ylim = ylim, padding_factor = padding_factor,
    plot_width = plot_width, plot_height = plot_height, 
    x_axis_type = x_axis_type, y_axis_type = y_axis_type, 
    x_mapper_type = x_mapper_type, y_mapper_type = y_mapper_type, 
    background_fill = background_fill, border_fill = border_fill, 
    min_border = min_border, min_border_left = min_border_left, 
    min_border_right = min_border_right, min_border_top = min_border_top, 
    min_border_bottom = min_border_bottom, h_symmetry = h_symmetry, 
    v_symmetry = v_symmetry, outline_line_color = outline_line_color, 
    xaxes = xaxes, yaxes = yaxes, tools = tools, theme = theme,
    ## spec for each glyph
    glyphSpecs = list(),
    ## keep track of x and y range of each glyph
    glyphXRanges = list(),
    glyphYRanges = list(),
    glyphDefer = list(),
    ## keep track of the axes ('cat' or 'num')
    xAxisType = NULL,
    yAxisType = NULL
  ), class = "BokehFigure")
}


