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
#' @param xgrid whether to draw x axis grid lines
#' @param ygrid whether to draw y axis grid lines
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
  xgrid = TRUE,
  ygrid = TRUE,
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
  id <- gen_id(list(time = tt), "Plot")

  fig <- structure(list(
    width = width, height = height, title = title,
    xlab = xlab, ylab = ylab,
    xlim = xlim, ylim = ylim, padding_factor = padding_factor,
    plot_width = plot_width, plot_height = plot_height,
    xgrid = xgrid, ygrid = ygrid, xaxes = xaxes, yaxes = yaxes,
    tools = tools, theme = theme,
    model = fig_model_skeleton(id, title, width, height),
    modeltype = "Plot", # not used
    ref = list(
      type    = "Plot",
      subtype = "Figure",
      id      =  id
    ),
    time = tt,
    ## place to store spec, data, and function for deferred glyphs
    glyph_defer_specs = list(), # not used
    glyph_defer_data = list(), # not used
    glyph_defer = list(),
    layers = list(),
    data_sigs = list(),
    ## keep track of x and y range of each glyph
    glyph_x_ranges = list(),
    glyph_y_ranges = list(),
    ## keep track of the axes ('cat' or 'num')
    x_axis_type = NULL,
    y_axis_type = NULL,
    has_x_axis = FALSE,
    has_y_axis = FALSE,
    has_x_range = FALSE,
    has_y_range = FALSE
  ), class = "BokehFigure")

  ## check and add tools
  tool_list <- tools[tools %in% c("pan", "wheel_zoom", "box_zoom", "resize", "crosshair", "tap", "box_select", "lasso_select", "reset", "save")]
  not_used <- setdiff(tool_list, tools)
  if(length(not_used) > 0)
    message("Note: tools not used: ", paste(not_used, collapse = ", "))
  for(tl in tool_list)
    fig <- eval(parse(text = paste("tool_", tl, "(fig)", sep = "")))

  fig
}

fig_model_skeleton <- function(id, title, width = 480, height = 480) {
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
