#' Initialize a Bokeh figure
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
#' @param xaxes where to put x axis, or FALSE if no x axis ticks / labels
#' @param yaxes where to put y axis, or FALSE if no y axis ticks / labels
#' @param tools interactivity tools options
#' @param theme an rbokeh theme to use (tableau by default)
#' @template dots-figure
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
  theme = getOption("bokeh_theme"),
  ...
) {

  ## figure of another type (like GMapPlot)
  if("type" %in% names(list(...))) {
    type <- list(...)$type
  } else {
    type <- "Plot"
  }

  if(is.null(xlab) && !missing(xlab))
    xlab <- ""

  if(is.null(ylab) && !missing(ylab))
    ylab <- ""

  tt <- Sys.time()
  id <- gen_id(list(time = tt), type)

  model <- fig_model_skeleton(id, title, width, height, type)
  ref <- list(
    type = type,
    id = id
  )
  ref$subtype <- model$plot$subtype

  fig <- structure(list(
    width = width, height = height, title = title,
    xlab = xlab, ylab = ylab,
    xlim = xlim, ylim = ylim, padding_factor = padding_factor,
    plot_width = plot_width, plot_height = plot_height,
    xgrid = xgrid, ygrid = ygrid, xaxes = xaxes, yaxes = yaxes,
    tools = tools, theme = theme,
    model = model,
    modeltype = type, # not used
    ref = ref,
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

  extra_pars <- handle_extra_pars(list(...), figure_par_validator_map)
  if(!is.null(extra_pars))
    fig$model$plot$attributes <- c(fig$model$plot$attributes, extra_pars)

  ## check and add tools
  tool_list <- tools[tools %in% c("pan", "wheel_zoom", "box_zoom", "resize", "crosshair", "box_select", "lasso_select", "reset", "save")]
  not_used <- setdiff(tool_list, tools)
  if(length(not_used) > 0)
    message("Note: tools not used: ", paste(not_used, collapse = ", "))
  for(tl in tool_list)
    fig <- eval(parse(text = paste("tool_", tl, "(fig)", sep = "")))

  fig
}

fig_model_skeleton <- function(id, title, width = 480, height = 480, type = "Plot") {

  if(type == "GMapPlot") {
    subtype <- NULL
  } else {
    subtype <- "Figure"
  }

  model <- list(plot = list(
    type       = type,
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
  model$plot$subtype <- subtype
  model
}

figure_par_validator_map <- list(
  "background_fill" = "color",
  "border_fill" = "color",
  "outline_line_color" = "color",
  "title_text_color" = "color",
  "min_border" = "int",
  "min_border_bottom" = "int",
  "min_border_left" = "int",
  "min_border_right" = "int",
  "min_border_top" = "int",
  "outline_line_dash_offset" = "int",
  "plot_width" = "int",
  "outline_line_alpha" = "num_data_spec",
  "title_text_alpha" = "num_data_spec",
  "outline_line_width" = "num_data_spec",
  "title_text_font" = "string",
  "title_text_font_size" = "font_size_string",
  "outline_line_cap" = "line_cap",
  "outline_line_dash" = "line_dash",
  "outline_line_join" = "line_join",
  "title_text_align" = "text_align",
  "title_text_baseline" = "text_baseline",
  "title_text_font_style" = "font_style",
  "toolbar_location" = "toolbar_location",
  "logo" =  "logo",
  "h_symmetry" = "logical",
  "v_symmetry" = "logical"
)


