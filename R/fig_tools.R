# get_docs(mods, "BoxSelectTool",
#   c("dimensions", "select_every_mousemove",
#     "names", "callback")) %>% cat()

# get_docs(mods, "BoxAnnotation",
#   c("line_color", "line_alpha", "fill_color", "fill_alpha", "line_width",
#     "line_dash", "line_dash_offset", "line_join", "line_cap")) %>% cat()

#' Add "box_select" tool to a Bokeh figure
#' @param fig Figure to modify.
#' @param dimensions Which dimensions the box selection is to be free in. By default, users may freely draw selections boxes with any dimensions. If only "width" is supplied, the box will be constrained to span the entire vertical space of the plot, only the horizontal dimension can be controlled. If only "height" is supplied, the box will be constrained to span the entire horizontal space of the plot, and the vertical dimension can be controlled. One of 'width', 'height', or 'both'.
#' @param select_every_mousemove Whether a selection computation should happen on every mouse event, or only once, when the selection region is completed. Default: False
#' @param line_color The line color values for the box.
#' @param line_alpha The line alpha values for the box.
#' @param fill_color The fill color values for the box.
#' @param fill_alpha The fill alpha values for the box.
#' @param line_width The line width values for the box.
#' @param line_dash The line dash values for the box.
#' @param line_dash_offset The line dash offset values for the box.
#' @param line_join The line join values for the box.
#' @param line_cap The line cap values for the box.
#' @param names A list of names to query for. If set, only renderers that have a matching value for their "name" attribute will be used.
#' @param callback A callback to run in the browser on completion of drawing a selection box. The cb_data parameter that is available to the Callback code will contain one BoxSelectTool-specific field: :geometry: object containing the coordinates of the selection box.
#' @param remove Logical indicating whether the tool should be removed.
#' @examples
#' figure(tools = NULL) %>%
#'   ly_points(1:10) %>%
#'   tool_box_select(line_color = "red", callback = "console.log('calling back...')")
#' @family tools
#' @export
tool_box_select <- function(fig,
  dimensions = NULL,
  select_every_mousemove = TRUE,
  line_color = NULL,
  line_alpha = NULL,
  fill_color = NULL,
  fill_alpha = NULL,
  line_width = NULL,
  line_dash = NULL,
  line_dash_offset = NULL,
  line_join = NULL,
  line_cap = NULL,
  names = NULL,
  callback = NULL,
  remove = FALSE
) {
  args <- get_specified_args(nnms = c("fig", "remove"))
  update_tool(fig, which = "box_select", args, remove = remove)
}


# get_docs(mods, "LassoSelectTool",
#   c("select_every_mousemove",
#     "names", "callback")) %>% cat()

# get_docs(mods, "PolyAnnotation",
#   c("line_color", "line_alpha", "fill_color", "fill_alpha", "line_width",
#     "line_dash", "line_dash_offset", "line_join", "line_cap")) %>% cat()

#' Add "lasso_select" tool to a Bokeh figure
#' @param fig Figure to modify.
#' @param select_every_mousemove Whether a selection computation should happen on every mouse event, or only once, when the selection region is completed. Default: True
#' @param line_color The line color values for the polygon.
#' @param line_alpha The line alpha values for the polygon.
#' @param fill_color The fill color values for the polygon.
#' @param fill_alpha The fill alpha values for the polygon.
#' @param line_width The line width values for the polygon.
#' @param line_dash The line dash values for the polygon.
#' @param line_dash_offset The line dash offset values for the polygon.
#' @param line_join The line join values for the polygon.
#' @param line_cap The line cap values for the polygon.
#' @param names A list of names to query for. If set, only renderers that have a matching value for their "name" attribute will be used.
#' @param callback A callback to run in the browser on every selection of a lasso area. The cb_data parameter that is available to the Callback code will contain one LassoSelectTool-specific field: :geometry: object containing the coordinates of the lasso area.
#' @param remove Logical indicating whether the tool should be removed.
#' @examples
#' figure(tools = NULL) %>%
#'   ly_points(1:10) %>%
#'   tool_lasso_select(line_color = "red", callback = "console.log('calling back...')")
#' @family tools
#' @export
tool_lasso_select <- function(fig,
  select_every_mousemove = TRUE,
  line_color = NULL,
  line_alpha = NULL,
  fill_color = NULL,
  fill_alpha = NULL,
  line_width = NULL,
  line_dash = NULL,
  line_dash_offset = NULL,
  line_join = NULL,
  line_cap = NULL,
  names = NULL,
  callback = NULL,
  remove = FALSE
) {
  args <- get_specified_args(nnms = c("fig", "remove"))
  update_tool(fig, which = "lasso_select", args, remove = remove)
}

# get_docs(mods, "PolySelectTool", c("names")) %>% cat()

# get_docs(mods, "PolyAnnotation",
#   c("line_color", "line_alpha", "fill_color", "fill_alpha", "line_width",
#     "line_dash", "line_dash_offset", "line_join", "line_cap")) %>% cat()

#' Add "poly_select" tool to a Bokeh figure
#' @param fig Figure to modify.
#' @param line_color The line color values for the polygon.
#' @param line_alpha The line alpha values for the polygon.
#' @param fill_color The fill color values for the polygon.
#' @param fill_alpha The fill alpha values for the polygon.
#' @param line_width The line width values for the polygon.
#' @param line_dash The line dash values for the polygon.
#' @param line_dash_offset The line dash offset values for the polygon.
#' @param line_join The line join values for the polygon.
#' @param line_cap The line cap values for the polygon.
#' @param names A list of names to query for. If set, only renderers that have a matching value for their "name" attribute will be used.
#' @param remove Logical indicating whether the tool should be removed.
#' @examples
#' figure(tools = NULL) %>%
#'   ly_points(1:10) %>%
#'   tool_poly_select(line_color = "red")
#' @family tools
#' @export
tool_poly_select <- function(fig,
  line_color = NULL,
  line_alpha = NULL,
  fill_color = NULL,
  fill_alpha = NULL,
  line_width = NULL,
  line_dash = NULL,
  line_dash_offset = NULL,
  line_join = NULL,
  line_cap = NULL,
  names = NULL,
  remove = FALSE
) {
  args <- get_specified_args(nnms = c("fig", "remove"))
  update_tool(fig, which = "poly_select", args, remove = remove)
}

# get_docs(mods, "CrosshairTool", c("dimensions", "line_width", "line_alpha",
#   "line_color")) %>% cat()

#' Add "crosshair" tool to a Bokeh figure
#' @param fig Figure to modify.
#' @param dimensions Which dimensions the crosshair tool is to track. By default, both a vertical and horizontal line will be drawn. If only "width" is supplied, only a horizontal line will be drawn. If only "height" is supplied, only a vertical line will be drawn. One of 'width', 'height', or 'both'.
#' @param line_width Stroke width in units of pixels.
#' @param line_alpha An alpha value to use to stroke paths with. Acceptable values are floating point numbers between 0 (transparent) and 1 (opaque).
#' @param line_color A color to use to stroke paths with.
#' @param remove Logical indicating whether the tool should be removed.
#' @examples
#' figure(tools = NULL) %>%
#'   ly_points(1:10) %>%
#'   tool_crosshair(line_width = 12, line_alpha = 0.25)
#' @family tools
#' @export
tool_crosshair <- function(fig,
  dimensions = NULL,
  line_width = NULL,
  line_alpha = NULL,
  line_color = NULL,
  remove = FALSE
) {
  args <- get_specified_args(nnms = c("fig", "remove"))
  update_tool(fig, which = "crosshair", args, remove = remove)
}

# get_docs(mods, "BoxZoomTool", c("dimensions", "match_aspect")) %>% cat()

#' Add "box_zoom" tool to a Bokeh figure
#' @param fig Figure to modify.
#' @param dimensions Which dimensions the zoom box is to be free in. By default, users may freely draw zoom boxes with any dimensions. If only "width" is supplied, the box will be constrained to span the entire vertical space of the plot, only the horizontal dimension can be controlled. If only "height" is supplied, the box will be constrained to span the entire horizontal space of the plot, and the vertical dimension can be controlled. One of 'width', 'height', or 'both'.
#' @param match_aspect Whether the box zoom region should be restricted to have the same aspect ratio as the plot region. Note: If the tool is restricted to one dimension, this value has no effect.
#' @param line_color The line color values for the box.
#' @param line_alpha The line alpha values for the box.
#' @param fill_color The fill color values for the box.
#' @param fill_alpha The fill alpha values for the box.
#' @param line_width The line width values for the box.
#' @param line_dash The line dash values for the box.
#' @param line_dash_offset The line dash offset values for the box.
#' @param line_join The line join values for the box.
#' @param line_cap The line cap values for the box.
#' @param remove Logical indicating whether the tool should be removed.
#' @examples
#' figure(tools = "reset") %>%
#'   ly_points(1:10) %>%
#'   tool_box_zoom(line_color = "red", match_aspect = TRUE)
#' @family tools
#' @export
tool_box_zoom <- function(fig,
  dimensions = NULL,
  match_aspect = FALSE,
  line_color = NULL,
  line_alpha = NULL,
  fill_color = NULL,
  fill_alpha = NULL,
  line_width = NULL,
  line_dash = NULL,
  line_dash_offset = NULL,
  line_join = NULL,
  line_cap = NULL,
  remove = FALSE
) {
  args <- get_specified_args(nnms = c("fig", "remove"))
  update_tool(fig, which = "box_zoom", args, remove = remove)
}

# get_docs(mods, "WheelZoomTool", c("dimensions")) %>% cat()

#' Add "wheel_zoom" tool to a Bokeh figure
#' @param fig Figure to modify.
#' @param dimensions Which dimensions the wheel zoom tool is constrained to act in. By default the wheel zoom tool will zoom in any dimension, but can be configured to only zoom horizontally across the width of the plot, or vertically across the height of the plot. One of 'width', 'height', or 'both'.
#' @param remove Logical indicating whether the tool should be removed.
#' @examples
#' figure(tools = NULL) %>%
#'   ly_points(1:10) %>%
#'   tool_wheel_zoom()
#' @family tools
#' @export
tool_wheel_zoom <- function(fig, dimensions = NULL, remove = FALSE) {
  args <- get_specified_args(nms = "dimensions")
  update_tool(fig, which = "wheel_zoom", args, remove = remove)
}

# get_docs(mods, "ZoomInTool", c("dimensions", "factor")) %>% cat()

#' Add "zoom_in" tool to a Bokeh figure
#' @param fig Figure to modify.
#' @param dimensions Which dimensions the zoom-in tool is constrained to act in. By default the zoom-in tool will zoom in any dimension, but can be configured to only zoom horizontally across the width of the plot, or vertically across the height of the plot. One of 'width', 'height', or 'both'.
#' @param factor Proportion to zoom for each click of the zoom-in tool.
#' @param remove Logical indicating whether the tool should be removed.
#' @examples
#' figure(tools = NULL) %>%
#'   ly_points(1:10) %>%
#'   tool_zoom_in(factor = 0.5) %>%
#'   tool_zoom_out(factor = 0.5)
#' @family tools
#' @export
tool_zoom_in <- function(fig, dimensions = NULL, factor = NULL, remove = FALSE) {
  args <- get_specified_args(nms = c("dimensions", "factor"))
  update_tool(fig, which = "zoom_in", args, remove = remove)
}

# get_docs(mods, "ZoomOutTool", c("dimensions", "factor")) %>% cat()

#' Add "zoom_out" tool to a Bokeh figure
#' @param fig Figure to modify.
#' @param dimensions Which dimensions the zoom-out tool is constrained to act in. By default the zoom-out tool will zoom in any dimension, but can be configured to only zoom horizontally across the width of the plot, or vertically across the height of the plot. One of 'width', 'height', or 'both'.
#' @param factor Proportion to zoom for each click of the zoom-out tool.
#' @param remove Logical indicating whether the tool should be removed.
#' @examples
#' figure(tools = NULL) %>%
#'   ly_points(1:10) %>%
#'   tool_zoom_in(factor = 0.5) %>%
#'   tool_zoom_out(factor = 0.5)
#' @family tools
#' @export
tool_zoom_out <- function(fig, dimensions = NULL, factor = NULL, remove = FALSE) {
  args <- get_specified_args(nms = c("dimensions", "factor"))
  update_tool(fig, which = "zoom_out", args, remove = remove)
}

# get_docs(mods, "PanTool", c("dimensions")) %>% cat()

#' Add "pan" tool to a Bokeh figure
#' @param fig Figure to modify.
#' @param dimensions Which dimensions the pan tool is constrained to act in. By default the pan tool will pan in any dimension, but can be configured to only pan horizontally across the width of the plot, or vertically across the height of the plot. One of 'width', 'height', or 'both'.
#' @examples
#' \donttest{
#' # only pan on x axis
#' figure() %>% ly_points(1:10) %>%
#'  tool_pan(dimensions = "height")
#' }
#' @param remove Logical indicating whether the tool should be removed.
#' @examples
#' figure(tools = NULL) %>%
#'   ly_points(1:10) %>%
#'   tool_pan(dimensions = "width")
#' @family tools
#' @export
tool_pan <- function(fig, dimensions = NULL, remove = FALSE) {
  args <- get_specified_args(nms = "dimensions")
  update_tool(fig, which = "pan", args, remove = remove)
}

# get_docs(mods, "WheelPanTool", c("dimension")) %>% cat()

#' Add "wheel_pan" tool to a Bokeh figure
#' @param fig Figure to modify.
#' @param dimension Which dimension the wheel pan tool is constrained to act in. By default the wheel pan tool will pan the plot along the x-axis. Must be one of 'width' or 'height'.
#' @param remove Logical indicating whether the tool should be removed.
#' @examples
#' figure(tools = NULL) %>%
#'   ly_points(1:10) %>%
#'   tool_wheel_pan(dimension = "width")
#' @family tools
#' @export
tool_wheel_pan <- function(fig, dimension = NULL, remove = FALSE) {
  args <- get_specified_args(nms = "dimension")
  update_tool(fig, "wheel_pan", args, remove = remove)
}

# get_docs(mods, "ResetTool", c("reset_size")) %>% cat()

#' Add "reset" tool to a Bokeh figure
#' @param fig Figure to modify.
#' @param reset_size Whether activating the Reset tool should also reset the plot's canvas dimensions to their original size.
#' @param remove Logical indicating whether the tool should be removed.
#' @examples
#' figure() %>%
#'   ly_points(1:10) %>%
#'   tool_reset()
#' @family tools
#' @export
tool_reset <- function(fig, reset_size = TRUE, remove = FALSE) {
  args <- get_specified_args(nms = "reset_size")
  update_tool(fig, "reset", args, remove = remove)
}

#' Add "undo" tool to a Bokeh figure
#' @param fig Figure to modify.
#' @param remove Logical indicating whether the tool should be removed.
#' @examples
#' figure() %>%
#'   ly_points(1:10) %>%
#'   tool_undo() %>%
#'   tool_redo()
#' @family tools
#' @export
tool_undo <- function(fig, remove = FALSE) {
  update_tool(fig, "undo", NULL, remove = remove)
}

#' Add "redo" tool to a Bokeh figure
#' @param fig Figure to modify.
#' @param remove Logical indicating whether the tool should be removed.
#' @examples
#' figure() %>%
#'   ly_points(1:10) %>%
#'   tool_undo() %>%
#'   tool_redo()
#' @family tools
#' @export
tool_redo <- function(fig, remove = FALSE) {
  update_tool(fig, "redo", NULL, remove = remove)
}

#' Add "save" tool to a Bokeh figure
#' @param fig Figure to modify.
#' @param remove Logical indicating whether the tool should be removed.
#' @examples
#' figure(tools = NULL) %>%
#'   ly_points(1:10) %>%
#'   tool_save()
#' @family tools
#' @export
tool_save <- function(fig, remove = FALSE) {
  update_tool(fig, "save", NULL, remove = remove)
}

#' Add "help" tool to a Bokeh figure
#' @param fig Figure to modify.
#' @param redirect Site to be redirected through upon click.
#' @param help_tooltip Tooltip displayed when hovering over the help icon.
#' @param remove Logical indicating whether the tool should be removed.
#' @examples
#' figure(tools = NULL) %>%
#'   ly_points(1:10, 1:10) %>%
#'   tool_help(help_tooltip = "you can do it!")
#'
#' # help tool specified but then later removed
#' figure(tools = "help") %>%
#'   ly_points(1:10, 1:10) %>%
#'   tool_help(remove = TRUE)
#' @family tools
#' @export
tool_help <- function(fig, redirect = NULL, help_tooltip = NULL, remove = FALSE) {
  args <- get_specified_args(nnms = "fig")
  update_tool(fig, "help", args, remove = remove)
}

# TODO: add 'clear'
update_tool <- function(fig, which, args, remove = FALSE) {
  if (remove) {
    fig$x$pars$tools[[which]] <- NULL
    return(fig)
  }

  tool_overlay_args <- c("line_color", "line_alpha", "fill_color", "fill_alpha",
    "line_width", "line_dash", "line_dash_offset", "line_join", "line_cap")
  if (which == "crosshair")
    tool_overlay_args <- NULL

  ov_nms <- intersect(names(args), tool_overlay_args)
  if (is.null(fig$x$pars$tools[[which]]))
    fig$x$pars$tools[[which]] <- list()

  fig$x$pars$tools[[which]]$overlay <- args[ov_nms]
  fig$x$pars$tools[[which]]$mod <- args[setdiff(names(args), ov_nms)]

  fig
}
