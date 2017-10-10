#' Add "box_select" tool to a Bokeh figure
#' @param select_every_mousemove Whether a selection computation should happen on every mouse event, or only once, when the selection region is completed. Default: TRUE.
#' @param names A list of names to query for. If set, only renderers that have a matching value for their ``name`` attribute will be used.
#' @param A callback to run in the browser on completion of drawing a selection box. The cb_data parameter that is available to the Callback code will contain one BoxSelectTool-specific field:
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

#' Add "lasso_select" tool to a Bokeh figure
#' @param select_every_mousemove Whether a selection computation should happen on every mouse event, or only once, when the selection region is completed. Default: TRUE.
#' @param names A list of names to query for. If set, only renderers that have a matching value for their ``name`` attribute will be used.
#' @param callback A callback to run in the browser on every selection of a lasso area. The cb_data parameter that is available to the Callback code will contain one LassoSelectTool-specific field.
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

#' Add "poly_select" tool to a Bokeh figure
#' @param names A list of names to query for. If set, only renderers that have a matching value for their ``name`` attribute will be used.
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

#' Add "crosshair" tool to a Bokeh figure
#' @param dimensions Which dimensions the crosshair tool is to track. By default, both a vertical and horizontal line will be drawn. If only "width" is supplied, only a horizontal line will be drawn. If only "height" is supplied, only a vertical line will be drawn. One of 'width', 'height', or 'both'.
#' @param line_width Stroke width in units of pixels.
#' @param line_alpha An alpha value to use to stroke paths with.
#' @param line_color A color to use to stroke the crosshair paths with.
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

#' Add "box_zoom" tool to a Bokeh figure
#' @param dimensions Which dimensions the zoom box is to be free in. By default, users may freely draw zoom boxes with any dimensions. If only "width" is supplied, the box will be constrained to span the entire vertical space of the plot, only the horizontal dimension can be controlled. If only "height" is supplied, the box will be constrained to span the entire horizontal space of the plot, and the vertical dimension can be controlled. One of 'width', 'height', or 'both'.
#' @param match_aspect Whether the box zoom region should be restricted to have the same aspect ratio as the plot region. Boolean. Note: If the tool is restricted to one dimension, this value has no effect.
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

#' Add "wheel_zoom" tool to a Bokeh figure
#' @param dimensions Which dimensions the wheel zoom tool is constrained to act in. By default the wheel zoom tool will zoom in any dimension, but can be configured to only zoom horizontally across the width of the plot, or vertically across the height of the plot. One of 'width', 'height', or 'both'.
#' @export
tool_wheel_zoom <- function(fig, dimensions = NULL, remove = FALSE) {
  args <- get_specified_args(nms = "dimensions")
  update_tool(fig, which = "wheel_zoom", args, remove = remove)
}

#' Add "zoom_in" tool to a Bokeh figure
#' @param factor Proportion to zoom for each click of the zoom-in tool.
#' @param dimensions Which dimensions the zoom-in tool is constrained to act in. By default the zoom-in tool will zoom in any dimension, but can be configured to only zoom horizontally across the width of the plot, or vertically across the height of the plot. One of 'width', 'height', or 'both'.
#' @export
tool_zoom_in <- function(fig, dimensions = NULL, factor = NULL, remove = FALSE) {
  args <- get_specified_args(nms = c("dimensions", "factor"))
  update_tool(fig, which = "zoom_in", args, remove = remove)
}

#' Add "zoom_out" tool to a Bokeh figure
#' @param factor Proportion to zoom for each click of the zoom-out tool.
#' @param dimensions Which dimensions the zoom-out tool is constrained to act in. By default the zoom-out tool will zoom in any dimension, but can be configured to only zoom horizontally across the width of the plot, or vertically across the height of the plot. One of 'width', 'height', or 'both'.
#' @export
tool_zoom_out <- function(fig, dimensions = NULL, factor = NULL, remove = FALSE) {
  args <- get_specified_args(nms = c("dimensions", "factor"))
  update_tool(fig, which = "zoom_out", args, remove = remove)
}

#' Add "pan" tool to a Bokeh figure
#' @param dimensions a vector specifying whether the pan tool should pan with respect to the x axis ("width") and the y axis ("height") or both (c("width", "height"))
#' @examples
#' \donttest{
#' # only pan on x axis
#' figure() %>% ly_points(1:10) %>%
#'  tool_pan(dimensions = "height")
#' }
#' @export
tool_pan <- function(fig, dimensions = NULL, remove = FALSE) {
  args <- get_specified_args(nms = "dimensions")
  update_tool(fig, which = "pan", args, remove = remove)
}

#' Add "wheel_pan" tool to a Bokeh figure
#' @param Which dimension the wheel pan tool is constrained to act in. By default the wheel pan tool will pan the plot along the x-axis. Must be one of 'width' or 'height'.
#' @export
tool_wheel_pan <- function(fig, dimension = NULL, remove = FALSE) {
  args <- get_specified_args(nms = "dimension")
  update_tool(fig, "wheel_pan", args, remove = remove)
}

#' Add "reset" tool to a Bokeh figure
#' @param reset_size Whether activating the Reset tool should also reset the plot's canvas dimensions to their original size.
#' @export
tool_reset <- function(fig, reset_size = TRUE, remove = FALSE) {
  args <- get_specified_args(nms = "reset_size")
  update_tool(fig, "reset", args, remove = remove)
}

#' Add "undo" tool to a Bokeh figure
#' @export
tool_undo <- function(fig, remove = FALSE) {
  update_tool(fig, "undo", NULL, remove = remove)
}

#' Add "redo" tool to a Bokeh figure
#' @export
tool_redo <- function(fig, remove = FALSE) {
  update_tool(fig, "redo", NULL, remove = remove)
}

#' Add "save" tool to a Bokeh figure
#' @export
tool_save <- function(fig, remove = FALSE) {
  update_tool(fig, "save", NULL, remove = remove)
}

#' Add "help" tool to a Bokeh figure
#' @param redirect Site to be redirected through upon click.
#' @param help_tooltip Tooltip displayed when hovering over the help icon.
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
