#' Instantiate an rbokeh figure.
#'
#' @param data A default data frame to be used by the layers added to the figure.
#' @param width Figure width in pixels.
#' @param height Figure width in pixels.
#' @param title A title to display on the plot.
#' @param title_location Where the title will be located. Titles on the left or right side will  be rotated. One of 'above', 'below', 'left', 'right'.
#' @param xlab Label for x axis. Also specifiable in \code{\link{x_axis}}.
#' @param ylab Label for y axis. Also specifiable in \code{\link{y_axis}}.
#' @param xlim The extent of the plotting area in the x-dimension (will be computed automatically if not specified). Also specifiable in \code{\link{x_range}}.
#' @param ylim The extent of the plotting area in the y-dimension (will be computed automatically if not specified). Also specifiable in \code{\link{y_range}}.
#' @param range_padding If limits are not explicitly specified, by what factor should the computed extents of the data be padded? This is a number used as a multiplier of the computed range.
#' @param xgrid Logical indicating whether to draw x axis grid lines.
#' @param ygrid Logical indicating whether to draw y axis grid lines.
#' @param xaxes Where to put x axis, or FALSE if no x axis ticks / labels.
#' @param yaxes Where to put y axis, or FALSE if no y axis ticks / labels.
#' @param legend_location The location where the legend should draw itself, or \code{NULL} to omit the legend. One of 'top_right', 'top_left', 'bottom_left', 'bottom_right'.
#' @param logo What version of the Bokeh logo to display on the toolbar. If set to \code{NULL}, no logo will be displayed. One of 'normal', 'grey', or \code{NULL}.
#' @param tools character vector of interactivity tools options (acceptable values are: "box_select", "lasso_select", "poly_select", "crosshair", "box_zoom", "wheel_zoom", "zoom_in", "zoom_out", "pan", "wheel_pan", "reset", "undo", "redo", "save", "help").  Additionally, tool functions can be called on a figure to specify more control - see \code{\link{tool_box_select}}, for example, where links to the rest of the family of tool functions can also be found. If \code{NULL}, the toolbar will not be drawn.  If \code{""} the toolbar will be drawn but no tools will be added by default.
#' @param theme An rbokeh theme to use. See, for example, \code{\link{bk_default_theme}}.
#' @param toolbar_location Where the toolbar will be located. If set to None, no toolbar will be attached to the plot. One of 'above', 'below', 'left', 'right'.
#' @param toolbar_sticky Stick the toolbar to the edge of the plot. Default: TRUE If FALSE, the toolbar will be outside of the axes, titles etc.
#' @param v_symmetry Whether the total vertical padding on both sides of the plot will be made equal (the top or bottom padding amount, whichever is larger).
#' @param h_symmetry Whether the total horizontal padding on both sides of the plot will be made equal (the left or right padding amount, whichever is larger).
#' @param lod_factor Decimation factor to use when applying level-of-detail decimation.
#' @param lod_interval Interval (in ms) during which an interactive tool event will enable level-of-detail downsampling.
#' @param lod_threshold A number of data points, above which level-of-detail downsampling may be performed by glyph renderers. Set to ``None`` to disable any level-of-detail downsampling.
#' @param lod_timeout Timeout (in ms) for checking whether interactive tool events are still occurring. Once level-of-detail mode is enabled, a check is made every ``lod_timeout`` ms. If no interactive tool events have happened, level-of-detail mode is disabled.
#' @param output_backend Specify the output backend for the plot area. Default is "canvas", HTML5 Canvas. Note: When set to "webgl", glyphs without a WebGL rendering implementation will fall back to rendering onto 2D canvas. Must be one of 'canvas', 'svg', 'webgl'.
#' @param min_border A convenience property to set all all the ``min_border_X`` properties to the same value. If an individual border property is explicitly set, it will override ``min_border``.
#' @param min_border_left Minimum size in pixels of the padding region to the left of the central plot region. Note: This is a *minimum*. The padding region may expand as needed to accommodate titles or axes, etc.
#' @param min_border_bottom Minimum size in pixels of the padding region below the bottom of the central plot region. Note: This is a *minimum*. The padding region may expand as needed to accommodate titles or axes, etc.
#' @param min_border_right Minimum size in pixels of the padding region to the right of the central plot region. Note: This is a *minimum*. The padding region may expand as needed to accommodate titles or axes, etc.
#' @param min_border_top Minimum size in pixels of the padding region above the top of the central plot region. Note: This is a *minimum*. The padding region may expand as needed to accommodate titles or axes, etc.
#' @param hidpi Whether to use HiDPI mode when available.
#' @examples
#' # empty figure
#' figure()
#'
#' # figure with single point
#' figure() %>%
#'   ly_points(1, 1)
#'
#' # simple figure
#' figure() %>%
#'   ly_points(1:10, 1:10)
#'
#' # use svg to render the plot
#' figure(output_backend = "svg") %>%
#'   ly_points(1:10, 1:10)
#'
#' # remove axes and grid
#' figure(xaxes = FALSE, yaxes = FALSE, xgrid = FALSE, ygrid = FALSE) %>%
#'   ly_points(1:10, 1:10)
#'
#' # axis above and right
#' figure(xaxes = "above", yaxes = "right", xgrid = FALSE, ygrid = FALSE,
#'   toolbar_location = "left") %>%
#'   ly_points(1:10, 1:10)
#'
#' # custom limits
#' figure(xlim = c(-10, 20)) %>%
#'   ly_points(1:10, 1:10)
#'
#' # specifying tools
#' figure(tools = "crosshair") %>%
#'   ly_points(1:10, 1:10)
#'
#' # specifying tools
#' figure(tools = c("lasso_select", "poly_select")) %>%
#'   ly_points(1:10, 1:10)
#'
#' # gray logo
#' figure(logo = "grey") %>%
#'   ly_points(1:10, 1:10)
#'
#' # no toolbar
#' figure(tools = NULL) %>%
#'   ly_points(1:10, 1:10)
#' @export
figure <- function(
  data = NULL,
  width = 600,
  height = 600,
  title = NULL,
  title_location = NULL,
  xlab = NULL,
  ylab = NULL,
  xlim = NULL,
  ylim = NULL,
  range_padding = 0.07,
  xgrid = TRUE,
  ygrid = TRUE,
  xaxes = "below",
  yaxes = "left",
  legend_location = "top_right",
  logo = NULL,
  theme = getOption("bokeh_theme"),
  tools = c("pan", "wheel_zoom", "box_zoom", "reset", "save", "help"),
  toolbar_location = "above",
  toolbar_sticky = NULL,
  h_symmetry = TRUE,
  v_symmetry = FALSE,
  lod_factor = 10,
  lod_interval = 300,
  lod_threshold = NULL,
  lod_timeout = 500,
  output_backend = "canvas",
  min_border = NULL,
  min_border_left = NULL,
  min_border_bottom = NULL,
  min_border_right = NULL,
  min_border_top = NULL,
  hidpi = NULL
  ) {

  args <- get_specified_args(nms = c(
    "h_symmetry", "v_symmetry", "toolbar_location", "lod_factor",
    "lod_interval", "lod_threshold", "lod_timeout", "output_backend",
    "toolbar_sticky", "title_location", "min_border", "min_border_left",
    "min_border_bottom", "min_border_right", "min_border_top", "hidpi"))
  args$plot_height <- height
  args$plot_width <- width

  if (!is.null(args$title) && !is.null(args$title_location))
    args$title_location <- NULL

  axis_left <- axis_below <- TRUE
  axis_right <- axis_above <- FALSE
  if (is.logical(xaxes) && !xaxes) {
    axis_below <- FALSE
  } else if (xaxes == "above") {
    axis_below <- FALSE
    axis_above <- TRUE
  }
  if (is.logical(yaxes) && !yaxes) {
    axis_left <- FALSE
  } else if (yaxes == "right") {
    axis_left <- FALSE
    axis_right <- TRUE
  }

  plt <- do.call(Plot$new, args)

  obj <- htmlwidgets::createWidget(
    name = "rbokeh",
    x = list(
      mods = list(plot = plt),
      layers = list(),
      pars = list(
        gen = list(
          logo = logo, tools = tools,
          labs = list(x = xlab, y = ylab),
          range_padding = list(x = range_padding, y = range_padding)
        ),
        title = list(text = title),
        legend = list(legend_location = legend_location),
        tools = structure(lapply(tools, function(x) list()), names = tools),
        ranges = list(
          x = list(
            type = NULL,
            lims_infer = NULL,
            lims_spec = xlim,
            args = list()
          ),
          y = list(
            type = NULL,
            lims_infer = NULL,
            lims_spec = ylim,
            args = list()
          )
        ),
        axes = list(
          below = list(
            draw = axis_below,
            log = FALSE,
            args = list(
              ticker = list(),
              grid = if (xgrid) list() else NA,
              tickformatter = list(),
              axis = list()
            )
          ),
          left = list(
            draw = axis_left,
            log = FALSE,
            args = list(
              ticker = list(),
              grid = if (ygrid) list() else NA,
              tickformatter = list(),
              axis = list()
            )
          ),
          above = list(
            draw = axis_above,
            log = FALSE,
            args = list(
              ticker = list(),
              grid = if (xgrid) list() else NA,
              tickformatter = list(),
              axis = list()
            )
          ),
          right = list(
            draw = axis_right,
            type = NULL,
            log = FALSE,
            args = list(
              ticker = list(),
              grid = if (ygrid) list() else NA,
              tickformatter = list(),
              axis = list()
            )
          )
        )
      ),
      debug = FALSE,
      theme = theme,
      data = data
    ),
    sizingPolicy = htmlwidgets::sizingPolicy(defaultWidth = 500, defaultHeight = 500),
    preRenderHook = rbokeh_prerender,
    width = width,
    height = height,
    package = "rbokeh"
  )

  obj
}

### taken care of in other places:
# background_fill_alpha
# background_fill_color
# border_fill_alpha
# border_fill_color
# outline_line_alpha
# outline_line_cap
# outline_line_color
# outline_line_dash
# outline_line_dash_offset
# outline_line_join
# outline_line_width
# renderers
# x_range
# x_scale
# y_range
# y_scale

# internal
figure_data <- function(fig) {
  if (is.list(fig)) {
    # will return NULL even if it all doesn't exist,
    # as long as fig is a list
    return(fig$x$data)
  }

  return(NULL)
}

# internal
rbokeh_prerender <- function(obj, keep_aux = FALSE) {

  obj <- prepare_figure(obj)

  mod_list <- unname(unlist(obj$x$mods))
  mod_list <- mod_list[sapply(mod_list, function(a) inherits(a, "Model"))]

  fig <- list(list(
    version = "0.12.10",
    title = "Bokeh Figure",
    roots = list(
      root_ids = list(obj$x$mods$plot$get_prop("id")),
      references = lapply(mod_list, function(a) a$get_all_props())
    )
  ))
  docid <- digest::digest(Sys.time())
  names(fig) <- docid

  obj$x$docs_json <- fig

  obj$x$elementid <- digest::digest(Sys.time())
  obj$x$modelid <- obj$x$mods$plot$get_prop("id")
  obj$x$docid <- docid

  if (!keep_aux) {
    obj$x$mods <- NULL
    obj$x$layers <- NULL
    obj$x$theme <- NULL
    obj$x$pars <- NULL
    obj$x$data <- NULL
  }

  obj$preRenderHook <- NULL # nolint

  attr(obj$x, "TOJSON_ARGS") <- list(auto_unbox = TRUE, null = "null", na = "null")

  obj
}
