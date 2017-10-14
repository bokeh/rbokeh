#' Instantiate an rbokeh figure
#'
#' @param data A default data frame to be used by the layers added to the figure.
#' @param width TODO_DOC
#' @param height TODO_DOC
#' @param title TODO_DOC
#' @param title_location Where the title will be located. Titles on the left or right side will  be rotated. One of 'above', 'below', 'left', 'right'.
#' @param xlab TODO_DOC
#' @param ylab TODO_DOC
#' @param xlim TODO_DOC
#' @param ylim TODO_DOC
#' @param range_padding TODO_DOC
#' @param xgrid TODO_DOC
#' @param ygrid TODO_DOC
#' @param xaxes TODO_DOC
#' @param yaxes TODO_DOC
#' @param legend_location TODO_DOC
#' @param tools TODO_DOC
#' @param logo TODO_DOC
#' @param theme TODO_DOC
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
  xaxes = TRUE,
  yaxes = TRUE,
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
            draw = yaxes,
            log = FALSE,
            args = list(
              ticker = list(),
              grid = if (xgrid) list() else NA,
              tickformatter = list(),
              axis = list()
            )
          ),
          left = list(
            draw = xaxes,
            log = FALSE,
            args = list(
              ticker = list(),
              grid = if (ygrid) list() else NA,
              tickformatter = list(),
              axis = list()
            )
          ),
          above = list(
            draw = FALSE,
            log = FALSE,
            args = list(
              ticker = list(),
              grid = if (xgrid) list() else NA,
              tickformatter = list(),
              axis = list()
            )
          ),
          right = list(
            draw = FALSE,
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

### present:
# h_symmetry
# v_symmetry
# plot_height
# plot_width
# title
# lod_factor
# lod_interval
# lod_threshold
# lod_timeout

### to add:
# min_border
# min_border_bottom
# min_border_left
# min_border_right
# min_border_top
# output_backend ('canvas', 'svg', 'webgl') (causes an error in rendering...)

### look into:
# title_location
# tool_events
# toolbar
# toolbar_location
# toolbar_sticky

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

### ignore
## inner_height
## inner_width
## layout_height
## layout_width
## above
## below
## left
## right
## extra_x_ranges
## extra_y_ranges
## hidpi


figure_data <- function(fig) {
  if (is.list(fig)) {
    # will return NULL even if it all doesn't exist,
    # as long as fig is a list
    return(fig$x$data)
  }

  return(NULL)
}

rbokeh_prerender <- function(obj, keep_aux = FALSE) {

  obj <- prepare_figure(obj)

  mod_list <- unname(unlist(obj$x$mods))
  mod_list <- mod_list[sapply(mod_list, function(a) inherits(a, "Model"))]

  fig <- list(list(
    version = "0.12.9",
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
