#' Instantiate an rbokeh figure
#'
#' @param data default dataset to use
#' @export
figure <- function(
  data = NULL,
  width = 600,
  height = 600,
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  xlim = NULL,
  ylim = NULL,
  range_padding = 0.07,
  xgrid = TRUE,
  ygrid = TRUE,
  legend_location = "top_right",
  tools = c("pan", "wheel_zoom", "box_zoom", "reset", "save", "help"),
  logo = NULL,
  theme = getOption("bokeh_theme"),
  toolbar_location = "above",
  h_symmetry = TRUE,
  v_symmetry = FALSE,
  lod_factor = 10,
  lod_interval = 300,
  lod_threshold = NULL,
  lod_timeout = 500
  # output_backend = "canvas" # note: was "webgl = FALSE/TRUE"
  ) {

  plt <- Plot$new(plot_height = height, plot_width = width,
    h_symmetry = h_symmetry, v_symmetry = v_symmetry,
    toolbar_location = toolbar_location,
    lod_factor = lod_factor, lod_interval = lod_interval,
    lod_threshold = lod_threshold, lod_timeout = lod_timeout
    # output_backend = output_backend
  )

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
            draw = TRUE,
            log = FALSE,
            args = list(
              ticker = list(),
              grid = if (xgrid) list() else NA,
              tickformatter = list(),
              axis = list()
            )
          ),
          left = list(
            draw = TRUE,
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
