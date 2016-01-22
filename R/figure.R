#' Initialize a Bokeh figure
#'
#' @param data data to be supplied to all layers, if the layer doesn't supply a data value
#' @param width figure width in pixels
#' @param height figure width in pixels
#' @param title a title to display above the plot. - "title" is also the prefix for a set of Text Properties, so you can set the font for the title with the parameter text_font.
#' @param xlab label for x axis
#' @param ylab label for y axis
#' @param xlim the extent of the plotting area in the x-dimension (will be computed automatically if not specified).
#' @param ylim the extent of the plotting area in the y-dimension (will be computed automatically if not specified).
#' @param padding_factor if limits are not specified, by what factor should the extents of the data be padded
#' @param xgrid whether to draw x axis grid lines
#' @param ygrid whether to draw y axis grid lines
#' @param xaxes where to put x axis, or FALSE if no x axis ticks / labels
#' @param yaxes where to put y axis, or FALSE if no y axis ticks / labels
#' @param legend_location ('top_right', 'top_left', 'bottom_left', 'bottom_right') the location where the legend should draw itself, or NULL to omit the legend
#' @param tools character vector of interactivity tools options (acceptable values are: "pan", "wheel_zoom", "box_zoom", "resize", "crosshair", "box_select", "lasso_select", "reset", "save", "help").  Additionally, tool functions can be called on a figure to specify more control - see the "See Also" section below for a list of tool functions.  If \code{NULL}, the toolbar will not be drawn.  If \code{""} the toolbar will be drawn but no tools will be added by default.
#' @param theme an rbokeh theme to use
#' @param toolbar_location ('above', 'below', 'left', 'right') Where the toolbar will be located. If set to NULL, no toolbar will be attached to the plot.
#' @param h_symmetry (logical) Whether the total horizontal padding on both sides of the plot will be made equal (the left or right padding amount, whichever is larger).
#' @param v_symmetry (logical) Whether the total vertical padding on both sides of the plot will be made equal (the top or bottom padding amount, whichever is larger).
#' @param logo ('normal', 'grey') What version of the Bokeh logo to display on the toolbar. If set to NULL, no logo will be displayed.
#' @param lod_factor (integer) Decimation factor to use when applying level-of-detail decimation (see "Controlling level of detail").
#' @param lod_interval (integer) Interval (in ms) during which an interactive tool event will enable level-of-detail downsampling (see "Controlling level of detail").
#' @param lod_threshold (integer) A number of data points, above which level-of-detail downsampling may be performed by glyph renderers. Set to \code{NULL} to disable any level-of-detail downsampling (see "Controlling level of detail").
#' @param lod_timeout (integer) Timeout (in ms) for checking whether interactive tool events are still occurring. Once level-of-detail mode is enabled, a check is made every lod_timeout ms. If no interactive tool events have happened, level-of-detail mode is disabled (see "Controlling level of detail").
#' @param webgl (logical) should webgl be used for rendering?
#' @param \ldots parameters can be specified here that are available in \code{\link{theme_plot}}
#' @section Controlling level of detail:
#' Although the HTML canvas can comfortably display tens or even hundreds of thousands of glyphs, doing so can have adverse affects on interactive performance. In order to accommodate large-ish (but not enormous) data sizes, Bokeh plots offer "Level of Detail" (LOD) capability in the client.
#'
#' The basic idea is that during interactive operations (e.g., panning or zooming), the plot only draws some small fraction data points. This hopefully allows the general sense of the interaction to be preserved mid-flight, while maintaining interactive performance. See the \code{lod_} parameters for information on how to control this.
#' @examples
#' figure() %>% ly_points(1:10)
#' @seealso
#' Layers to add to a figure: \code{\link{ly_abline}};
#'   \code{\link{ly_annular_wedge}}; \code{\link{ly_annulus}};
#'   \code{\link{ly_arc}}; \code{\link{ly_bezier}};
#'   \code{\link{ly_boxplot}}; \code{\link{ly_contour}};
#'   \code{\link{ly_crect}}; \code{\link{ly_curve}};
#'   \code{\link{ly_density}}; \code{\link{ly_hist}};
#'   \code{\link{ly_image_url}}; \code{\link{ly_image}};
#'   \code{\link{ly_lines}}; \code{\link{ly_map}};
#'   \code{\link{ly_multi_line}}; \code{\link{ly_oval}};
#'   \code{\link{ly_patch}}; \code{\link{ly_points}};
#'   \code{\link{ly_polygons}}; \code{\link{ly_quadratic}};
#'   \code{\link{ly_quantile}}; \code{\link{ly_ray}};
#'   \code{\link{ly_segments}}; \code{\link{ly_text}};
#'   \code{\link{ly_wedge}}
#' Tools to add to a figure: \code{\link{tool_box_select}};
#'   \code{\link{tool_box_zoom}};
#'   \code{\link{tool_crosshair}};
#'   \code{\link{tool_lasso_select}};
#'   \code{\link{tool_reset}}; \code{\link{tool_resize}};
#'   \code{\link{tool_save}}; \code{\link{tool_wheel_zoom}}
#' Other figure types: \code{\link{grid_plot}}; \code{\link{gmap}}
#' @export
#' @import htmlwidgets
#' @import methods
figure <- function(
  data = NULL,
  width = 480,
  height = 520,
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  xlim = NULL,
  ylim = NULL,
  padding_factor = 0.07,
  xgrid = TRUE,
  ygrid = TRUE,
  xaxes = "below",
  yaxes = "left",
  legend_location = "top_right",
  tools = c("pan", "wheel_zoom", "box_zoom", "resize", "reset", "save", "help"),
  theme = getOption("bokeh_theme"),
  toolbar_location = "above",
  h_symmetry = TRUE,
  v_symmetry = FALSE,
  logo = "normal",
  lod_factor = 10,
  lod_interval = 300,
  lod_threshold = NULL,
  lod_timeout = 500,
  webgl = FALSE,
  ...
) {
  specified <- names(as.list(match.call())[-1])
  attr_pars <- c(as.list(environment())[specified], list(...))
  attr_pars <- attr_pars[names(attr_pars) %in% names(figure_par_validator_map)]

  dots <- list(...)

  ## figure of another type (like GMapPlot)
  if("type" %in% names(dots)) {
    type <- dots$type
  } else {
    type <- "Plot"
  }

  if(is.null(xlab) && !missing(xlab))
    xlab <- ""

  if(is.null(ylab) && !missing(ylab))
    ylab <- ""

  tt <- Sys.time()
  id <- gen_id(list(x = list(spec = list(time = tt))), type)

  model <- fig_model_skeleton(id, title, width, height, type)
  ref <- list(
    type = type,
    id = id
  )
  ref$subtype <- model$plot$subtype

  if(is.function(theme))
    theme <- theme()

  spec <- structure(list(
    figure_data = data,
    width = width, height = height, title = title,
    xlab = xlab, ylab = ylab,
    xlim = xlim, ylim = ylim, padding_factor = padding_factor,
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
    has_y_range = FALSE,
    legend_attrs = list(location = legend_location)
  ), class = "BokehFigure")

  extra_pars <- handle_extra_pars(attr_pars, figure_par_validator_map)
  epn <- names(extra_pars)

  if(!is.null(extra_pars[["min_border"]])) {
    if(is.null(extra_pars$min_border_left))
      extra_pars$min_border_left <- extra_pars$min_border
    if(is.null(extra_pars$min_border_right))
      extra_pars$min_border_right <- extra_pars$min_border
    if(is.null(extra_pars$min_border_top))
      extra_pars$min_border_top <- extra_pars$min_border
    if(is.null(extra_pars$min_border_bottom))
      extra_pars$min_border_bottom <- extra_pars$min_border
    extra_pars$min_border <- NULL
  }
  if(is.null(extra_pars$min_border_left))
    extra_pars$min_border_left <- 4
  if(is.null(extra_pars$min_border_right))
    extra_pars$min_border_right <- 4
  if(is.null(extra_pars$min_border_top))
    extra_pars$min_border_top <- 4
  if(is.null(extra_pars$min_border_bottom))
    extra_pars$min_border_bottom <- 4

  if(!"lod_threshold" %in% epn)
    extra_pars["lod_threshold"] <- list(NULL)

  if(is.null(tools))
    extra_pars["toolbar_location"] <- list(NULL)

  spec$model$plot$attributes <- c(spec$model$plot$attributes, extra_pars)

  fig <- htmlwidgets::createWidget(
     name = 'rbokeh',
     x = list(
        spec = spec,
        elementid = digest(Sys.time()),
        modeltype = type,
        modelid = id,
        docid = digest::digest(paste("rbokehfigure", Sys.time())),
        docs_json = list(list(
          version = get_bokeh_version(),
          title = "Bokeh Figure",
          roots = list(
            root_ids = list(id),
            references = NULL
        )))
     ),
     preRenderHook = rbokeh_prerender,
     width = spec$width,
     height = spec$height,
     package = 'rbokeh'
  )
  names(fig$x$docs_json) <- fig$x$docid

  ## check and add tools
  tool_list <- tools[tools %in% c("pan", "wheel_zoom", "box_zoom", "resize", "crosshair", "box_select", "lasso_select", "reset", "save", "help")]
  not_used <- setdiff(tool_list, tools)
  if(length(not_used) > 0)
    message("Note: tools not used: ", paste(not_used, collapse = ", "))
  for(tl in tool_list)
    fig <- eval(parse(text = paste("tool_", tl, "(fig)", sep = "")))

  fig
}

#' Retrieve rbokeh figure data
#'
#' @param fig rbokeh figure
#' @export
figure_data <- function(fig) {
  if (is.list(fig)) {
    # will return NULL even if it all doesn't exist,
    # as long as fig is a list
    return(fig$x$spec$figure_data)
  }

  return(NULL)
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
  "v_symmetry" = "logical",
  "lod_factor" = "int",
  "lod_interval" = "int",
  "lod_threshold" = "int",
  "lod_timeout" = "int",
  "webgl" = "logical"
)


# library(rvest)

# plots <- read_html("http://bokeh.pydata.org/en/latest/docs/reference/models/plots.html")

# attrs <- plots %>%
#   html_nodes("dl.attribute")

# attrs %>%
#   html_nodes("code.descname") %>%
#   html_text()
