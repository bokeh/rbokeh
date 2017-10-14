#' Add a "box" annotation to a Bokeh figure
#'
#' @param fig Figure to modify.
#' @family annotation functions
#' @export
ann_box <- function(fig, left = NULL, bottom = NULL, right = NULL, top = NULL,
  left_units = NULL,  bottom_units = NULL,  right_units = NULL,  top_units = NULL,
  color = NULL, alpha = NULL, lgroup = NULL, lname = NULL, ...) {

  spec <- c(list(
    left = left, bottom = bottom, right = right, top = top,
    left_units = left_units,  bottom_units = bottom_units,
    right_units = right_units,  top_units = top_units,
    color = color, alpha = alpha,
    model = "BoxAnnotation",
    type = "annotation",
    use_all_data = FALSE),
    ...)

  add_layer(fig, spec, lgroup, lname)
}





#' @export
ann_arrow <- function(fig, x_start = NULL, y_start = NULL, x_end = NULL, y_end = NULL,
  color = NULL, alpha = NULL, width = NULL, data = figure_data(fig),
  start = NULL, start_units = NULL, end = NULL, end_units = NULL,
  line_dash = NULL, line_join = NULL, line_dash_offset = NULL, line_cap = NULL,
  lgroup = NULL, lname = NULL
) {

  if (is.character(start))
    start <- arrow(start)
  if (is.character(end))
    end <- arrow(end)

  spec <- list(
    x_start = enquo(x_start), y_start = enquo(y_start),
    x_end = enquo(x_end), y_end = enquo(y_end),
    line_color = enquo(color), line_alpha = enquo(alpha), line_width = enquo(width),
    start = start, start_units = enquo(start_units),
    end = end, end_units = enquo(end_units),
    line_dash = enquo(line_dash), line_join = enquo(line_join),
    line_dash_offset = enquo(line_dash_offset), line_cap = enquo(line_cap),
    model = "Arrow",
    type = "annotation",
    data = data)

  add_layer(fig, spec, lgroup, lname)
}

# x_range_name
# y_range_name
# "normal", "open", "tee", "vee"
# "screen", "data"

#' @export
arrow <- function(type = c("normal", "open", "tee", "vee"), size = NULL,
  color = NULL, alpha = NULL,
  line_width = NULL, line_join = NULL, line_cap = NULL,
  line_dash = NULL, line_dash_offset = NULL,
  ...) {

  type_conv <- list(normal = "NormalHead", open = "OpenHead", tee = "TeeHead", vee = "VeeHead")
  type <- type_conv[[type[1]]]

  spec <- list(...)
  spec$type <- type
  spec$size <- size
  spec$alpha <- alpha
  spec$color <- color
  spec$line_width <- line_width
  spec$line_dash <- line_dash
  spec$line_dash_offset <- line_dash_offset
  spec$line_join <- line_join
  spec$line_cap <- line_cap

  spec
}

#' @export
get_arrow_mod <- function(spec, theme) {
  # resolve color -> line_color and fill_color
  if (is.null(spec$color))
    spec$color <- "black" # TODO: get this from theme
  if (is.null(spec$line_color))
    spec$line_color <- spec$color
  if (is.null(spec$fill_color))
    spec$fill_color <- spec$color

  # resolve alpha -> line_alphg and fill_alpha
  if (is.null(spec$alpha))
    spec$alpha <- 1
  if (is.null(spec$line_alpha))
    spec$line_alpha <- spec$alpha
  if (is.null(spec$fill_alpha))
    spec$fill_alpha <- spec$alpha * 0.5 # TODO: get this from theme

  type <- spec$type
  spec$type <- NULL

  mod <- utils::getFromNamespace(type, "rbokeh")
  par_nms <- names(bk_prop_types[[type]])
  do.call(mod$new, spec[intersect(names(spec), par_nms)])
}

#' @export
ann_band <- function(fig, base = NULL, lower = NULL, upper = NULL,
  base_units = NULL, lower_units = NULL, upper_units = NULL,
  dimension = c("height", "width"), color = NULL, alpha = NULL,
  data = figure_data(fig),
  line_width = NULL, line_dash = NULL, line_dash_offset = NULL,
  line_join = NULL, line_cap = NULL, lgroup = NULL, lname = NULL, ...) {

  spec <- list(
    base = enquo(base), lower = enquo(lower), upper = enquo(upper),
    base_units = enquo(base_units), lower_units = enquo(lower_units),
    upper_units = enquo(upper_units), dimension = dimension[1],
    color = enquo(color), alpha = enquo(alpha),
    line_width = enquo(line_width), line_dash = enquo(line_dash),
    line_dash_offset = enquo(line_dash_offset), line_join = enquo(line_join),
    line_cap = enquo(line_cap),
    data = data,
    model = "Band",
    type = "annotation",
    ...)

  add_layer(fig, spec, lgroup, lname)
}

# cat(paste(names(bk_prop_types$Whisker), collapse = "\n"))

# TODO: this is silly to specify lower_head and upper_head as arror models...
# instead set these models automatically and let other parameters easily
# control the attributes of the heads

#' @export
ann_whisker <- function(fig, base = NULL, lower = NULL, upper = NULL,
  lower_head = NULL, upper_head = NULL, dimension = NULL, data = figure_data(fig),
  base_units = NULL, lower_units = NULL, upper_units = NULL,
  line_color = NULL, line_alpha = NULL, line_width = NULL, line_dash = NULL,
  line_dash_offset = NULL, line_join = NULL, line_cap = NULL,
  lgroup = NULL, lname = NULL, ...) {

  if (is.character(lower_head))
    lower_head <- arrow(lower_head)
  if (is.character(upper_head))
    upper_head <- arrow(upper_head)

  spec <- list(
    base = enquo(base), lower = enquo(lower), upper = enquo(upper),
    lower_head = lower_head, upper_head = upper_head,
    dimension = enquo(dimension), base_units = enquo(base_units),
    lower_units = enquo(lower_units), upper_units = enquo(upper_units),
    line_color = enquo(line_color), line_alpha = enquo(line_alpha),
    line_width = enquo(line_width), line_dash = enquo(line_dash),
    line_dash_offset = enquo(line_dash_offset),
    line_join = enquo(line_join), line_cap = enquo(line_cap),
    data = data,
    model = "Whisker",
    type = "annotation",
    ...)

  add_layer(fig, spec, lgroup, lname)
}

#' @export
ann_labels <- function(fig, x = NULL, y = NULL, text = NULL, angle = NULL,
  data = figure_data(fig),
  x_units = NULL, y_units = NULL, angle_units = NULL, x_offset = NULL, y_offset = NULL,
  text_align = NULL, text_color = NULL, text_alpha = NULL, text_font = NULL,
  text_font_size = NULL, text_font_style = NULL, text_baseline = NULL,
  background_fill_color = NULL, background_fill_alpha = NULL, border_line_color = NULL,
  border_line_alpha = NULL, border_line_width = NULL, border_line_join = NULL,
  border_line_dash = NULL, border_line_dash_offset = NULL, border_line_cap = NULL,
  render_mode = NULL, lgroup = NULL, lname = NULL, ...) {

  spec <- list(
    x = enquo(x), y = enquo(y), text = enquo(text), angle = enquo(angle),
    x_units = enquo(x_units), y_units = enquo(y_units),
    angle_units = enquo(angle_units), x_offset = enquo(x_offset),
    y_offset = enquo(y_offset), text_align = enquo(text_align),
    text_color = enquo(text_color), text_alpha = enquo(text_alpha),
    text_font = enquo(text_font), text_font_size = enquo(text_font_size),
    text_font_style = enquo(text_font_style), text_baseline = enquo(text_baseline),
    background_fill_color = enquo(background_fill_color),
    background_fill_alpha = enquo(background_fill_alpha),
    border_line_color = enquo(border_line_color),
    border_line_alpha = enquo(border_line_alpha),
    border_line_width = enquo(border_line_width),
    border_line_join = enquo(border_line_join),
    border_line_dash = enquo(border_line_dash),
    border_line_dash_offset = enquo(border_line_dash_offset),
    border_line_cap = enquo(border_line_cap), render_mode = enquo(render_mode),
    data = data,
    model = "LabelSet",
    type = "annotation",
    ...)

  add_layer(fig, spec, lgroup, lname)
}

#' @export
ann_span <- function(fig, location = NULL, dimension = c("height", "width"),
  location_units = NULL, line_color = NULL, line_alpha = NULL,
  line_width = NULL, line_cap = NULL, line_join = NULL, line_dash = NULL,
  line_dash_offset = NULL, render_mode = NULL,
  lgroup = NULL, lname = NULL, ...) {

  spec <- list(
    location = enquo(location), location_units = enquo(location_units),
    dimension = dimension[1], line_color = enquo(line_color),
    line_alpha = enquo(line_alpha), line_width = enquo(line_width),
    line_cap = enquo(line_cap), line_join = enquo(line_join),
    line_dash = enquo(line_dash), line_dash_offset = enquo(line_dash_offset),
    render_mode = enquo(render_mode),
    model = "Span",
    type = "annotation",
    ...)

  add_layer(fig, spec, lgroup, lname)
}

#' @export
ann_poly <- function(fig, xs = NULL, ys = NULL, color = NULL, alpha = NULL,
  ys_units = NULL, xs_units = NULL, line_width = NULL, line_cap = NULL,
  line_join = NULL, line_dash = NULL, line_dash_offset = NULL,
  lgroup = NULL, lname = NULL, ...) {

  spec <- list(
    xs = enquo(xs), ys = enquo(ys), color = enquo(color), alpha = enquo(alpha),
    ys_units = enquo(ys_units), xs_units = enquo(xs_units),
    line_width = enquo(line_width), line_cap = enquo(line_cap),
    line_join = enquo(line_join), line_dash = enquo(line_dash),
    line_dash_offset = enquo(line_dash_offset),
    model = "PolyAnnotation",
    type = "annotation",
    ...)

  add_layer(fig, spec, lgroup, lname)
}

#' @export
ann_title <- function(fig, text = NULL, align = NULL, text_color = NULL,
  text_alpha = NULL, text_font_size = NULL, text_font = NULL,
  text_font_style = NULL, offset = NULL, background_fill_color = NULL,
  background_fill_alpha = NULL, border_line_color = NULL, border_line_alpha = NULL,
  border_line_width = NULL, border_line_join = NULL, border_line_cap = NULL,
  border_line_dash = NULL, border_line_dash_offset = NULL, render_mode = NULL) {

  ttl <- fig$x$pars$title
  if (is.null(ttl))
    ttl <- list()

  specified <- setdiff(names(as.list(match.call())[-1]), "fig")
  if (length(specified) > 0) {
    pars <- as.list(environment())[specified]
    ttl[names(pars)] <- pars
  }

  fig$x$pars$title <- ttl

  fig
}
