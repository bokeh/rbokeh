# dput(setdiff(names(formals(ann_box)), c("...", "lname", "lgroup", "fig", "alpha", "color")))

# get_docs(mods, "BoxAnnotation",
#   c("left", "bottom", "right", "top", "left_units", "bottom_units",
# "right_units", "top_units")) %>% cat()

#' Add a "box" annotation (rectangular shaded region) to a Bokeh figure
#'
#' @param fig Figure to modify.
#' @param left The x-coordinates of the left edge of the box annotation. Datetime values are also accepted, but note that they are immediately converted to milliseconds-since-epoch.
#' @param bottom The y-coordinates of the bottom edge of the box annotation. Datetime values are also accepted, but note that they are immediately converted to milliseconds-since-epoch.
#' @param right The x-coordinates of the right edge of the box annotation. Datetime values are also accepted, but note that they are immediately converted to milliseconds-since-epoch.
#' @param top The y-coordinates of the top edge of the box annotation. Datetime values are also accepted, but note that they are immediately converted to milliseconds-since-epoch.
#' @param left_units The unit type for the left attribute. Interpreted as "data space" units by default. One of "data" or "screen".
#' @param bottom_units The unit type for the bottom attribute. Interpreted as "data space" units by default. One of "data" or "screen".
#' @param right_units The unit type for the right attribute. Interpreted as "data space" units by default. One of "data" or "screen".
#' @param top_units The unit type for the top attribute. Interpreted as "data space" units by default. One of "data" or "screen".
#' @template par-coloralpha
#' @template par-lnamegroup
#' @template dots-fillline
#' @family annotation functions
#' @export
#' @examples
# box annotation
#' figure(data = iris) %>%
#'   ly_points(x = Sepal.Width, y = Sepal.Length, color = Species) %>%
#'   ann_box(bottom = 5, color = "black")
#'
#' # with screen units
#' figure(data = iris) %>%
#'   ly_points(x = Sepal.Width, y = Sepal.Length, color = Species) %>%
#'   ann_box(bottom = 540, bottom_units = "screen", color = "black")
#'
#' # text annotation with box annotation and screen units to illustrate
#' # an inside-plot heading that doesn't move with pan/zoom
#' figure(data = iris) %>%
#'   ly_points(x = Sepal.Width, y = Sepal.Length, color = Species) %>%
#'   ann_box(bottom = 565, bottom_units = "screen", color = "black") %>%
#'   ann_labels(200, 570, "Header", text_color = "white",
#'     x_units = "screen", y_units = "screen")
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
    quos(...))

  add_layer(fig, spec, lgroup, lname)
}


# dput(setdiff(names(formals(ann_arrow)),
#   c("...", "lname", "lgroup", "fig", "alpha", "color", "data")))

# get_docs(mods, "Arrow",
#   c("x_start", "y_start", "x_end", "y_end", "line_width", "start",
#   "start_units", "end", "end_units", "line_dash", "line_join",
#   "line_dash_offset", "line_cap")) %>% cat()

#' Add an "arrow" annotation to a Bokeh figure
#' @param fig Figure to modify.
#' @param x_start The x-coordinates to locate the start of the arrows.
#' @param y_start The y-coordinates to locate the start of the arrows.
#' @param x_end The x-coordinates to locate the end of the arrows.
#' @param y_end The y-coordinates to locate the end of the arrows.
#' @template par-coloralpha
#' @param line_width The line width values for the arrow body.
#' @param data An optional data frame supplying data to which other parameters can refer.
#' @param start Instance of ArrowHead.
#' @param start_units The unit type for the start_x and start_y attributes. Interpreted as "data space" units by default.
#' @param end Instance of ArrowHead.
#' @param end_units The unit type for the end_x and end_y attributes. Interpreted as "data space" units by default.
#' @param line_dash The line dash values for the arrow body.
#' @param line_join The line join values for the arrow body.
#' @param line_dash_offset The line dash offset values for the arrow body.
#' @param line_cap The line cap values for the arrow body.
#' @template par-lnamegroup
#' @template dots-fillline
#' @family annotation functions
#' @examples
#' figure() %>%
#'   ly_points(1:10, 1:10) %>%
#'   ann_arrow(3, 5, 4, 4)
#'
#' # with different head
#' figure() %>%
#'   ly_points(1:10, 1:10) %>%
#'   ann_arrow(3, 5, 4, 4, end = "vee")
#'
#' # with more specific head parameters using "arrow()"
#' figure() %>%
#'   ly_points(1:10, 1:10) %>%
#'   ann_arrow(3, 5, 4, 4, end = arrow("vee", color = "red"))
#'
#' # vectorized example using a data frame of values as input
#' da <- cbind(expand.grid(1:4, 3:8), cbind(expand.grid(2:5, 4:9)))
#' names(da) <- c("x1", "y1", "x2", "y2")
#' figure() %>%
#'   ly_points(x = 1:5, y = 3:7) %>%
#'   ann_arrow(x1, y1, x2, y2, data = da,
#'     end = arrow("vee", color = "red", size = 50), start = arrow("tee", color = "red"))
#' @export
ann_arrow <- function(fig, x_start = NULL, y_start = NULL, x_end = NULL, y_end = NULL,
  color = NULL, alpha = NULL, line_width = NULL, data = figure_data(fig),
  start = NULL, start_units = NULL, end = NULL, end_units = NULL,
  line_dash = NULL, line_join = NULL, line_dash_offset = NULL, line_cap = NULL,
  lgroup = NULL, lname = NULL, ...
) {

  if (is.character(start))
    start <- arrow(start)
  if (is.character(end))
    end <- arrow(end)

  spec <- list(
    x_start = enquo(x_start), y_start = enquo(y_start),
    x_end = enquo(x_end), y_end = enquo(y_end),
    line_color = enquo(color), line_alpha = enquo(alpha),
    line_width = enquo(line_width),
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

# dput(setdiff(names(formals(arrow)), c("...", "lname", "lgroup", "fig", "alpha", "color")))

# get_docs(mods, "NormalHead",
#   c("size", "line_width", "line_join", "line_cap", "line_dash",
#     "line_dash_offset")) %>% cat()

#' Specify details of an arrow head.
#' @param type The arrow head type. One of "normal", "open", "tee", "vee".
#' @param size The size, in pixels, of the arrow head.
#' @template par-coloralpha
#' @param line_width The line width values for the arrow head outline.
#' @param line_join The line join values for the arrow head outline.
#' @param line_cap The line cap values for the arrow head outline.
#' @param line_dash The line dash values for the arrow head outline.
#' @param line_dash_offset The line dash offset values for the arrow head outline.
#' @param \ldots Additional parameters to pass to the arrow head specification.
#' @note See \code{\link{ann_arrow}} for examples.
#' @family annotation functions
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

# internal
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

# dput(setdiff(names(formals(ann_arrow)),
#   c("...", "lname", "lgroup", "fig", "alpha", "color", "data")))

# get_docs(mods, "Arrow",
#   c("x_start", "y_start", "x_end", "y_end", "line_width", "start",
#   "start_units", "end", "end_units", "line_dash", "line_join",
#   "line_dash_offset", "line_cap")) %>% cat()


# dput(setdiff(names(formals(ann_band)),
#   c("...", "lname", "lgroup", "fig", "alpha", "color", "data")))

# get_docs(mods, "Band",
#   c("base", "lower", "upper", "base_units", "lower_units", "upper_units",
#     "dimension", "line_width", "line_dash", "line_dash_offset",
#     "line_join", "line_cap")) %>% cat()

#' Add a "band" annotation (filled area band) to a Bokeh figure
#' @param fig Figure to modify.
#' @param base The orthogonal coordinates of the upper and lower values.
#' @param lower The coordinates of the lower portion of the filled area band.
#' @param upper The coordinations of the upper portion of the filled area band.
#' @param base_units Either "screen" or "data".
#' @param lower_units Either "screen" or "data".
#' @param upper_units Either "screen" or "data".
#' @param dimension The direction of the band.
#' @template par-coloralpha
#' @param data An optional data frame supplying data to which other parameters can refer.
#' @param line_width The line width values for the band.
#' @param line_dash The line dash values for the band.
#' @param line_dash_offset The line dash offset values for the band.
#' @param line_join The line join values for the band.
#' @param line_cap The line cap values for the band.>
#' @template par-lnamegroup
#' @template dots-fillline
#' @family annotation functions
#' @export
#' @examples
# band annotation example
#' x <- runif(100)
#' d <- data.frame(x = x, y = x ^ 2 + rnorm(100, sd = 0.1))
#' mod <- loess(y ~ x, data = d)
#' sx <- seq(min(x), max(x), length = 100)
#' pr <- predict(mod, newdata = sx, se = TRUE)
#' bd <- data.frame(x = sx, l = pr$fit - pr$se.fit * 2, u = pr$fit + pr$se.fit * 2)
#' figure() %>%
#'   ly_points(x, y, data = d) %>%
#'   ann_band(x, l, u, data = bd, dimension = "height", color = "gray")
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
    quos(...))

  add_layer(fig, spec, lgroup, lname)
}

# cat(paste(names(bk_prop_types$Whisker), collapse = "\n"))

# dput(setdiff(names(formals(ann_whisker)),
#   c("...", "lname", "lgroup", "fig", "alpha", "color", "data")))

# get_docs(mods, "Whisker",
#   c("base", "lower", "upper", "lower_head", "upper_head", "dimension",
# "base_units", "lower_units", "upper_units", "line_color", "line_alpha",
# "line_width", "line_dash", "line_dash_offset", "line_join", "line_cap")) %>% cat()

# TODO: this is silly to specify lower_head and upper_head as arror models...
# instead set these models automatically and let other parameters easily
# control the attributes of the heads

#' Add a "whisker" annotation to a Bokeh figure
#' @param fig Figure to modify.
#' @param base The orthogonal coordinates of the upper and lower values.
#' @param lower The coordinates of the lower end of the whiskers.
#' @param upper The coordinations of the upper end of the whiskers.
#' @param lower_head Instance of ArrowHead.
#' @param upper_head Instance of ArrowHead.
#' @param dimension The direction of the band.
#' @param data An optional data frame supplying data to which other parameters can refer.
#' @param base_units Either "screen" or "data".
#' @param lower_units Either "screen" or "data".
#' @param upper_units Either "screen" or "data".
#' @param line_color The line color values for the whisker body.
#' @param line_alpha The line alpha values for the whisker body.
#' @param line_width The line width values for the whisker body.
#' @param line_dash The line dash values for the whisker body.
#' @param line_dash_offset The line dash offset values for the whisker body.
#' @param line_join The line join values for the whisker body.
#' @param line_cap The line cap values for the whisker body.
#' @template par-lnamegroup
#' @family annotation functions
#' @export
ann_whisker <- function(fig, base = NULL, lower = NULL, upper = NULL,
  lower_head = NULL, upper_head = NULL, dimension = NULL, data = figure_data(fig),
  base_units = NULL, lower_units = NULL, upper_units = NULL,
  line_color = NULL, line_alpha = NULL, line_width = NULL, line_dash = NULL,
  line_dash_offset = NULL, line_join = NULL, line_cap = NULL,
  lgroup = NULL, lname = NULL) {

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
    type = "annotation")

  add_layer(fig, spec, lgroup, lname)
}


# dput(setdiff(names(formals(ann_labels)),
#   c("...", "lname", "lgroup", "fig", "alpha", "color", "data")))

# get_docs(mods, "LabelSet",
#   c("x", "y", "text", "angle", "x_units", "y_units", "angle_units",
# "x_offset", "y_offset", "text_align", "text_color", "text_alpha",
# "text_font", "text_font_size", "text_font_style", "text_baseline",
# "background_fill_color", "background_fill_alpha", "border_line_color",
# "border_line_alpha", "border_line_width", "border_line_join",
# "border_line_dash", "border_line_dash_offset", "border_line_cap",
# "render_mode")) %>% cat()

#' Add a "label" annotation to a Bokeh figure
#' @param fig Figure to modify.
#' @param x The x-coordinates to locate the text anchors.
#' @param y The y-coordinates to locate the text anchors.
#' @param text The text values to render.
#' @param angle The angles to rotate the text, as measured from the horizontal. Warning: The center of rotation for canvas and css render_modes is different. For \code{render_mode="canvas"} the label is rotated from the top-left corner of the annotation, while for \code{render_mode="css"} the annotation is rotated around it's center.
#' @param x_units The unit type for the xs attribute. Interpreted as "data space" units by default. One of "data" or "screen".
#' @param y_units The unit type for the ys attribute. Interpreted as "data space" units by default. One of "data" or "screen".
#' @param angle_units One of "rad" or "deg".
#' @param data An optional data frame supplying data to which other parameters can refer.
#' @param x_offset Offset values to apply to the x-coordinates. This is useful, for instance, if it is desired to "float" text a fixed distance in screen units from a given data position.
#' @param y_offset Offset values to apply to the y-coordinates. This is useful, for instance, if it is desired to "float" text a fixed distance in screen units from a given data position.
#' @param text_align The text align values for the text.
#' @param text_color The text color values for the text.
#' @param text_alpha The text alpha values for the text.
#' @param text_font The text font values for the text.
#' @param text_font_size The text font size values for the text.
#' @param text_font_style The text font style values for the text.
#' @param text_baseline The text baseline values for the text.
#' @param background_fill_color The fill color values for the text bounding box.
#' @param background_fill_alpha The fill alpha values for the text bounding box.
#' @param border_line_color The line color values for the text bounding box.
#' @param border_line_alpha The line alpha values for the text bounding box.
#' @param border_line_width The line width values for the text bounding box.
#' @param border_line_join The line join values for the text bounding box.
#' @param border_line_dash The line dash values for the text bounding box.
#' @param border_line_dash_offset The line dash offset values for the text bounding box.
#' @param border_line_cap The line cap values for the text bounding box.
#' @param render_mode Specifies whether the text is rendered as a canvas element or as an css element overlaid on the canvas. The default mode is "canvas". Note: The CSS labels won't be present in the output using the "save" tool. Warning: Not all visual styling properties are supported if the render_mode is set to "css". The border_line_dash property isn't fully supported and border_line_dash_offset isn't supported at all. Setting text_alpha will modify the opacity of the entire background box and border in addition to the text. Finally, clipping label annotations inside of the plot area isn't supported in "css" mode.
#' @template par-lnamegroup
#' @family annotation functions
#' @export
#' @examples
#' d <- data.frame(x = 1:10, y = rnorm(10), txt = letters[1:10])
#' figure(data = d) %>%
#'   ly_points(x, y) %>%
#'   ann_labels(x, y, txt)
ann_labels <- function(fig, x = NULL, y = NULL, text = NULL, angle = NULL,
  data = figure_data(fig),
  x_units = NULL, y_units = NULL, angle_units = NULL, x_offset = NULL, y_offset = NULL,
  text_align = NULL, text_color = NULL, text_alpha = NULL, text_font = NULL,
  text_font_size = NULL, text_font_style = NULL, text_baseline = NULL,
  background_fill_color = NULL, background_fill_alpha = NULL, border_line_color = NULL,
  border_line_alpha = NULL, border_line_width = NULL, border_line_join = NULL,
  border_line_dash = NULL, border_line_dash_offset = NULL, border_line_cap = NULL,
  render_mode = NULL, lgroup = NULL, lname = NULL) {

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
    type = "annotation")

  add_layer(fig, spec, lgroup, lname)
}

# dput(setdiff(names(formals(ann_span)),
#   c("...", "lname", "lgroup", "fig", "alpha", "color", "data")))

# get_docs(mods, "Span",
#   c("location", "dimension", "location_units", "line_color", "line_alpha",
# "line_width", "line_cap", "line_join", "line_dash", "line_dash_offset",
# "render_mode")) %>% cat()

#' Add a "span" (horizontal or vertical line) annotation to a Bokeh figure
#' @param fig Figure to modify.
#' @param location The location of the span, along "dimension".
#' @param dimension The direction of the span.
#' @param location_units The unit type for the location attribute. Interpreted as "data space" units by default.
#' @param line_color The line color values for the span.
#' @param line_alpha The line alpha values for the span.
#' @param line_width The line width values for the span.
#' @param line_cap The line cap values for the span.
#' @param line_join The line join values for the span.
#' @param line_dash The line dash values for the span.
#' @param line_dash_offset The line dash offset values for the span.
#' @param render_mode Specifies whether the span is rendered as a canvas element or as a css element overlaid on the canvas. The default mode is "canvas". Warning: The line_dash and line_dash_offset attributes aren't supported if the render_mode is set to "css".
#' @template par-lnamegroup
#' @family annotation functions
#' @export
ann_span <- function(fig, location = NULL, dimension = c("height", "width"),
  location_units = NULL, line_color = NULL, line_alpha = NULL,
  line_width = NULL, line_cap = NULL, line_join = NULL, line_dash = NULL,
  line_dash_offset = NULL, render_mode = NULL,
  lgroup = NULL, lname = NULL) {

  spec <- list(
    location = enquo(location), location_units = enquo(location_units),
    dimension = dimension[1], line_color = enquo(line_color),
    line_alpha = enquo(line_alpha), line_width = enquo(line_width),
    line_cap = enquo(line_cap), line_join = enquo(line_join),
    line_dash = enquo(line_dash), line_dash_offset = enquo(line_dash_offset),
    render_mode = enquo(render_mode),
    model = "Span",
    type = "annotation")

  add_layer(fig, spec, lgroup, lname)
}

# dput(setdiff(names(formals(ann_poly)),
#   c("...", "lname", "lgroup", "fig", "alpha", "color", "data")))

# get_docs(mods, "PolyAnnotation",
#   c("xs", "ys", "ys_units", "xs_units", "line_width", "line_cap",
# "line_join", "line_dash", "line_dash_offset")) %>% cat()

#' Add a "polygon" annotation to a Bokeh figure
#' @param fig Figure to modify.
#' @param xs The x-coordinates of the region to draw.
#' @param ys The y-coordinates of the region to draw.
#' @template par-coloralpha
#' @param xs_units The unit type for the xs attribute. Interpreted as "data space" units by default. One of "data" or "screen".
#' @param ys_units The unit type for the ys attribute. Interpreted as "data space" units by default. One of "data" or "screen".
#' @param line_width The line width values for the polygon.
#' @param line_cap The line cap values for the polygon.
#' @param line_join The line join values for the polygon.
#' @param line_dash The line dash values for the polygon.
#' @param line_dash_offset The line dash offset values for the polygon.
#' @template par-lnamegroup
#' @template dots-fillline
#' @family annotation functions
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
    quos(...))

  add_layer(fig, spec, lgroup, lname)
}

# dput(setdiff(names(formals(ann_title)),
#   c("...", "lname", "lgroup", "fig", "alpha", "color", "data")))

# get_docs(mods, "Title",
#   c("text", "align", "text_color", "text_alpha", "text_font_size",
# "text_font", "text_font_style", "offset", "background_fill_color",
# "background_fill_alpha", "border_line_color", "border_line_alpha",
# "border_line_width", "border_line_join", "border_line_cap", "border_line_dash",
# "border_line_dash_offset", "render_mode")) %>% cat()

#' Add or update the "title" annotation to a Bokeh figure
#' @param fig Figure to modify.
#' @param text The text value to render.
#' @param align Location to align the title text.
#' @param text_color Color to use to render text with - a hex code (with no alpha) or any of the 147 named CSS colors, e.g 'green', 'indigo'.
#' @param text_alpha An alpha value to use to fill text with. Acceptable values are floating point numbers between 0 (transparent) and 1 (opaque).
#' @param text_font_size Font size in px, em, or pt, e.g., '12pt', '1.5em'.
#' @param text_font Name of a font to use for rendering text, e.g., 'times', 'helvetica'.
#' @param text_font_style A style to use for rendering text. One of 'normal' 'italic' 'bold'.
#' @param offset Offset the text by a number of pixels (can be positive or negative). Shifts the text in different directions based on the location of the title. Possible values are "above", "below", "right", "left".
#' @param background_fill_color The fill color values for the text bounding box.
#' @param background_fill_alpha The fill alpha values for the text bounding box.
#' @param border_line_color The line color values for the text bounding box.
#' @param border_line_alpha The line alpha values for the text bounding box.
#' @param border_line_width The line width values for the text bounding box.
#' @param border_line_join The line join values for the text bounding box.
#' @param border_line_cap The line cap values for the text bounding box.
#' @param border_line_dash The line dash values for the text bounding box.
#' @param border_line_dash_offset The line dash offset values for the text bounding box.
#' @param render_mode Specifies whether the text is rendered as a canvas element or as an css element overlaid on the canvas. The default mode is "canvas". Note: The CSS labels won't be present in the output using the "save" tool. Warning: Not all visual styling properties are supported if the render_mode is set to "css". The border_line_dash property isn't fully supported and border_line_dash_offset isn't supported at all. Setting text_alpha will modify the opacity of the entire background box and border in addition to the text. Finally, clipping Label annotations inside of the plot area isn't supported in "css" mode.
#' @family annotation functions
#' @export
#' @examples
#' # note title can also be specified in figure() but more control with ann_title()
#' figure() %>%
#'   ly_points(1:10, 1:10) %>%
#'   ann_title("title example")
#'
#' # more control over title annotation
#' figure(title = "title example") %>%
#'   ly_points(1:10, 1:10) %>%
#'   ann_title(text_font = "Courier", text_font_size = 20)
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
