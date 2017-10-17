#' Provide additional specification information about a Bokeh plot attribute.
#' @param x Data / variable / expression to add specification information to.
#' @param domain The domain of \code{x}. If \code{x} is categorical, this is the unique values or levels of the factor. If \code{x} is numeric, this is the desired range of the data to map from. If not supplied, this is inferred directly from \code{x}.
#' @param range The range of the mapping to be made from \code{x} to the associated plot attribute. For example, if \code{x} is being mapped to a color attribute and \code{x} is categorical, then \code{range} can be specified as a vector of colors to map the unique values of \code{x} to. See examples below.
#' @param transform An optional string specifying a JavaScript transform to be applied to the data in the browser, the result of which will be the attribute value(s) used in rendering. See examples below.
#' @param units The units according which to treat the attribute. One of 'screen' or 'data'. If 'screen', the value of \code{x} will specify the attribute in terms of pixels on the screen. If 'data', the value of \code{x} will specify the attribute in terms of the scale of the data on the plot. This is only valid in the case of a numeric \code{x} applying to a plot attribute such as \code{size}, and is not applicable to all glyphs. It will be ignored if not applicable.
# @param name
#' @param exponent When \code{x} is being mapped to a \code{size} attribute, the exponent to use in the transformation.
# @param log
#' @param unknown The color to use for values of \code{x} that cannot be mapped to the specified range.
#' @export
#' @examples
#' # no custom spec
#' figure() %>%
#'   ly_points(x = Sepal.Width, y = Sepal.Length,
#'     color = spec(Species, range = c("red", "green", "blue")),
#'   data = iris)
#'
#' # specifying a custom mapping for color
#' figure() %>%
#'   ly_points(x = Sepal.Width, y = Sepal.Length,
#'     color = spec(Species, range = c("red", "green", "blue")),
#'   data = iris)
#'
#' # explicitly specifying which values correspond to which color
#' figure() %>%
#'   ly_points(x = Sepal.Width, y = Sepal.Length,
#'     color = spec(Species, domain = c("virginica", "versicolor", "setosa"),
#'       range = c("red", "blue", "green")),
#'   data = iris)
#'
#' # unmapped values are plotted in gray (with default theme)
#' iris$Species2 <- as.character(iris$Species)
#' iris$Species2[1:20] <- "unknown"
#' figure() %>%
#'   ly_points(x = Sepal.Width, y = Sepal.Length,
#'     color = spec(Species2, domain = c("virginica", "versicolor", "setosa"),
#'       range = c("red", "blue", "green")),
#'   data = iris)
#'
#' # specifying "screen" vs. "data" units
#' figure() %>%
#'   ly_oval(Sepal.Length, Sepal.Width,
#'     width = spec(15, units = "screen"),
#'     height = spec(30, units = "screen"),
#'     angle = pi / 4,
#'     data = iris, color = Species)
#'
#' # example using custom JS transforms for every specified plot attribute
#' # random size, line width, and color
#' # note that JS transforms are automatically vectorized if provided as a string
#' figure() %>%
#'   ly_points(rnorm(100), rnorm(100),
#'     size = spec(1:100, transform = "return Math.random() * 50 + 10;"),
#'     line_width = spec(1:100, transform = "return Math.random() * 10"),
#'     color = spec(1:100, transform = "
#'       var rnd = function() { return Math.floor(Math.random() * 255); }
#'       return '#' + (rnd()).toString(16) + (rnd()).toString(16) + (rnd()).toString(16);
#'     "))
#'
#' # using spec to specify size of points
#' # requires google API key
#' \dontrun{
#' gmap(lat = 40.44, lng = -113.785, zoom = 4,
#'   width = 1000, height = 700,
#'   map_style = gmap_style("blue_water"), map_type = "roadmap") %>%
#'   ly_points(long, lat, data = us.cities, color = factor(capital),
#'     size = spec(pop, range = c(2, 50)), hover = us.cities)
#' }
spec <- function(x,
  domain = NULL, range = NULL,
  transform = NULL, units = "screen",
  # name = NULL,
  exponent = 0.5,
  # log = FALSE,
  unknown = "lightgray") {

  # TODO: validate other arguments...

  spc <- list()
  if (!missing(domain))
    spc$domain <- domain

  if (!missing(range))
    spc$range <- range

  if (!missing(transform)) {
    if (is.character(transform))
      transform <- custom_js_transform(transform)

    if (!is.null(transform) && !inherits(transform, "custom_js_transform"))
      stop("The 'transform' argument specified in spec() is invalid.", call. = FALSE)

    spc$transform <- transform
  }

  if (!missing(units))
    spc$units <- units

  # if (!missing(name))
  #   spc$name <- name

  if (!missing(exponent))
    spc$exponent <- exponent

  # if (!missing(log))
  #   spc$log <- log

  if (!missing(unknown))
    spc$unknown <- unknown

  attr(x, "spec") <- spc

  x
}

#' Specify a custom JavaScript transformation.
#' @param func A string of JavaScript code to be applied to one value of an array.
#' @param v_func An optional string of JavaScript code to be applied to an entire array. If not specified, the array will be looped over and \code{func} will be applied to each element.
#' @param global A string of JavaScript code indicating global values to be specified prior to the code for \code{func} or \code{v_func}.
#' @export
custom_js_transform <- function(func, v_func = NULL, global = NULL) {
  if (!is.character(func))
    stop("Argument 'func' to custom_js_transform must be a string.", call. = FALSE)

  if (is.null(v_func)) {
    v_func <- glue::glue("
      var fn = function(x) {{
        {func}
      }}
      return RBK.vectorize(fn, xs)")
  }

  if (!is.character(v_func))
    stop("Argument 'v_func' to custom_js_transform must be a string.", call. = FALSE)

  if (!is.null(global)) {
    func <- glue::glue("
      {global}
      {func}")
    v_func <- glue::glue("
      {global}
      {v_func}")
  }

  res <- list(func = func, v_func = v_func)
  class(res) <- c("custom_js_transform", "list")

  res
}


# spec(x, mapping = [list or custom js], units = "screen"|"data")

# asis

# fill: list(color, alpha)
# line: list(color, alpha, width)

# or:

# color: list(fill, line)
# alpha: list(fill, line)

# x: NumberSpec
# y: NumberSpec
# color:
# alpha:
# size: ScreenDistanceSpec
# hov_color:
# hov_alpha:
# ns_color:
# ns_alpha:

# line_width: NumberSpec
# line_alpha: NumberSpec
# line_color: ColorSpec
# fill_alpha: NumberSpec
# fill_color: ColorSpec
# angle: AngleSpec

# hov_line_width: NumberSpec
# hov_line_alpha: NumberSpec
# hov_line_color: ColorSpec
# hov_fill_alpha: NumberSpec
# hov_fill_color: ColorSpec
# hov_size: ScreenDistanceSpec
# hov_angle: AngleSpec

# ns_line_width: NumberSpec
# ns_line_alpha: NumberSpec
# ns_line_color: ColorSpec
# ns_fill_alpha: NumberSpec
# ns_fill_color: ColorSpec
# ns_size: ScreenDistanceSpec
# ns_angle: AngleSpec

# line_join: Enum('miter', 'round', 'bevel')
# line_cap: Enum('butt', 'round', 'square')
# line_dash_offset: Int
# angle_units: Enum('deg', 'rad')
# line_dash: DashPattern
