#' @export
spec <- function(x, domain = NULL, range = NULL, transform = NULL, units = "screen", name = NULL,
  exponent = 0.5, log = FALSE, unknown = "lightgray") {

  # TODO: validate other arguments...

  if (is.character(transform))
    transform <- custom_js_transform(transform)

  if (!is.null(transform) && !inherits(transform, "custom_js_transform"))
    stop("The 'transform' argument specified in spec() is invalid.", call. = FALSE)

  attr(x, "spec") <- list(
    domain = domain,
    range = range,
    transform = transform,
    units = units,
    exponent = exponent,
    log = log
  )
  x
}

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
