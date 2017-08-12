#' Customize x axis of a Bokeh figure
#' @export
x_axis <- function(fig, label = NULL, position = "below", log = FALSE, draw = TRUE,
  ticker = NULL, tickformatter = NULL, axis = NULL, grid = NULL) {

  get_nms <- c("label", "ticker", "tickformatter", "axis", "grid", "log")
  args <- get_specified_args(get_nms)

  if (is.null(position))
    position <- "below"
  if (!position %in% c("below", "above")) {
    message("x-axis position must be either below or above - setting to 'below'")
    position <- "below"
  }

  update_axis(fig, position, draw, args, which = "x")
}

#' @export
y_axis <- function(fig, label = NULL, position = "left", log = FALSE, draw = TRUE,
  ticker = NULL, tickformatter = NULL, axis = NULL, grid = NULL) {

  get_nms <- c("label", "ticker", "tickformatter", "axis", "grid", "log")
  args <- get_specified_args(get_nms)

  if (is.null(position))
    position <- "left"
  if (!position %in% c("left", "right")) {
    message("y-axis position must be either 'left' or 'right' - setting to 'left'")
    position <- "left"
  }

  update_axis(fig, position, draw, args, which = "y")
}

update_axis <- function(fig, position, draw, args, which) {
  if ("log" %in% names(args) && args$log == TRUE)
    fig$x$pars$axes$log[[which]] <- TRUE

  if ("label" %in% names(args))
    fig$x$pars$axes$lab[[which]] <- args$label

  axis_type <- fig$x$pars$axes$type[[which]]

  # TODO: move this validation to when axes are built in prepare_figure
  # valid_mods <- list(
  #   numeric = c("BasicTicker", "FixedTicker", "SingleIntervalTicker",
  #     "BasicTickFormatter", "NumeralTickFormatter", "PrintfTickFormatter",
  #     "LinearAxis", "FuncTickFormatter", "LogTicker", "LogTickFormatter",
  #     "LogAxis", "FuncTickFormatter"),
  #   categorical = c("DatetimeTicker", "DatetimeTickFormatter", "DatetimeAxis",
  #     "FuncTickFormatter"),
  #   datetime = c("CategoricalTicker", "CategoricalTickFormatter", "CategoricalAxis",
  #     "FuncTickFormatter")
  # )
  # if (is.null(axis_type)) {
  #   message("Cannot customize axes without data.")
  #   return(fig)
  # }
  # if (!cur_obj$model %in% valid_mods[[axis_type]]) {
  #   message("The model '", cur_obj$model,"' is not compatible with the ", which,
  #     " axis which is of type '", axis_type, "... Ignoring this specification.")

  if (draw) {
    mod_nms <- c("ticker", "tickformatter", "axis", "grid")
    for (nm in intersect(mod_nms, names(args))) {
      cur_obj <- eval(args[[nm]])
      if (cur_obj$clear) {
        fig$x$pars$axes$args[[position]][[nm]] <- cur_obj
      } else {
        fig$x$pars$axes$args[[position]][[nm]] <-
          modifyList(fig$x$pars$axes$args[[position]][[nm]], cur_obj)
      }
      fig$x$pars$axes$args[[position]][[nm]]$clear <- NULL
    }
  } else {
    fig$x$pars$axes$args[[position]] <- NULL
  }

  fig
}


## tickers
##---------------------------------------------------------

#' @export
ticker_num <- function(
  min_interval = 0,
  max_interval = NULL,
  desired_num_ticks = 6,
  num_minor_ticks = 5,
  mantissas = c(1, 2, 5),
  base = 10,
  clear = FALSE
) {
  structure(list(
    desired_num_ticks = desired_num_ticks,
    num_minor_ticks = num_minor_ticks,
    min_interval = min_interval,
    max_interval = max_interval,
    mantissas = mantissas,
    base = base,
    model = "BasicTicker",
    clear = clear
  ),
  class = c("list", "ticker"))
}

ticker_fixed <- function(
  ticks = NULL,
  desired_num_ticks = 6,
  num_minor_ticks = 5,
  clear = FALSE
) {
  structure(list(
    ticks = ticks,
    num_minor_ticks = num_minor_ticks,
    desired_num_ticks = desired_num_ticks,
    model = "FixedTicker",
    clear = clear
  ),
  class = c("list", "ticker"))
}

#' @export
ticker_interval <- function(
  interval = NULL,
  desired_num_ticks = 6,
  num_minor_ticks = 5,
  clear = FALSE
) {
  structure(list(
    interval = interval,
    num_minor_ticks = num_minor_ticks,
    desired_num_ticks = desired_num_ticks,
    model = "SingleIntervalTicker",
    clear = clear
  ),
  class = c("list", "ticker"))
}

#' @export
ticker_log <- function(
  base = 10,
  min_interval = 0,
  max_interval = NULL,
  mantissas = list(1, 5),
  desired_num_ticks = 6,
  num_minor_ticks = 5,
  clear = FALSE
) {
  structure(list(
    base = base,
    min_interval = min_interval,
    max_interval = max_interval,
    mantissas = mantissas,
    desired_num_ticks = desired_num_ticks,
    num_minor_ticks = num_minor_ticks,
    model = "LogTicker",
    clear = clear
  ),
  class = c("list", "ticker"))
}

#' @export
ticker_date <- function(
  desired_num_ticks = 6L,
  num_minor_ticks = 0L,
  clear = FALSE
) {
  structure(list(
    desired_num_ticks = desired_num_ticks,
    num_minor_ticks = num_minor_ticks,
    model = "DatetimeTicker",
    clear = clear
  ),
  class = c("list", "ticker"))
}

#' @export
ticker_cat <- function(clear = FALSE) {
  structure(list(
    model = "CategoricalTicker",
    clear = clear
  ),
  class = c("list", "ticker"))
}

# MercatorTicker

## tickformatters
##---------------------------------------------------------

#' @export
tickformatter_num <- function(
  use_scientific = TRUE,
  power_limit_low = -3,
  power_limit_high = 5,
  precision = "auto",
  clear = FALSE
) {
  structure(list(
    use_scientific = use_scientific,
    power_limit_low = power_limit_low,
    power_limit_high = power_limit_high,
    precision = precision,
    model = "BasicTickFormatter",
    clear = clear
  ),
  class = c("list", "tickformatter"))
}

#' @export
tickformatter_func <- function(
  code = "",
  args = NULL,
  clear = FALSE
) {
  structure(list(
    code = code,
    args = args,
    model = "FuncTickFormatter",
    clear = clear
  ),
  class = c("list", "tickformatter"))
}

#' @export
tickformatter_numeral <- function(
  format = "0,0",
  rounding = "round",
  language = "en",
  clear = FALSE
) {
  structure(list(
    format = format,
    rounding = rounding,
    language = language,
    model = "NumeralTickFormatter",
    clear = clear
  ),
  class = c("list", "tickformatter"))
}

#' @export
tickformatter_printf <- function(format = "%s", clear = FALSE) {
  structure(list(
    format = format,
    model = "PrintfTickFormatter",
    clear = clear
  ),
  class = c("list", "tickformatter"))
}

#' @export
tickformatter_log <- function(clear = FALSE) {
  structure(list(
    model = "LogTickFormatter",
    clear = clear
  ),
  class = c("list", "tickformatter"))
}

#' @export
tickformatter_date <- function(
  years = c("%Y"),
  months = c("%m/%Y", "%b%y"),
  days = c("%m/%d", "%a%d"),
  hours = c("%Hh", "%H:%M"),
  hourmin = c("%H:%M"),
  minutes = c(":%M", "%Mm"),
  minsec = c(":%M:%S"),
  seconds = c("%Ss"),
  milliseconds = c("%3Nms", "%S.%3Ns"),
  microseconds = c("%fus"),
  clear = FALSE
) {
  structure(list(
    years = years,
    months = months,
    days = days,
    hours = hours,
    hourmin = hourmin,
    minutes = minutes,
    minsec = minsec,
    seconds = seconds,
    milliseconds = milliseconds,
    microseconds = microseconds,
    model = "DatetimeTickFormatter",
    clear = clear
  ),
  class = c("list", "tickformatter"))
}

#' @export
tickformatter_cat <- function(clear = FALSE) {
  structure(list(
    model = "CategoricalTickFormatter",
    clear = clear
  ),
  class = c("list", "tickformatter"))
}

## axis and grid spec
##---------------------------------------------------------

#' @export
axis_spec <- function(
  visible = TRUE,
  major_label_overrides = NULL,
  bounds = "auto",
  level = "overlay",
  # the rest of this is theme stuff...
  axis_label_standoff = NULL,
  axis_label_text_align = NULL,
  axis_label_text_alpha = NULL,
  axis_label_text_baseline = NULL,
  axis_label_text_color = NULL,
  axis_label_text_font = NULL,
  axis_label_text_font_size = NULL,
  axis_label_text_font_style = NULL,
  major_label_orientation = NULL,
  major_label_standoff = NULL,
  major_label_text_align = NULL,
  major_label_text_alpha = NULL,
  major_label_text_baseline = NULL,
  major_label_text_color = NULL,
  major_label_text_font = NULL,
  major_label_text_font_size = NULL,
  major_label_text_font_style = NULL,
  axis_line_alpha = NULL,
  axis_line_cap = NULL,
  axis_line_color = NULL,
  axis_line_dash = NULL,
  axis_line_dash_offset = NULL,
  axis_line_join = NULL,
  axis_line_width = NULL,
  major_tick_in = NULL,
  major_tick_line_alpha = NULL,
  major_tick_line_cap = NULL,
  major_tick_line_color = NULL,
  major_tick_line_dash = NULL,
  major_tick_line_dash_offset = NULL,
  major_tick_line_join = NULL,
  major_tick_line_width = NULL,
  major_tick_out = NULL,
  minor_tick_in = NULL,
  minor_tick_line_alpha = NULL,
  minor_tick_line_cap = NULL,
  minor_tick_line_color = NULL,
  minor_tick_line_dash = NULL,
  minor_tick_line_dash_offset = NULL,
  minor_tick_line_join = NULL,
  minor_tick_line_width = NULL,
  minor_tick_out = NULL,
  clear = FALSE
) {
  res <- get_specified_args()
  res$clear <- clear
  class(res) <- c("list", "axis")
  res
}

#' @export
grid_spec <- function(
  visible = TRUE,
  level = "underlay",
  bounds = "auto",
  # the rest of this is theme stuff...
  band_fill_alpha = NULL,
  band_fill_color = NULL,
  grid_line_alpha = NULL,
  grid_line_cap = NULL,
  grid_line_color = NULL,
  grid_line_dash = NULL,
  grid_line_dash_offset = NULL,
  grid_line_join = NULL,
  grid_line_width = NULL,
  minor_grid_line_alpha = NULL,
  minor_grid_line_cap = NULL,
  minor_grid_line_color = NULL,
  minor_grid_line_dash = NULL,
  minor_grid_line_dash_offset = NULL,
  minor_grid_line_join = NULL,
  minor_grid_line_width = NULL,
  clear = FALSE
) {
  res <- get_specified_args()
  res$clear <- clear
  class(res) <- c("list", "axis_grid")
  res
}
