#' Customize x or y axis of a Bokeh figure.
#' @param fig Figure to modify.
#' @param label A character string indicating the axis label.
#' @param position The position of the axis. Either "below" or "above" for an x axis and "left" or "right" for a y axis. Note that multiple x and y axes can be coexist in a figure.
#' @param log A logical indicating whether the axis should be on the log scale.
#' @param draw A logical indicating whether the axis be drawn. This is useful when you want to disable an axis.
#' @param ticker A ticker specificition (see "See Also" below).
#' @param tickformatter  A tickformatter specificition (see "See Also" below).
#' @param axis  An axis specificition (see "See Also" below).
#' @param grid  A grid specificition (see "See Also" below).
#' @seealso \code{\link{ticker_num}}, \code{\link{ticker_fixed}}, \code{\link{ticker_interval}}, \code{\link{ticker_log}}, \code{\link{ticker_date}}, \code{\link{ticker_cat}}, \code{\link{tickformatter_num}}, \code{\link{tickformatter_func}}, \code{\link{tickformatter_numeral}}, \code{\link{tickformatter_printf}}, \code{\link{tickformatter_log}}, \code{\link{tickformatter_date}}, \code{\link{tickformatter_cat}}, \code{\link{axis_spec}}, \code{\link{grid_spec}},
#' @param x,y numeric vectors.
#' @example man-roxygen/ex-axis.R
#' @name axis
NULL

#' @rdname axis
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

#' @rdname axis
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

# internal
update_axis <- function(fig, position, draw, args, which) {
  if ("log" %in% names(args) && args$log == TRUE)
    fig$x$pars$axes[[position]]$log <- TRUE

  if ("label" %in% names(args))
    fig$x$gen$labs[[which]] <- args$label

  # axis_type <- fig$x$pars$ranges[[which]]$type

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

  fig$x$pars$axes[[position]]$draw <- draw

  # if (draw) {
  mod_nms <- c("ticker", "tickformatter", "axis", "grid")
  for (nm in intersect(mod_nms, names(args))) {
    cur_obj <- args[[nm]]
    if (cur_obj$clear) {
      fig$x$pars$axes[[position]]$args[[nm]] <- cur_obj
    } else {
      fig$x$pars$axes[[position]]$args[[nm]] <-
        modifyList(fig$x$pars$axes[[position]]$args[[nm]], cur_obj)
    }
    fig$x$pars$axes[[position]]$args[[nm]]$clear <- NULL
  }
  # } else {
  #   fig$x$pars$axes[[position]]$args <- NULL
  # }

  fig
}

## tickers
##---------------------------------------------------------

# get_docs(mods, "BasicTicker",
#   c("min_interval", "max_interval", "desired_num_ticks",
#     "num_minor_ticks", "mantissas", "base")) %>% cat()

#' Generate ticks on a linear / numeric scale.
#' @param min_interval The smallest allowable interval between two adjacent ticks.
#' @param max_interval The largest allowable interval between two adjacent ticks. Note: To specify an unbounded interval, set to \code{NULL}.
#' @param desired_num_ticks A desired target number of major tick positions to generate across the plot range. Note: This value is a suggestion, and ticker subclasses may ignore it entirely, or use it only as an ideal goal to approach as well as can be, in the context of a specific ticking strategy.
#' @param num_minor_ticks The number of minor tick positions to generate between adjacent major tick values.
#' @param mantissas The acceptable list numbers to generate multiples of.
#' @param base The multiplier to use for scaling mantissas.
#' @param clear A logical indicating whether all non-specified parameters should be cleared out of any exising specification in the figure. This can be useful when modifying a previously-defined figure.
#' @return A specification that is used as the \code{ticker} argument for \code{\link{x_axis}} or \code{\link{y_axis}}.
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

# get_docs(mods, "FixedTicker",
#   c("ticks", "desired_num_ticks", "num_minor_ticks")) %>% cat()

#' Generate ticks at fixed, explicitly supplied locations.
#' @param ticks List of tick locations.
#' @param num_minor_ticks The number of minor tick positions to generate between adjacent major tick values.
#' @param clear A logical indicating whether all non-specified parameters should be cleared out of any exising specification in the figure. This can be useful when modifying a previously-defined figure.
#' @examples
#' figure() %>%
#'   ly_points(1:10) %>%
#'   x_axis(ticker = ticker_fixed(c(1, 7, 9)))
#' @export
ticker_fixed <- function(
  ticks = NULL,
  num_minor_ticks = 5,
  clear = FALSE
) {
  structure(list(
    ticks = ticks,
    num_minor_ticks = num_minor_ticks,
    model = "FixedTicker",
    clear = clear
  ),
  class = c("list", "ticker"))
}

# get_docs(mods, "SingleIntervalTicker",
#   c("interval", "desired_num_ticks", "num_minor_ticks")) %>% cat()

#' Generate evenly spaced ticks at a fixed interval regardless of scale.
#' @param interval The interval between adjacent ticks.
#' @param desired_num_ticks A desired target number of major tick positions to generate across the plot range. Note: This value is a suggestion, and ticker subclasses may ignore it entirely, or use it only as an ideal goal to approach as well as can be, in the context of a specific ticking strategy.
#' @param num_minor_ticks The number of minor tick positions to generate between adjacent major tick values.
#' @param clear A logical indicating whether all non-specified parameters should be cleared out of any exising specification in the figure. This can be useful when modifying a previously-defined figure.
#' @return A specification that is used as the \code{ticker} argument for \code{\link{x_axis}} or \code{\link{y_axis}}.
#' @examples
#' figure() %>%
#'   ly_points(1:10) %>%
#'   x_axis(ticker = ticker_interval(0.5))
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

# get_docs(mods, "LogTicker",
  # c("base", "min_interval", "max_interval", "mantissas",
  #   "desired_num_ticks", "num_minor_ticks")) %>% cat()

#' Generate ticks on a log scale.
#' @param base The multiplier to use for scaling mantissas.
#' @param min_interval The smallest allowable interval between two adjacent ticks.
#' @param max_interval The largest allowable interval between two adjacent ticks. Note: To specify an unbounded interval, set to \code{NULL}.
#' @param mantissas The acceptable list numbers to generate multiples of.
#' @param desired_num_ticks A desired target number of major tick positions to generate across the plot range. Note: This value is a suggestion, and ticker subclasses may ignore it entirely, or use it only as an ideal goal to approach as well as can be, in the context of a specific ticking strategy.
#' @param num_minor_ticks The number of minor tick positions to generate between adjacent major tick values.
#' @param clear A logical indicating whether all non-specified parameters should be cleared out of any exising specification in the figure. This can be useful when modifying a previously-defined figure.
#' @return A specification that is used as the \code{ticker} argument for \code{\link{x_axis}} or \code{\link{y_axis}}.
#' @examples
#' figure() %>%
#'   ly_points(rexp(100, rate = 1 / 100), rnorm(100)) %>%
#'   x_axis(ticker = ticker_log(base = 2))
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

# get_docs(mods, "DatetimeTicker",
#   c("desired_num_ticks", "num_minor_ticks")) %>% cat()

#' Generate nice ticks across different date and time scales.
#' @param desired_num_ticks A desired target number of major tick positions to generate across the plot range. Note: This value is a suggestion, and ticker subclasses may ignore it entirely, or use it only as an ideal goal to approach as well as can be, in the context of a specific ticking strategy.
#' @param num_minor_ticks The number of minor tick positions to generate between adjacent major tick values.
#' @param clear A logical indicating whether all non-specified parameters should be cleared out of any exising specification in the figure. This can be useful when modifying a previously-defined figure.
#' @return A specification that is used as the \code{ticker} argument for \code{\link{x_axis}} or \code{\link{y_axis}}.
#' @examples
#' figure(data = d, width = 1000) %>%
#'   ly_lines(date, co2_df) %>%
#'   x_axis(ticker = ticker_date(desired_num_ticks = 20))
#'
#' figure(data = d, width = 1000) %>%
#'   ly_lines(date, co2_df) %>%
#'   x_axis(tickformatter = tickformatter_date(year = "'%y"))
#'
#' figure() %>%
#'   ly_points(Sys.time() - c(0:9) * 900, rnorm(10)) %>%
#'   x_axis(tickformatter = tickformatter_date(hourmin = "%r %Z"))
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

# get_docs(mods, "CategoricalTicker") %>% cat()

#' Generate ticks for categorical ranges.
#' @param clear A logical indicating whether all non-specified parameters should be cleared out of any exising specification in the figure. This can be useful when modifying a previously-defined figure.
#' @note This function should never need to be used as there are no parameters for a categorical ticker.
#' @return A specification that is used as the \code{ticker} argument for \code{\link{x_axis}} or \code{\link{y_axis}}.
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


# get_docs(mods, "BasicTickFormatter",
#   c("use_scientific", "power_limit_low", "power_limit_high", "precision")) %>% cat()

#' Display tick values from continuous ranges as "basic numbers", using scientific notation when appropriate by default.
#' @param use_scientific Whether to ever display scientific notation. If \code{TRUE}, then when to use scientific notation is controlled by "power_limit_low" and "power_limit_high".
#' @param power_limit_low Limit the use of scientific notation to when: log(x) <= power_limit_low
#' @param power_limit_high Limit the use of scientific notation to when: log(x) >= power_limit_high
#' @param precision How many digits of precision to display in tick labels.
#' @param clear A logical indicating whether all non-specified parameters should be cleared out of any exising specification in the figure. This can be useful when modifying a previously-defined figure.
#' @return A specification that is used as the \code{tickformatter} argument for \code{\link{x_axis}} or \code{\link{y_axis}}.
#' @examples
#' figure() %>%
#'   ly_points(c(1:10) * 1000000, 1:10) %>%
#'   x_axis(tickformatter = tickformatter_num(use_scientific = FALSE))
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

# get_docs(mods, "FuncTickFormatter",
#   c("code", "args")) %>% cat()

#' Display tick values that are formatted by a user-defined function.
#' @param code A snippet of JavaScript code that reformats a single tick to the desired format. The variable "tick" will contain the unformatted tick value and can be expected to be present in the code snippet namespace at render time.
#' @param args A mapping of names to Bokeh plot objects. These objects are made available to the formatter code snippet as the values of named parameters to the callback.
#' @param clear A logical indicating whether all non-specified parameters should be cleared out of any exising specification in the figure. This can be useful when modifying a previously-defined figure.
#' @return A specification that is used as the \code{tickformatter} argument for \code{\link{x_axis}} or \code{\link{y_axis}}.
#' @examples
#' figure() %>%
#'   ly_points(1:26, letters) %>%
#'   x_axis(
#'     ticker = ticker_num(desired_num_ticks = 20),
#'     tickformatter = tickformatter_func(code = "return tick + 'm';"))
#'
#' figure(data = d, width = 1000) %>%
#'   ly_lines(date, co2_df) %>%
#'   x_axis(tickformatter = tickformatter_func("
#' var cur = new Date();
#' var diff = (cur.getTime() - tick) / (1000 * 60 * 60 * 24 * 365);
#' return diff.toFixed(2) + ' years ago'"))
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

# get_docs(mods, "NumeralTickFormatter",
#   c("format", "rounding", "language")) %>% cat()
# cat(mods$NumeralTickFormatter$props[[8]]$desc)

#' Tick formatter based on a human-readable format string.
#' @param format The number format, as defined in the tables described in the details. For the complete specification, see \url{http://numbrojs.com/format.html}.
#' @param rounding Rounding functions (round, floor, ceil) and their synonyms (nearest, rounddown, roundup).
#' @param language The language to use for formatting language-specific features (e.g. thousands separator).
#' @param clear A logical indicating whether all non-specified parameters should be cleared out of any exising specification in the figure. This can be useful when modifying a previously-defined figure.
#' @details
#' Possible values for \code{format} are defined in the following tables:
#'
#' \strong{NUMBERS}
#'
#' \tabular{llr}{
#' \strong{Number}    \tab  \strong{Format}      \tab  \strong{String}\cr
#' \code{10000}       \tab  \code{'0,0.0000'}    \tab  \code{10,000.0000}\cr
#' \code{10000.23}    \tab  \code{'0,0'}         \tab  \code{10,000}\cr
#' \code{10000.23}    \tab  \code{'+0,0'}        \tab  \code{+10,000}\cr
#' \code{-10000}      \tab  \code{'0,0.0'}       \tab  \code{-10,000.0}\cr
#' \code{10000.1234}  \tab  \code{'0.000'}       \tab  \code{10000.123}\cr
#' \code{10000.1234}  \tab  \code{'0[.]00000'}   \tab  \code{10000.12340}\cr
#' \code{-10000}      \tab  \code{'(0,0.0000)'}  \tab  \code{(10,000.0000)}\cr
#' \code{-0.23}       \tab  \code{'.00'}         \tab  \code{-.23}\cr
#' \code{-0.23}       \tab  \code{'(.00)'}       \tab  \code{(.23)}\cr
#' \code{0.23}        \tab  \code{'0.00000'}     \tab  \code{0.23000}\cr
#' \code{0.23}        \tab  \code{'0.0[0000]'}   \tab  \code{0.23}\cr
#' \code{1230974}     \tab  \code{'0.0a'}        \tab  \code{1.2m}\cr
#' \code{1460}        \tab  \code{'0 a'}         \tab  \code{1 k}\cr
#' \code{-104000}     \tab  \code{'0a'}          \tab  \code{-104k}\cr
#' \code{1}           \tab  \code{'0o'}          \tab  \code{1st}\cr
#' \code{52}          \tab  \code{'0o'}          \tab  \code{52nd}\cr
#' \code{23}          \tab  \code{'0o'}          \tab  \code{23rd}\cr
#' \code{100}         \tab  \code{'0o'}          \tab  \code{100th}
#' }
#'
#' \strong{CURRENCY}
#'
#' \tabular{llr}{
#' \strong{Number}   \tab  \strong{Format}      \tab   \strong{String}\cr
#' \code{1000.234}   \tab  \code{'$0,0.00'}     \tab  \code{ $1,000.23}\cr
#' \code{1000.2}     \tab  \code{'0,0[.]00 $'}  \tab  \code{ 1,000.20 $}\cr
#' \code{1001}       \tab  \code{'$ 0,0[.]00'}  \tab  \code{ $ 1,001}\cr
#' \code{-1000.234}  \tab  \code{'($0,0)'}      \tab  \code{ ($1,000)}\cr
#' \code{-1000.234}  \tab  \code{'$0.00'}       \tab  \code{ -$1000.23}\cr
#' \code{1230974}    \tab  \code{'($ 0.00 a)'}  \tab  \code{ $ 1.23 m}
#' }
#'
#' \strong{BYTES}
#'
#' \tabular{llr}{
#' \strong{Number}       \tab \strong{Format}   \tab \strong{String}\cr
#' \code{100}            \tab \code{'0b'}       \tab \code{100B}\cr
#' \code{2048}           \tab \code{'0 b'}      \tab \code{2 KB}\cr
#' \code{7884486213}     \tab \code{'0.0b'}     \tab \code{7.3GB}\cr
#' \code{3467479682787}  \tab \code{'0.000 b'}  \tab \code{3.154 TB}
#' }
#'
#' \strong{PERCENTAGES}
#'
#' \tabular{llr}{
#' \strong{Number}     \tab  \strong{Format}      \tab  \strong{String}\cr
#' \code{1}            \tab  \code{'0\%'}         \tab  \code{100\%}\cr
#' \code{0.974878234}  \tab  \code{'0.000\%'}     \tab  \code{97.488\%}\cr
#' \code{-0.43}        \tab  \code{'0 \%'}        \tab  \code{-43 \%}\cr
#' \code{0.43}         \tab  \code{'(0.000 \%)'}  \tab  \code{43.000 \%}
#' }
#'
#' \strong{TIME}
#'
#' \tabular{llr}{
#' \strong{Number}  \tab \strong{Format}    \tab   \strong{String}\cr
#' \code{25}        \tab \code{'00:00:00'}  \tab   \code{0:00:25}\cr
#' \code{238}       \tab \code{'00:00:00'}  \tab   \code{0:03:58}\cr
#' \code{63846}     \tab \code{'00:00:00'}  \tab   \code{17:44:06}
#' }
#' @return A specification that is used as the \code{tickformatter} argument for \code{\link{x_axis}} or \code{\link{y_axis}}.
#' @examples
#' figure() %>%
#'   ly_points(1:10) %>%
#'   x_axis(tickformatter = tickformatter_numeral(format = "0o"))
#'
#' figure() %>%
#'   ly_points((1:10) ^ 4, 1:10) %>%
#'   x_axis(tickformatter = tickformatter_numeral(format = "($0,0)", language = "it"))
#'
#' figure() %>%
#'   ly_points((1:11) ^ 6, 1:11) %>%
#'   x_axis(tickformatter = tickformatter_numeral(format = "0.0b"))
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


# get_docs(mods, "PrintfTickFormatter",
#   c("format")) %>% cat()

#' Tick formatter based on a printf-style format string.
#' @param format The number format, as defined in the details below.
#' @param clear A logical indicating whether all non-specified parameters should be cleared out of any exising specification in the figure. This can be useful when modifying a previously-defined figure.
#' @details The placeholder in the format string is marked by % and is followed by one or more of these elements, in this order:
#' \itemize{
#'   \item An optional "+" sign Causes the result to be preceded with a plus or minus sign on numeric values. By default, only the "-" sign is used on negative numbers.
#'   \item An optional padding specifier Specifies what (if any) character to use for padding. Possible values are 0 or any other character preceded by a "'" (single quote). The default is to pad with spaces.
#'   \item An optional "-" sign Causes sprintf to left-align the result of this placeholder. The default is to right-align the result.
#'   \item An optional number Specifies how many characters the result should have. If the value to be returned is shorter than this number, the result will be padded.
#'   \item An optional precision modifier Consists of a "." (dot) followed by a number, specifies how many digits should be displayed for floating point numbers. When used on a string, it causes the result to be truncated.
#'   \item A type specifier Can be any of:
#'   \itemize{
#'     \item "\%" --- yields a literal "\%" character
#'     \item "b" --- yields an integer as a binary number
#'     \item "c" --- yields an integer as the character with that ASCII value
#'     \item "d" or "i" --- yields an integer as a signed decimal number
#'     \item "e" --- yields a float using scientific notation
#'     \item "u" --- yields an integer as an unsigned decimal number
#'     \item "f" --- yields a float as is
#'     \item "o" --- yields an integer as an octal number
#'     \item "s" --- yields a string as is
#'     \item "x" --- yields an integer as a hexadecimal number (lower-case)
#'     \item "X" --- yields an integer as a hexadecimal number (upper-case
#'   }
#' }
#' @return A specification that is used as the \code{tickformatter} argument for \code{\link{x_axis}} or \code{\link{y_axis}}.
#' @examples
#' figure() %>%
#'   ly_points(1:10) %>%
#'   x_axis(tickformatter = tickformatter_printf(format = "%5.3f mu"))
#' @export
tickformatter_printf <- function(format = "%s", clear = FALSE) {
  structure(list(
    format = format,
    model = "PrintfTickFormatter",
    clear = clear
  ),
  class = c("list", "tickformatter"))
}


# get_docs(mods, "LogTickFormatter") %>% cat()

#' Display tick values from continuous ranges as powers of some base. Most often useful in conjunction with a "LogTicker".
#' @param clear A logical indicating whether all non-specified parameters should be cleared out of any exising specification in the figure. This can be useful when modifying a previously-defined figure.
#' @return A specification that is used as the \code{tickformatter} argument for \code{\link{x_axis}} or \code{\link{y_axis}}.
#' @note This function should never need to be used as there are no parameters for a log tickformatter.
#' @export
tickformatter_log <- function(clear = FALSE) {
  structure(list(
    model = "LogTickFormatter",
    clear = clear
  ),
  class = c("list", "tickformatter"))
}

# get_docs(mods, "DatetimeTickFormatter",
#   c("years", "months", "days", "hours", "hourmin", "minutes", "minsec",
#     "seconds", "milliseconds", "microseconds")) %>% cat()
# cat(mods$DatetimeTickFormatter$desc)


#' A tick formatter for displaying datetime values nicely across a range of scales.
#' @param years Formats for displaying datetime values in the "years" range. See details.
#' @param months Formats for displaying datetime values in the "months" range. See details.
#' @param days Formats for displaying datetime values in the "days" range. See details.
#' @param hours Formats for displaying datetime values in the "hours" range. See details.
#' @param hourmin Formats for displaying datetime values in the "hourmin" (for combined hours and  minutes) range. See details.
#' @param minutes Formats for displaying datetime values in the "minutes" range. See details.
#' @param minsec Formats for displaying datetime values in the "minsec" (for combined minutes and  seconds) range. See details.
#' @param seconds Formats for displaying datetime values in the "seconds" range. See details.
#' @param milliseconds Formats for displaying datetime values in the "milliseconds" range. See details.
#' @param microseconds Formats for displaying datetime values in the "microseconds" range. See details.
#' @param clear A logical indicating whether all non-specified parameters should be cleared out of any exising specification in the figure. This can be useful when modifying a previously-defined figure.
#' @return A specification that is used as the \code{tickformatter} argument for \code{\link{x_axis}} or \code{\link{y_axis}}.
#' @details
#' This tick formatter has the following properties (listed together with their default values) that can be used to control the formatting of axis ticks at different scales scales:
#' \itemize{
#'   \item microseconds --- \code{['\%fus']}
#'   \item milliseconds --- \code{['\%3Nms', '\%S.\%3Ns']}
#'   \item seconds --- \code{['\%Ss']}
#'   \item minsec --- \code{[':\%M:\%S']}
#'   \item minutes --- \code{[':\%M', '\%Mm']}
#'   \item hourmin --- \code{['\%H:\%M']}
#'   \item hours --- \code{['\%Hh', '\%H:\%M']}
#'   \item days --- \code{['\%m/\%d', '\%a\%d']}
#'   \item months --- \code{['\%m/\%Y', '\%b\%y']}
#'   \item years --- \code{['\%Y']}
#' }
#'
#' Each scale property can be set to format or list of formats to use for formatting datetime tick values that fall in that "time scale". By default, only the first format string passed for each time scale will be used. By default, all leading zeros are stripped away from the formatted labels.
#'
#' This list of supported \href{http://man7.org/linux/man-pages/man3/strftime.3.html}{strftime} formats is reproduced below.
#'
#' \itemize{
#'   \item \code{\%a} --- The abbreviated name of the day of the week according to the current locale.
#'   \item \code{\%A} --- The full name of the day of the week according to the current locale.
#'   \item \code{\%b} --- The abbreviated month name according to the current locale.
#'   \item \code{\%B} --- The full month name according to the current locale.
#'   \item \code{\%c} --- The preferred date and time representation for the current locale.
#'   \item \code{\%C} --- The century number (year/100) as a 2-digit integer.
#'   \item \code{\%d} --- The day of the month as a decimal number (range 01 to 31).
#'   \item \code{\%D} --- Equivalent to \%m/\%d/\%y.  (Americans should note that in many other countries \%d/\%m/\%y is rather common. This means that in international context this format is ambiguous and should not be used.)
#'   \item \code{\%e} --- Like \%d, the day of the month as a decimal number, but a leading zero is replaced by a space.
#'   \item \code{\%f} --- Microsecond as a decimal number, zero-padded on the left (range 000000-999999). This is an extension to the set of directives available to `timezone`_.
#'   \item \code{\%F} --- Equivalent to \%Y-\%m-\%d (the ISO 8601 date format).
#'   \item \code{\%G} --- The ISO 8601 week-based year with century as a decimal number. The 4-digit year corresponding to the ISO week number (see \%V). This has the same format and value as \%Y, except that if the ISO week number belongs to the previous or next year, that year is used instead.
#'   \item \code{\%g} --- Like \%G, but without century, that is, with a 2-digit year (00-99).
#'   \item \code{\%h} --- Equivalent to \%b.
#'   \item \code{\%H} --- The hour as a decimal number using a 24-hour clock (range 00 to 23).
#'   \item \code{\%I} --- The hour as a decimal number using a 12-hour clock (range 01 to 12).
#'   \item \code{\%j} --- The day of the year as a decimal number (range 001 to 366).
#'   \item \code{\%k} --- The hour (24-hour clock) as a decimal number (range 0 to 23). Single digits are preceded by a blank.  (See also \%H.)
#'   \item \code{\%l} --- The hour (12-hour clock) as a decimal number (range 1 to 12). Single digits are preceded by a blank.  (See also \%I.)  (TZ)
#'   \item \code{\%m} --- The month as a decimal number (range 01 to 12).
#'   \item \code{\%M} --- The minute as a decimal number (range 00 to 59).
#'   \item \code{\%n} --- A newline character. Bokeh text does not currently support newline characters.
#'   \item \code{\%N} --- Nanosecond as a decimal number, zero-padded on the left (range 000000000-999999999). Supports a padding width specifier, i.e. \%3N displays 3 leftmost digits. However, this is only accurate to the millisecond level of precision due to limitations of `timezone`_.
#'   \item \code{\%p} --- Either "AM" or "PM" according to the given time value, or the corresponding strings for the current locale.  Noon is treated as "PM" and midnight as "AM".
#'   \item \code{\%P} --- Like \%p but in lowercase: "am" or "pm" or a corresponding string for the current locale.
#'   \item \code{\%r} --- The time in a.m. or p.m. notation.  In the POSIX locale this is equivalent to \%I:\%M:\%S \%p.
#'   \item \code{\%R} --- The time in 24-hour notation (\%H:\%M). For a version including the seconds, see \%T below.
#'   \item \code{\%s} --- The number of seconds since the Epoch, 1970-01-01 00:00:00 +0000 (UTC).
#'   \item \code{\%S} --- The second as a decimal number (range 00 to 60).  (The range is up to 60 to allow for occasional leap seconds.)
#'   \item \code{\%t} --- A tab character. Bokeh text does not currently support tab characters.
#'   \item \code{\%T} --- The time in 24-hour notation (\%H:\%M:\%S).
#'   \item \code{\%u} --- The day of the week as a decimal, range 1 to 7, Monday being 1. See also \%w.
#'   \item \code{\%U} --- The week number of the current year as a decimal number, range 00 to 53, starting with the first Sunday as the first day of week 01.  See also \%V and \%W.
#'   \item \code{\%V} --- The ISO 8601 week number (see NOTES) of the current year as a decimal number, range 01 to 53, where week 1 is the first week that has at least 4 days in the new year.  See also \%U and \%W.
#'   \item \code{\%w} --- The day of the week as a decimal, range 0 to 6, Sunday being 0. See also \%u.
#'   \item \code{\%W} --- The week number of the current year as a decimal number, range 00 to 53, starting with the first Monday as the first day of week 01.
#'   \item \code{\%x} --- The preferred date representation for the current locale without the time.
#'   \item \code{\%X} --- The preferred time representation for the current locale without the date.
#'   \item \code{\%y} --- The year as a decimal number without a century (range 00 to 99).
#'   \item \code{\%Y} --- The year as a decimal number including the century.
#'   \item \code{\%z} --- The +hhmm or -hhmm numeric timezone (that is, the hour and minute offset from UTC).
#'   \item \code{\%Z} --- The timezone name or abbreviation.
#'   \item \code{\%\%} --- A literal '\%' character.
#' }
#' @note The client library BokehJS uses the \href{http://bigeasy.github.io/timezone/}{timezone} library to format datetimes. The inclusion of the list below is based on the claim that timezone makes to support "the full compliment of GNU date format specifiers." However, this claim has not been tested exhaustively against this list. If you find formats that do not function as expected, please submit a \href{https://github.com/bokeh/bokeh/issues}{github issue}, so that the documentation can be updated appropriately.
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

# get_docs(mods, "CategoricalTickFormatter") %>% cat()

#' Display tick values from categorical ranges as string values.
#' @param clear A logical indicating whether all non-specified parameters should be cleared out of any exising specification in the figure. This can be useful when modifying a previously-defined figure.
#' @note This function should never need to be used as there are no parameters for a categorical tickformatter
#' @return A specification that is used as the \code{tickformatter} argument for \code{\link{x_axis}} or \code{\link{y_axis}}.
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

# get_docs(mods, "LinearAxis",
#   c("visible", "major_label_overrides", "bounds", "level", "axis_label_standoff",
#     "axis_label_text_align", "axis_label_text_alpha", "axis_label_text_baseline",
#     "axis_label_text_color", "axis_label_text_font", "axis_label_text_font_size",
#     "axis_label_text_font_style", "major_label_orientation", "major_label_standoff",
#     "major_label_text_align", "major_label_text_alpha", "major_label_text_baseline",
#     "major_label_text_color", "major_label_text_font", "major_label_text_font_size",
#     "major_label_text_font_style", "axis_line_alpha", "axis_line_cap", "axis_line_color",
#     "axis_line_dash", "axis_line_dash_offset", "axis_line_join", "axis_line_width",
#     "major_tick_in", "major_tick_line_alpha", "major_tick_line_cap", "major_tick_line_color",
#     "major_tick_line_dash", "major_tick_line_dash_offset", "major_tick_line_join",
#     "major_tick_line_width", "major_tick_out", "minor_tick_in", "minor_tick_line_alpha",
#     "minor_tick_line_cap", "minor_tick_line_color", "minor_tick_line_dash",
#     "minor_tick_line_dash_offset", "minor_tick_line_join", "minor_tick_line_width",
#     "minor_tick_out")) %>% cat()

#' Specify axis parameters for an axis.
#' @param visible Is the renderer visible.
#' @param major_label_overrides Provide explicit tick label values for specific tick locations that override normal formatting.
#' @param bounds Bounds for the rendered axis. If unset, the axis will span the entire plot in the given dimension.
#' @param level Specifies the level in which to paint this renderer.
#' @param axis_label_standoff The distance in pixels that the axis labels should be offset from the tick labels.
#' @param axis_label_text_align The text align of the axis label.
#' @param axis_label_text_alpha The text alpha of the axis label.
#' @param axis_label_text_baseline The text baseline of the axis label.
#' @param axis_label_text_color The text color of the axis label.
#' @param axis_label_text_font The text font of the axis label.
#' @param axis_label_text_font_size The text font size of the axis label.
#' @param axis_label_text_font_style The text font style of the axis label.
#' @param major_label_orientation What direction the major label text should be oriented. If a number is supplied, the angle of the text is measured from horizontal.
#' @param major_label_standoff The distance in pixels that the major tick labels should be offset from the associated ticks.
#' @param major_label_text_align The text align of the major tick labels.
#' @param major_label_text_alpha The text alpha of the major tick labels.
#' @param major_label_text_baseline The text baseline of the major tick labels.
#' @param major_label_text_color The text color of the major tick labels.
#' @param major_label_text_font The text font of the major tick labels.
#' @param major_label_text_font_size The text font size of the major tick labels.
#' @param major_label_text_font_style The text font style of the major tick labels.
#' @param axis_line_alpha The line alpha of the axis line.
#' @param axis_line_cap The line cap of the axis line.
#' @param axis_line_color The line color of the axis line.
#' @param axis_line_dash The line dash of the axis line.
#' @param axis_line_dash_offset The line dash offset of the axis line.
#' @param axis_line_join The line join of the axis line.
#' @param axis_line_width The line width of the axis line.
#' @param major_tick_in The distance in pixels that major ticks should extend into the main plot area.
#' @param major_tick_line_alpha The line alpha of the major ticks.
#' @param major_tick_line_cap The line cap of the major ticks.
#' @param major_tick_line_color The line color of the major ticks.
#' @param major_tick_line_dash The line dash of the major ticks.
#' @param major_tick_line_dash_offset The line dash offset of the major ticks.
#' @param major_tick_line_join The line join of the major ticks.
#' @param major_tick_line_width The line width of the major ticks.
#' @param major_tick_out The distance in pixels that major ticks should extend out of the main plot area.
#' @param minor_tick_in The distance in pixels that minor ticks should extend into the main plot area.
#' @param minor_tick_line_alpha The line alpha of the minor ticks.
#' @param minor_tick_line_cap The line cap of the minor ticks.
#' @param minor_tick_line_color The line color of the minor ticks.
#' @param minor_tick_line_dash The line dash of the minor ticks.
#' @param minor_tick_line_dash_offset The line dash offset of the minor ticks.
#' @param minor_tick_line_join The line join of the minor ticks.
#' @param minor_tick_line_width The line width of the minor ticks.
#' @param minor_tick_out The distance in pixels that major ticks should extend out of the main plot area.
#' @param clear A logical indicating whether all non-specified parameters should be cleared out of any exising specification in the figure. This can be useful when modifying a previously-defined figure.
#' @note Several of the above attributes can also be set globally using \code{\link{theme_axis}}.
#' @return A specification that is used as the \code{axis} argument for \code{\link{x_axis}} or \code{\link{y_axis}}.
#' @examples
#' figure() %>%
#'   ly_points(1:10) %>%
#'   x_axis(label = "x axis", axis = axis_spec(
#'     axis_label_text_color = "blue",
#'     axis_label_standoff = 30,
#'     axis_label_text_font_size = "20pt"
#'   )) %>%
#'   y_axis(label = "y axis", axis = axis_spec(
#'     major_label_text_color = "red"
#'   )) %>%
#'   rbokeh_prerender(keep_aux = TRUE)
#'   prepare_figure()
#'
#' figure() %>%
#'   ly_points(1:10) %>%
#'   x_axis(axis = axis_spec(bounds = c(3, 8)))
#'
#' figure() %>%
#'   ly_points(1:10) %>%
#'   x_axis(axis = axis_spec(
#'     major_tick_line_color = "firebrick",
#'     major_tick_line_width = 3,
#'     minor_tick_line_color = "orange",
#'     major_tick_out = 10,
#'     minor_tick_in = -3,
#'     minor_tick_out = 8
#'   )) %>%
#'   y_axis(axis = axis_spec(
#'     minor_tick_line_color = NA,
#'     major_tick_out = 10,
#'     minor_tick_in = -3,
#'     minor_tick_out = 8
#'   ))
#'
#' figure() %>%
#'   ly_points(1:10) %>%
#'   x_axis(axis = axis_spec(major_label_orientation = 45)) %>%
#'   y_axis(axis = axis_spec(major_label_orientation = "vertical"))
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


# get_docs(mods, "Grid",
#   c("visible", "level", "bounds", "band_fill_alpha", "band_fill_color",
#     "grid_line_alpha", "grid_line_cap", "grid_line_color", "grid_line_dash",
#     "grid_line_dash_offset", "grid_line_join", "grid_line_width",
#     "minor_grid_line_alpha", "minor_grid_line_cap", "minor_grid_line_color",
#     "minor_grid_line_dash", "minor_grid_line_dash_offset", "minor_grid_line_join",
#     "minor_grid_line_width")) %>% cat()

#' Specify parameters for displaying a horizontal or vertical grid for an axis.
#' @param visible Is the renderer visible.
#' @param level Specifies the level in which to paint this renderer.
#' @param bounds Bounds for the rendered grid lines. If unset, the grid lines will span the entire plot in the given dimension.
#' @param band_fill_alpha The fill alpha of alternating bands between Grid lines.
#' @param band_fill_color The fill color of alternating bands between Grid lines.
#' @param grid_line_alpha The line alpha of the Grid lines.
#' @param grid_line_cap The line cap of the Grid lines.
#' @param grid_line_color The line color of the Grid lines.
#' @param grid_line_dash The line dash of the Grid lines.
#' @param grid_line_dash_offset The line dash offset of the Grid lines.
#' @param grid_line_join The line join of the Grid lines.
#' @param grid_line_width The line width of the Grid lines.
#' @param minor_grid_line_alpha The line alpha of the minor Grid lines.
#' @param minor_grid_line_cap The line cap of the minor Grid lines.
#' @param minor_grid_line_color The line color of the minor Grid lines.
#' @param minor_grid_line_dash The line dash of the minor Grid lines.
#' @param minor_grid_line_dash_offset The line dash offset of the minor Grid lines.
#' @param minor_grid_line_join The line join of the minor Grid lines.
#' @param minor_grid_line_width The line width of the minor Grid lines.
#' @param clear A logical indicating whether all non-specified parameters should be cleared out of any exising specification in the figure. This can be useful when modifying a previously-defined figure.
#' @return A specification that is used as the \code{grid} argument for \code{\link{x_axis}} or \code{\link{y_axis}}.
#' @examples
#' figure() %>%
#'   ly_points(1:10) %>%
#'   x_axis(grid = grid_spec(grid_line_color = NA)) %>%
#'   y_axis(grid = grid_spec(
#'     band_fill_alpha = 0.1,
#'     band_fill_color = "navy"
#'   ))
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

