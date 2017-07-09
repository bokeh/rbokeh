# #' Customize x axis of a Bokeh figure
# #' @param fig figure to modify
# #' @param label axis label
# #' @param position where to place the axis (either "above" or "below")
# #' @param log logical or integer - if TRUE, a log axis with base 10 is used - if an integer, a log axis with base of that integer will be used
# #' @param grid logical - should a reference grid be shown for this axis?
# #' @param desired_num_ticks desired target number of major tick positions to generate across the plot range
# #' @param num_minor_ticks number of minor ticks
# #' @param visible should axis be shown?
# #' @param number_formatter Bokeh numeric tick label formatter
# #'  (\href{http://bokeh.pydata.org/en/latest/docs/reference/models.html#bokeh.models.formatters.BasicTickFormatter}{"basic"},
# #'  \href{http://bokeh.pydata.org/en/latest/docs/reference/models.html#bokeh.models.formatters.NumeralTickFormatter}{"numeral"},
# #'  or \href{http://bokeh.pydata.org/en/latest/docs/reference/models.html#bokeh.models.formatters.PrintfTickFormatter}{"printf"});
# #'  ignored if \code{log} is TRUE
# #' @param power_limit_high (int) Limit the use of scientific notation to when log(x) >= value. Only applicable when \code{number_formatter} is "basic".
# #' @param power_limit_low (int) Limit the use of scientific notation to when log(x) <= value. Only applicable when \code{number_formatter} is "basic".
# #' @param precision (int) How many digits of precision to display in tick labels. Automatically determined if not specified. Only applicable when \code{number_formatter} is "basic".
# #' @param use_scientific (logical) Whether to ever display scientific notation. If True, then when to use scientific notation is controlled by \code{power_limit_low} and \code{power_limit_high}. Only applicable when \code{number_formatter} is "basic".
# #' @param format Specification of format options.  Specification depends on the value of \code{number_formatter} - see "details" below.
# #' @details \code{format} parameter:
# #' When \code{number_formatter} is "basic" and the axis type is datetime, \code{format} specifies how to display tick values from a continuous range as formatted datetimes. See \href{http://bokeh.pydata.org/en/latest/docs/reference/models.html#bokeh.models.formatters.DatetimeTickFormatter}{DatetimeTickFormatter}
# #' When \code{number_formatter} is "numeral", \code{format} specifies a human-readable format string. See \href{http://bokeh.pydata.org/en/latest/docs/reference/models.html#bokeh.models.formatters.NumeralTickFormatter}{NumeralTickFormatter}.
# #' When \code{number_formatter} is "printf", \code{format} is a printf-style format string. See \href{http://bokeh.pydata.org/en/latest/docs/reference/models.html#bokeh.models.formatters.PrintfTickFormatter}{PrintfTickFormatter}.
# #' @family axes
# #' @example man-roxygen/ex-axis.R
# #' @export
# x_axis <- function(fig, label, position = "below", log = FALSE,
#   grid = TRUE, desired_num_ticks = NULL, num_minor_ticks = 5,
#   visible = TRUE, number_formatter = c("basic", "numeral", "printf"),
#   power_limit_high = 5, power_limit_low = -3, precision = NULL,
#   use_scientific = TRUE, format = NULL) {

#   if (is.null(position))
#     position <- "below"
#   if (!position %in% c("below", "above")) {
#     message("x axis position must be either below or above - setting to 'below'")
#     position <- "below"
#   }

#   if (is.logical(log)) {
#     if (log) {
#       log <- 10.0
#     } else {
#       log <- NULL
#     }
#   } else {
#     log <- as.numeric(log)
#   }

#   if (missing(label))
#     label <- fig$x$spec$xlab
#   fig$x$spec$xlab <- label

#   format_pars <- list(power_limit_high = power_limit_high,
#     power_limit_low = power_limit_low,
#     precision = precision, use_scientific = use_scientific,
#     format = format)
#   specified <- names(as.list(match.call())[-1])
#   format_pars <- format_pars[names(format_pars) %in% specified]

#   update_axis(fig, position = position, label = label, grid = grid,
#     desired_num_ticks = desired_num_ticks,
#     num_minor_ticks = num_minor_ticks, visible = visible,
#     log = log, number_formatter = match.arg(number_formatter),
#     format_pars = format_pars)
# }

# #' Customize x axis of a Bokeh figure
# #' @inheritParams x_axis
# #' @param position where to place the axis (either "left" or "right")
# #' @family axes
# #' @example man-roxygen/ex-axis.R
# #' @export
# y_axis <- function(fig, label, position = "left", log = FALSE,
#   grid = TRUE, desired_num_ticks = NULL, num_minor_ticks = 5,
#   visible = TRUE, number_formatter = c("basic", "numeral", "printf"),
#   power_limit_high = 5, power_limit_low = -3, precision = NULL,
#   use_scientific = TRUE, format = NULL) {

#   if (is.null(position))
#     position <- "left"
#   if (!position %in% c("left", "right")) {
#     message("y axis position must be either left or right - setting to 'left'")
#     position <- "left"
#   }

#   if (is.logical(log)) {
#     if (log) {
#       log <- 10.0
#     } else {
#       log <- NULL
#     }
#   } else {
#     log <- as.numeric(log)
#   }

#   if (missing(label))
#     label <- fig$x$spec$ylab
#   fig$x$spec$ylab <- label

#   format_pars <- list(power_limit_high = power_limit_high,
#     power_limit_low = power_limit_low,
#     precision = precision, use_scientific = use_scientific,
#     format = format)
#   specified <- names(as.list(match.call())[-1])
#   format_pars <- format_pars[names(format_pars) %in% specified]

#   update_axis(fig, position = position, label = label, grid = grid,
#     desired_num_ticks = desired_num_ticks,
#     num_minor_ticks = num_minor_ticks, visible = visible,
#     log = log, number_formatter = match.arg(number_formatter),
#     format_pars = format_pars)
# }
NULL
