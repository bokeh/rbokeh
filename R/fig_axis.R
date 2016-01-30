#' Customize x axis of a Bokeh figure
#' @param fig figure to modify
#' @param label axis label
#' @param position where to place the axis (either "above" or "below")
#' @param log logical or integer - if TRUE, a log axis with base 10 is used - if an integer, a log axis with base of that integer will be used
#' @param grid logical - should a reference grid be shown for this axis?
#' @param desired_num_ticks desired target number of major tick positions to generate across the plot range
#' @param num_minor_ticks number of minor ticks
#' @param visible should axis be shown?
#' @param number_formatter Bokeh numeric tick label formatter
#'  (\href{http://bokeh.pydata.org/en/latest/docs/reference/models.html#bokeh.models.formatters.BasicTickFormatter}{"basic"},
#'  \href{http://bokeh.pydata.org/en/latest/docs/reference/models.html#bokeh.models.formatters.NumeralTickFormatter}{"numeral"},
#'  or \href{http://bokeh.pydata.org/en/latest/docs/reference/models.html#bokeh.models.formatters.PrintfTickFormatter}{"printf"});
#'  ignored if \code{log} is TRUE
#' @param power_limit_high (int) Limit the use of scientific notation to when log(x) >= value. Only applicable when \code{number_formatter} is "basic".
#' @param power_limit_low (int) Limit the use of scientific notation to when log(x) <= value. Only applicable when \code{number_formatter} is "basic".
#' @param precision (int) How many digits of precision to display in tick labels. Automatically determined if not specified. Only applicable when \code{number_formatter} is "basic".
#' @param use_scientific (logical) Whether to ever display scientific notation. If True, then when to use scientific notation is controlled by \code{power_limit_low} and \code{power_limit_high}. Only applicable when \code{number_formatter} is "basic".
#' @param format Specification of format options.  Specification depends on the value of \code{number_formatter} - see "details" below.
#' @details \code{format} parameter:
#' When \code{number_formatter} is "basic" and the axis type is datetime, \code{format} specifies how to display tick values from a continuous range as formatted datetimes. See \href{http://bokeh.pydata.org/en/latest/docs/reference/models.html#bokeh.models.formatters.DatetimeTickFormatter}{DatetimeTickFormatter}
#' When \code{number_formatter} is "numeral", \code{format} specifies a human-readable format string. See \href{http://bokeh.pydata.org/en/latest/docs/reference/models.html#bokeh.models.formatters.NumeralTickFormatter}{NumeralTickFormatter}.
#' When \code{number_formatter} is "printf", \code{format} is a printf-style format string. See \href{http://bokeh.pydata.org/en/latest/docs/reference/models.html#bokeh.models.formatters.PrintfTickFormatter}{PrintfTickFormatter}.
#' @family axes
#' @example man-roxygen/ex-axis.R
#' @export
x_axis <- function(fig, label, position = "below", log = FALSE,
  grid = TRUE, desired_num_ticks = NULL, num_minor_ticks = 5,
  visible = TRUE, number_formatter = c("basic", "numeral", "printf"),
  power_limit_high = 5, power_limit_low = -3, precision = NULL,
  use_scientific = TRUE, format = NULL) {

  if(is.null(position))
    position <- "below"
  if(!position %in% c("below", "above")) {
    message("x axis position must be either below or above - setting to 'below'")
    position <- "below"
  }

  if(is.logical(log)) {
    if(log) {
      log <- 10.0
    } else {
      log <- NULL
    }
  } else {
    log <- as.numeric(log)
  }

  if(missing(label))
    label <- fig$x$spec$xlab
  fig$x$spec$xlab <- label

  format_pars <- list(power_limit_high = power_limit_high,
    power_limit_low = power_limit_low,
    precision = precision, use_scientific = use_scientific,
    format = format)
  specified <- names(as.list(match.call())[-1])
  format_pars <- format_pars[names(format_pars) %in% specified]

  update_axis(fig, position = position, label = label, grid = grid,
    desired_num_ticks = desired_num_ticks,
    num_minor_ticks = num_minor_ticks, visible = visible,
    log = log, number_formatter = match.arg(number_formatter),
    format_pars = format_pars)
}

#' Customize x axis of a Bokeh figure
#' @inheritParams x_axis
#' @param position where to place the axis (either "left" or "right")
#' @family axes
#' @example man-roxygen/ex-axis.R
#' @export
y_axis <- function(fig, label, position = "left", log = FALSE,
  grid = TRUE, desired_num_ticks = NULL, num_minor_ticks = 5,
  visible = TRUE, number_formatter = c("basic", "numeral", "printf"),
  power_limit_high = 5, power_limit_low = -3, precision = NULL,
  use_scientific = TRUE, format = NULL) {

  if(is.null(position))
    position <- "left"
  if(!position %in% c("left", "right")) {
    message("y axis position must be either left or right - setting to 'left'")
    position <- "left"
  }

  if(is.logical(log)) {
    if(log) {
      log <- 10.0
    } else {
      log <- NULL
    }
  } else {
    log <- as.numeric(log)
  }

  if(missing(label))
    label <- fig$x$spec$ylab
  fig$x$spec$ylab <- label

  format_pars <- list(power_limit_high = power_limit_high,
    power_limit_low = power_limit_low,
    precision = precision, use_scientific = use_scientific,
    format = format)
  specified <- names(as.list(match.call())[-1])
  format_pars <- format_pars[names(format_pars) %in% specified]

  update_axis(fig, position = position, label = label, grid = grid,
    desired_num_ticks = desired_num_ticks,
    num_minor_ticks = num_minor_ticks, visible = visible,
    log = log, number_formatter = match.arg(number_formatter),
    format_pars = format_pars)
}

# axis ref needs to be added to plot attributes as "above", "below", "left", or "right"
# axis ref needs to be added to plot attributes -> renderers as well
# then axis model added to object
# axis model also depends on the following references:
# - plot (already have that)
# - formatter
# - ticker

# formatter model added to object
# ticker model added to object, also referred to in grid
# also create grid

update_axis <- function(fig, position, label, grid = TRUE,
  desired_num_ticks = NULL, num_minor_ticks = 5, visible = TRUE,
  log = NULL, number_formatter = c("basic", "numeral", "printf"),
  format_pars = NULL) {

  is_y <- position %in% c("left", "right")

  f_id <- gen_id(fig, c(position, "formatter"))
  t_id <- gen_id(fig, c(position, "ticker"))
  a_id <- gen_id(fig, position)

  f_name <- ifelse(is_y, "y_formatter", "x_formatter")
  t_name <- ifelse(is_y, "y_tickformatter", "x_tickformatter")
  a_name <- ifelse(is_y, "y_axis", "x_axis")

  axis_type <- ifelse(is_y,
    fig$x$spec$y_axis_type, fig$x$spec$x_axis_type)

  if(axis_type == "numeric") {
    if(!is.null(log)) {
      type_list <- list(format = "LogTickFormatter", tick = "LogTicker", axis = "LogAxis")
      if(is_y) {
        fig$x$spec$model$plot$attributes$y_mapper_type <- "log"
      } else {
        fig$x$spec$model$plot$attributes$x_mapper_type <- "log"
      }
    } else {
      type_list <- list(
        format = paste0(simple_cap(match.arg(number_formatter)), "TickFormatter"),
        tick = "BasicTicker",
        axis = "LinearAxis")

      format_pars <- handle_extra_pars(format_pars,
        get(paste0(number_formatter, "_tick_formatter_map")))
    }
  } else if(axis_type == "datetime") {
    type_list <- list(format = "DatetimeTickFormatter",
      tick = "DatetimeTicker", axis = "DatetimeAxis")
    format_pars$formats <- format_pars$format
    format_pars$format <- NULL
    format_pars <- handle_extra_pars(format_pars,
      datetime_tick_formatter_map)
  } else {
    type_list <- list(format = "CategoricalTickFormatter",
      tick = "CategoricalTicker", axis = "CategoricalAxis")
  }

  formatter <- formatter_model(type_list$format, f_id, format_pars)
  ticker <- ticker_model(type_list$tick, t_id, desired_num_ticks,
    num_minor_ticks, log)
  axis <- axis_model(type = type_list$axis, label = label,
    id = a_id, plot_ref = fig$x$spec$ref,
    formatter_ref = formatter$ref, ticker_ref = ticker$ref,
    visible = visible)

  fig$x$spec$model$plot$attributes[[position]][[1]] <- axis$ref
  fig$x$spec$model$plot$attributes$renderers[[axis$ref$id]] <- axis$ref

  fig$x$spec$model[[a_name]] <- axis$model
  fig$x$spec$model[[f_name]] <- formatter$model
  fig$x$spec$model[[t_name]] <- ticker$model

  if(grid) {
    g_id <- gen_id(fig, c(position, "grid"))
    g_name <- ifelse(is_y, "y_grid", "x_grid")

    grid <- grid_model(g_id, plot_ref = fig$x$spec$ref,
      ticker_ref = ticker$ref, dimension = as.integer(is_y))

    fig$x$spec$model$plot$attributes$renderers[[grid$ref$id]] <- grid$ref

    fig$x$spec$model[[g_name]] <- grid$model
  }

  if(is_y) {
    fig$x$spec$has_y_axis <- TRUE
  } else {
    fig$x$spec$has_x_axis <- TRUE
  }

  fig
}

axis_model <- function(type = "LinearAxis", label = NULL, id,
  plot_ref, formatter_ref, ticker_ref, visible) {

  res <- base_model_object(type, id)
  res$model$attributes$plot <- plot_ref
  res$model$attributes$axis_label <- label
  res$model$attributes$formatter <- formatter_ref
  res$model$attributes$ticker <- ticker_ref
  res$model$attributes$visible <- visible

  res
}

formatter_model <- function(type = "BasicTickFormatter",
  id, format_pars) {
  res <- base_model_object(type, id)
  res$model$attributes <- c(res$model$attributes, format_pars)

  res
}

ticker_model <- function(type = "BasicTicker", id,
  desired_num_ticks = NULL, num_minor_ticks = 5, log = NULL) {

  res <- base_model_object(type, id)
  res$model$attributes$num_minor_ticks <- num_minor_ticks
  res$model$attributes$desired_num_ticks <- desired_num_ticks
  if(!is.null(log))
    res$model$attributes$base <- log

  res
}

grid_model <- function(id, dimension = 0, plot_ref, ticker_ref) {
  res <- base_model_object("Grid", id)
  res$model$attributes$dimension <- dimension
  res$model$attributes$plot <- plot_ref
  res$model$attributes$ticker <- ticker_ref
  res$model$attributes <- c(res$model$attributes)

  res
}

axis_par_validator_map <- list(
  "axis_label_standoff" = "int",
  "major_label_standoff" = "int",
  "major_tick_in" = "int",
  "major_tick_line_dash_offset" = "int",
  "major_tick_out" = "int",
  "minor_tick_in" = "int",
  "minor_tick_out" = "int",
  "minor_tick_line_dash_offset" = "int",
  "axis_line_dash_offset" = "int",
  "axis_label_text_alpha" = "num_data_spec",
  "axis_line_alpha" = "num_data_spec",
  "axis_line_width" = "num_data_spec",
  "major_label_text_alpha" = "num_data_spec",
  "major_tick_line_alpha" = "num_data_spec",
  "major_tick_line_width" = "num_data_spec",
  "minor_tick_line_alpha" = "num_data_spec",
  "minor_tick_line_width" = "num_data_spec",
  "axis_label_text_color" = "color",
  "axis_line_color" = "color",
  "major_label_text_color" = "color",
  "major_tick_line_color" = "color",
  "minor_tick_line_color" = "color",
  "axis_label_text_font" = "string",
  "major_label_text_font" = "string",
  "axis_label_text_font_size" = "font_size_string",
  "major_label_text_font_size" = "font_size_string",
  "axis_line_dash" = "line_dash",
  "major_tick_line_dash" = "line_dash",
  "minor_tick_line_dash" = "line_dash",
  "axis_label_text_align" = "text_align",
  "major_label_text_align" = "text_align",
  "axis_label_text_baseline" = "text_baseline",
  "major_label_text_baseline" = "text_baseline",
  "axis_label_text_font_style" = "font_style",
  "major_label_text_font_style" = "font_style",
  "axis_line_cap" = "line_cap",
  "major_tick_line_cap" = "line_cap",
  "minor_tick_line_cap" = "line_cap",
  "axis_line_join" = "line_join",
  "major_tick_line_join" = "line_join",
  "minor_tick_line_join" = "line_join",
  "major_label_orientation" = "label_orientation",
  "num_minor_ticks" = "int" # this is in ticker
)

grid_par_validator_map <- list(
  "band_fill_alpha" = "num_data_spec",
  "band_fill_color" = "color",
  "grid_line_alpha" = "num_data_spec",
  "grid_line_cap" = "line_cap",
  "grid_line_color" = "color",
  "grid_line_dash" = "line_dash",
  "grid_line_dash_offset" = "",
  "grid_line_join" = "line_join",
  "grid_line_width" = "num_data_spec",
  "minor_grid_line_alpha" = "num_data_spec",
  "minor_grid_line_cap" = "line_cap",
  "minor_grid_line_color" = "color",
  "minor_grid_line_dash" = "line_dash",
  "minor_grid_line_dash_offset" = "int",
  "minor_grid_line_join" = "line_join",
  "minor_grid_line_width" = "num_data_spec"
)

basic_tick_formatter_map <- list(
  "power_limit_high" = "int",
  "power_limit_low" = "int",
  "precision" = "int",
  "use_scientific" = "logical"
)

datetime_tick_formatter_map <- list(
  "formats" = "datetime_format"
)

numeral_tick_formatter_map <- list(
  "format" = "string",
  "language" = "string",
  "rounding" = "string"
)

printf_tick_formatter_map <- list(
  "format" = "string"
)
