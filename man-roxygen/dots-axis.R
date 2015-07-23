#' @param \ldots additional parameters for fine control over axes and grids (see additional parameters below)
#' @section Axis parameters:
#' \tabular{ll}{
#' \code{axis_label_standoff} \tab (integer) The distance in pixels that the axis labels should be offset from the tick labels. \cr
#' \code{axis_label_text_align} \tab ('left', 'right', 'center') The text align of the axis label. \cr
#' \code{axis_label_text_alpha} \tab (numeric) The text alpha of the axis label. \cr
#' \code{axis_label_text_baseline} \tab ('top', 'middle', 'bottom', 'alphabetic', 'hanging') The text baseline of the axis label. \cr
#' \code{axis_label_text_color} \tab (color) The text color of the axis label. \cr
#' \code{axis_label_text_font} \tab (string) The text font of the axis label. \cr
#' \code{axis_label_text_font_size} \tab (string - e.g. '12pt') The text font size of the axis label. \cr
#' \code{axis_label_text_font_style} \tab ('normal', 'italic', 'bold') The text font style of the axis label. \cr
#' \code{axis_line_alpha} \tab (numeric) The line alpha of the axis line. \cr
#' \code{axis_line_cap} \tab ('butt', 'round', 'square') The line cap of the axis line. \cr
#' \code{axis_line_color} \tab (color) The line color of the axis line. \cr
#' \code{axis_line_dash} \tab The line dash of the axis line. \cr
#' \code{axis_line_dash_offset} \tab (integer) The line dash offset of the axis line. \cr
#' \code{axis_line_join} \tab ('miter', 'round', 'bevel') The line join of the axis line. \cr
#' \code{axis_line_width} \tab (integer) The line width of the axis line. \cr
#' \code{major_label_orientation} \tab ('horizontal', 'vertical', or angle in degrees) What direction the major label text should be oriented. If a number is supplied, the angle of the text is measured from horizontal. \cr
#' \code{major_label_standoff} \tab (integer) The distance in pixels that the major tick labels should be offset from the associated ticks. \cr
#' \code{major_label_text_align} \tab ('left', 'right', 'center') The text align of the major tick labels. \cr
#' \code{major_label_text_alpha} \tab (numeric) The text alpha of the major tick labels. \cr
#' \code{major_label_text_baseline} \tab ('top', 'middle', 'bottom', 'alphabetic', 'hanging') The text baseline of the major tick labels. \cr
#' \code{major_label_text_color} \tab (color) The text color of the major tick labels. \cr
#' \code{major_label_text_font} \tab (string - 'Helvetica') The text font of the major tick labels. \cr
#' \code{major_label_text_font_size} \tab (string - e.g. '12pt') The text font size of the major tick labels. \cr
#' \code{major_label_text_font_style} \tab ('normal', 'italic', 'bold') The text font style of the major tick labels. \cr
#' \code{major_tick_in} \tab (integer) The distance in pixels that major ticks should extend into the main plot area. \cr
#' \code{major_tick_line_alpha} \tab (numeric) The line alpha of the major ticks. \cr
#' \code{major_tick_line_cap} \tab ('butt', 'round', 'square') The line cap of the major ticks. \cr
#' \code{major_tick_line_color} \tab (color) The line color of the major ticks. \cr
#' \code{major_tick_line_dash} \tab The line dash of the major ticks. \cr
#' \code{major_tick_line_dash_offset} \tab (integer) The line dash offset of the major ticks. \cr
#' \code{major_tick_line_join} \tab ('miter', 'round', 'bevel') The line join of the major ticks. \cr
#' \code{major_tick_line_width} \tab (integer) The line width of the major ticks. \cr
#' \code{major_tick_out} \tab (integer) The distance in pixels that major ticks should extend out of the main plot area. \cr
#' \code{minor_tick_in} \tab (integer) The distance in pixels that minor ticks should extend into the main plot area. \cr
#' \code{minor_tick_line_alpha} \tab (numeric) The line alpha of the minor ticks. \cr
#' \code{minor_tick_line_cap} \tab ('butt', 'round', 'square') The line cap of the minor ticks. \cr
#' \code{minor_tick_line_color} \tab (color) The line color of the minor ticks. \cr
#' \code{minor_tick_line_dash} \tab The line dash of the minor ticks. \cr
#' \code{minor_tick_line_dash_offset} \tab (integer) The line dash offset of the minor ticks. \cr
#' \code{minor_tick_line_join} \tab ('miter', 'round', 'bevel') The line join of the minor ticks. \cr
#' \code{minor_tick_line_width} \tab (integer) The line width of the minor ticks. \cr
#' \code{minor_tick_out} \tab (integer) The distance in pixels that major ticks should extend out of the main plot area. \cr
#' }
#' \subsection{Basic Tick Format}{
#' \tabular{ll}{
#' \code{power_limit_high} \tab (int) Limit the use of scientific notation to when log(x) >= value. \cr
#' \code{power_limit_low} \tab (int) Limit the use of scientific notation to when log(x) <= value. \cr
#' \code{precision} \tab (int) How many digits of precision to display in tick labels. Automatically
#'  determined if not specified. \cr
#' \code{use_scientific} \tab (logical) Whether to ever display scientific notation. If True, then when to use scientific
#'  notation is controlled by \code{power_limit_low} and \code{power_limit_high}. \cr
#'  }}
#' \subsection{Datetime Tick Format}{
#' \tabular{ll}{
#' \code{formats} \tab (list) Display tick values from a continuous range as formatted datetimes.
#'  See \href{http://bokeh.pydata.org/en/latest/docs/reference/models.html#bokeh.models.formatters.DatetimeTickFormatter}{DatetimeTickFormatter}. \cr
#'  }}
#' \subsection{Numeral Tick Format}{
#' \tabular{ll}{
#' \code{format} \tab (string) Tick formatter based on a human-readable format string.
#'  See \href{http://bokeh.pydata.org/en/latest/docs/reference/models.html#bokeh.models.formatters.NumeralTickFormatter}{NumeralTickFormatter}. \cr
#'  }}
#' \subsection{Printf Tick Format}{
#' \tabular{ll}{
#' \code{format} \tab (string) Tick formatter based on a printf-style format string.
#'  See \href{http://bokeh.pydata.org/en/latest/docs/reference/models.html#bokeh.models.formatters.PrintfTickFormatter}{PrintfTickFormatter}. \cr
#'  }}
