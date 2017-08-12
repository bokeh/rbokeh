#' Create hover specification
#'
#' @param line_policy When showing tooltips for lines, designates whether the tooltip position should be the "previous" or "next" points on the line, the "nearest" point to the current mouse position, or "interpolate" alongthe line to the current mouse position. Must be one of 'prev', 'next', 'nearest', 'interp', 'none'.
#' @param point_policy Whether the tooltip position should snap to the "center" (or other anchor) position of the associated glyph, or always follow the current mouse cursor position. Must be one of 'snap_to_data', 'follow_mouse', 'none'.
#' @param anchor If point policy is set to `"snap_to_data"`, `anchor` defines the attachment point of a tooltip. The default is to attach to the center of a glyph. Must be one of 'top_left', 'top_center', 'top_right', 'center_left', 'center', 'center_right', 'bottom_left', 'bottom_center', 'bottom_right'.
#' @param show_arrow Boolean. Whether tooltip's arrow should be shown.
#' @param mode Whether to consider hover pointer as a point (x/y values), or a span on h or v directions. Must be one of 'mouse', 'hline', 'vline'.
#' @param attachment Whether tooltip's arrow should appear in the horizontal or vertical dimension. Must be one of 'horizontal', 'vertical'.
#' @param callback A callback to run in the browser whenever the input's value changes. The cb_data parameter that is available to the Callback code will contain two HoverTool specific fields: :index: object containing the indices of the hovered points in the data, and source :geometry: object containing the coordinates of the hover cursor.
#' @export
hov <- function(
  data = NULL,
  line_policy = NULL,
  callback = NULL,
  anchor = NULL,
  show_arrow = NULL,
  mode = NULL,
  attachment = NULL,
  point_policy = NULL) {

  spec <- list()
  spec$data <- enquo(data)
  spec$line_policy <- line_policy
  spec$callback <- callback
  spec$anchor <- anchor
  spec$show_arrow <- show_arrow
  spec$mode <- mode
  spec$attachment <- attachment
  spec$point_policy <- point_policy

  class(spec) <- c("list", "hover_spec")

  spec
}




#  # The (name, field) pairs describing what the hover tool should display
#  # when there is a hit.
#  #
#  # Field names starting with "@" are interpreted as columns on the data
#  # source. For instance, "@temp" would look up values to display from the
#  # "temp" column of the data source.
#  #
#  # Field names starting with "$" are special, known fields:
#  #
#  # :$index: index of selected point in the data source :$x: x-coordinate
#  # under the cursor in data space :$y: y-coordinate under the cursor in
#  # data space :$sx: x-coordinate under the cursor in screen (canvas) space
#  # :$sy: y-coordinate under the cursor in screen (canvas) space :$color:
#  # color data from data source, with the syntax:
#  # ``$color[options]:fie`ld_name``. The available options are: 'hex' (to
#  # display the color as a hex value), and 'swatch' to also display a small
#  # color swatch.
#  #
#  # Field names that begin with ``@`` are associated with columns in a
#  # ``ColumnDataSource``. For instance the field name ``"@price"`` will
#  # display values from the ``"price"`` column whenever a hover is
#  # triggered.  If the hover is for the 17th glyph, then the hover tooltip
#  # will correspondingly display the 17th price value.
#  #
#  # Note that if a column name contains spaces, the it must be supplied by
#  # surrounding it in curly braces, e.g. ``@{adjusted close}`` will display
#  # values from a column named ``"adjusted close"``.
#  #
#  # By default, values for fields (e.g. ``@foo``) are displayed in a basic
#  # numeric format. However it is possible to control the formatting of
#  # values more precisely. Fields can be modified by appending a format
#  # specified to the end in curly braces. Some examples are below.
#  #
#  # .. code-block:: python
#  #
#  # "@foo{0,0.000}" # formats 10000.1234 as: 10,000.123
#  #
#  # "@foo{(.00)}" # formats -10000.1234 as: (10000.123)
#  #
#  # "@foo{($ 0.00 a)}" # formats 1230974 as: $ 1.23 m
#  #
#  # Specifying a format ``{safe}`` after a field name will override
#  # automatic escaping of the tooltip data source. Any HTML tags in the
#  # data tags will be rendered as HTML in the resulting HoverTool output.
#  # See :ref:`custom_hover_tooltip` for a more detailed example.
#  #
#  # ``None`` is also a valid value for tooltips. This turns off the
#  # rendering of tooltips. This is mostly useful when supplying other
#  # actions on hover via the callback property.
#  #
#  # .. note:: The tooltips attribute can also be configured with a mapping
#  # type, e.g. ``dict`` or ``OrderedDict``. However, if a ``dict`` is used,
#  # the visual presentation order is unspecified.
#  # > Either(String, List(Tuple(String, String)))
#  tooltips = NULL,
#  # A list of names to query for. If set, only renderers that have a
#  # matching value for their ``name`` attribute will be used.
#  # > List(String)
#  names = NULL,
#  # An explicit list of renderers to hit test again. If unset, defaults to
#  # all renderers on a plot.
#  # > List(Instance(Renderer))
#  renderers = NULL,
#  # Specify the formatting scheme for data source columns, e.g.
#  #
#  # .. code-block:: python
#  #
#  # tool.formatters = dict(date="datetime")
#  #
#  # will cause format specifications for the "date" column to be
#  # interpreted according to the "datetime" formatting scheme. The
#  # following schemed are available:
#  #
#  # :``"numeral"``: Provides a wide variety of formats for numbers,
#  # currency, bytes, times, and percentages. The full set of formats can be
#  # found in the |NumeralTickFormatter| reference documentation.
#  #
#  # :``"datetime"``: Provides formats for date and time values. The full
#  # set of formats is listed in the |DatetimeTickFormatter| reference
#  # documentation.
#  #
#  # :``"printf"``: Provides formats similar to C-style "printf" type
#  # specifiers. See the |PrintfTickFormatter| reference documentation for
#  # complete details.
#  #
#  # If no formatter is specified for a column name, the default
#  # ``"numeral"`` formatter is assumed.
#  #
#  # .. |NumeralTickFormatter| replace::
#  # :class:`~bokeh.models.formatters.NumeralTickFormatter` ..
#  # |DatetimeTickFormatter| replace::
#  # :class:`~bokeh.models.formatters.DatetimeTickFormatter` ..
#  # |PrintfTickFormatter| replace::
#  # :class:`~bokeh.models.formatters.PrintfTickFormatter`
#  # > Dict(String, Enum('numeral', 'datetime', 'printf'))
#  formatters = NULL
