# Base class for all objects stored in Bokeh |Document| instances.
Model <- R6::R6Class("Model",
  inherit = Base,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(id = id)
      types <- bk_prop_types[["Model"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # List of events that are subscribed to by Python callbacks. This is the
    # set of events that will be communicated from BokehJS back to Python for
    # this model.
    # > List(String)
    subscribed_events = NULL,
    # A mapping of event names to lists of CustomJS callbacks.
    # 
    # Typically, rather then modifying this property directly, callbacks
    # should be added using the ``Model.js_on_event`` method:
    # 
    # .. code:: python
    # 
    # callback = CustomJS(code="console.log('tap event occured')")
    # plot.js_on_event('tap', callback)
    # > Dict(String, List(Instance(CustomJS)))
    js_event_callbacks = NULL,
    # A mapping of attribute names to lists of CustomJS callbacks, to be set
    # up on BokehJS side when the document is created.
    # 
    # Typically, rather then modifying this property directly, callbacks
    # should be added using the ``Model.js_on_change`` method:
    # 
    # .. code:: python
    # 
    # callback = CustomJS(code="console.log('stuff')")
    # plot.x_range.js_on_change('start', callback)
    # > Dict(String, List(Instance(CustomJS)))
    js_property_callbacks = NULL,
    # An arbitrary, user-supplied name for this model.
    # 
    # This name can be useful when querying the document to retrieve specific
    # Bokeh models.
    # 
    # .. code:: python
    # 
    # >>> plot.circle([1,2,3], [4,5,6], name="temp") >>>
    # plot.select(name="temp")
    # [GlyphRenderer(id='399d53f5-73e9-44d9-9527-544b761c7705', ...)]
    # 
    # .. note:: No uniqueness guarantees or other conditions are enforced on
    # any names that are provided, nor is the name used directly by Bokeh for
    # any reason.
    # > String
    name = NULL,
    # An optional list of arbitrary, user-supplied values to attach to this
    # model.
    # 
    # This data can be useful when querying the document to retrieve specific
    # Bokeh models:
    # 
    # .. code:: python
    # 
    # >>> r = plot.circle([1,2,3], [4,5,6]) >>> r.tags = ["foo", 10] >>>
    # plot.select(tags=['foo', 10])
    # [GlyphRenderer(id='1de4c3df-a83d-480a-899b-fb263d3d5dd9', ...)]
    # 
    # Or simply a convenient way to attach any necessary metadata to a model
    # that can be accessed by CustomJS callbacks, etc.
    # 
    # .. note:: No uniqueness guarantees or other conditions are enforced on
    # any tags that are provided, nor are the tags used directly by Bokeh for
    # any reason.
    # > List(Any)
    tags = NULL
  )
)

# *toolbar icon*: |reset_icon|
# 
# The reset tool is an action. When activated in the toolbar, the tool
# resets the data bounds of the plot to their values when the plot was
# initially created.
# 
# Optionally, the reset tool also resets the plat canvas dimensions to
# their original size
# 
# .. |reset_icon| image:: /_images/icons/Reset.png :height: 18pt
ResetTool <- R6::R6Class("ResetTool",
  inherit = Action,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      tags = list(), subscribed_events = list(), reset_size = TRUE,
      name = NULL, js_property_callbacks = structure(list(), .Names =
      character(0)), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["ResetTool"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Whether activating the Reset tool should also reset the plot's canvas
    # dimensions to their original size.
    # > Bool
    reset_size = NULL
  )
)

# Base class for ``Scale`` models that represent an invertible
# computation to be carried out on the client-side.
# 
# JavaScript implementations should implement the following methods:
# 
# .. code-block: coffeescript
# 
# compute: (x) -> # compute the transform of a single value
# 
# v_compute: (xs) -> # compute the transform of an array of values
# 
# invert: (xprime) -> # compute the inverse transform of a single value
# 
# v_invert: (xprimes) -> # compute the inverse transform of an array of
# values
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
Scale <- R6::R6Class("Scale",
  inherit = Transform,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Scale"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Apply a custom defined transform to data.
CustomJSTransform <- R6::R6Class("CustomJSTransform",
  inherit = Transform,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      func = "", v_func = "", tags = list(), args = structure(list(), .Names =
      character(0)), subscribed_events = list(), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["CustomJSTransform"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # A snippet of JavaScript code to transform a single value. The variable
    # ``x`` will contain the untransformed value and can be expected to be
    # present in the function namespace at render time. The snippet will be
    # into the body of a function and therefore requires a return statement.
    # 
    # Example:
    # 
    # .. code-block:: javascript
    # 
    # func = ''' return Math.floor(x) + 0.5 '''
    # > String
    func = NULL,
    # A snippet of JavaScript code to transform an array of values. The
    # variable ``xs`` will contain the untransformed array and can be
    # expected to be present in the function namespace at render time. The
    # snippet will be into the body of a function and therefore requires a
    # return statement.
    # 
    # Example:
    # 
    # .. code-block:: javascript
    # 
    # v_func = ''' new_xs = new Array(xs.length) for(i = 0; i < xs.length;
    # i++) { new_xs[i] = xs[i] + 0.5 } return new_xs '''
    # 
    # .. warning:: The vectorized function, ``v_func``, must return an array
    # of the same length as the input ``xs`` array.
    # > String
    v_func = NULL,
    # A mapping of names to Bokeh plot objects. These objects are made
    # available to the callback code snippet as the values of named
    # parameters to the callback.
    # > Dict(String, Instance(Model))
    args = NULL
  )
)

# Render a horizontal or vertical line span.
Span <- R6::R6Class("Span",
  inherit = Annotation,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      visible = TRUE, line_join = "miter", dimension = "width",
      line_width = 1L, line_cap = "butt", location_units = "data",
      x_range_name = "default", name = NULL, location = NULL,
      line_dash = list(), y_range_name = "default", plot = NULL,
      line_color = "black", tags = list(), subscribed_events = list(),
      line_alpha = 1, render_mode = "canvas", level = "annotation",
      line_dash_offset = 0L, id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        visible = visible, name = name, plot = plot, tags = tags,
        subscribed_events = subscribed_events, level = level,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["Span"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The line join values for the span.
    # > Enum('miter', 'round', 'bevel')
    line_join = NULL,
    # The direction of the span.
    # > Enum('width', 'height')
    dimension = NULL,
    # The line width values for the span.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_width = NULL,
    # The line cap values for the span.
    # > Enum('butt', 'round', 'square')
    line_cap = NULL,
    # The unit type for the location attribute. Interpreted as "data space"
    # units by default.
    # > Enum('screen', 'data')
    location_units = NULL,
    # A particular (named) x-range to use for computing screen locations when
    # rendering annotations on the plot. If unset, use the default x-range.
    # > String
    x_range_name = NULL,
    # The location of the span, along ``dimension``.
    # > Float
    location = NULL,
    # The line dash values for the span.
    # > DashPattern
    line_dash = NULL,
    # A particular (named) y-range to use for computing screen locations when
    # rendering annotations on the plot. If unset, use the default y-range.
    # > String
    y_range_name = NULL,
    # The line color values for the span.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    line_color = NULL,
    # The line alpha values for the span.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_alpha = NULL,
    # Specifies whether the span is rendered as a canvas element or as an css
    # element overlaid on the canvas. The default mode is "canvas".
    # 
    # .. warning:: The line_dash and line_dash_offset attributes aren't
    # supported if the render_mode is set to "css"
    # > Enum('canvas', 'css')
    render_mode = NULL,
    # The line dash offset values for the span.
    # > Int
    line_dash_offset = NULL
  )
)

# Multi-select widget.
MultiSelect <- R6::R6Class("MultiSelect",
  inherit = InputWidget,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      width = NULL, size = 4L, height = NULL, disabled = FALSE, callback = NULL,
      options = list(), sizing_mode = "fixed", title = "", tags = list(),
      subscribed_events = list(), css_classes = NULL, name = NULL,
      value = list(), id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        width = width, height = height, disabled = disabled,
        sizing_mode = sizing_mode, title = title, tags = tags,
        subscribed_events = subscribed_events, css_classes = css_classes,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["MultiSelect"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The number of visible options in the dropdown list. (This uses the
    # ``select`` HTML element's ``size`` attribute. Some browsers might not
    # show less than 3 options.)
    # > Int
    size = NULL,
    # A callback to run in the browser whenever the current selection value
    # changes.
    # > Instance(Callback)
    callback = NULL,
    # Available selection options. Options may be provided either as a list
    # of possible string values, or as a list of tuples, each of the form
    # ``(value, label)``. In the latter case, the visible widget text for
    # each value will be corresponding given label.
    # > List(Either(String, Tuple(String, String)))
    options = NULL,
    # Initial or selected values.
    # > List(String)
    value = NULL
  )
)

# Generate ticks spaced apart by specific, even multiples of days.
DaysTicker <- R6::R6Class("DaysTicker",
  inherit = SingleIntervalTicker,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      num_minor_ticks = 5L, desired_num_ticks = 6L, days = list(),
      tags = list(), subscribed_events = list(), interval = NULL, name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        num_minor_ticks = num_minor_ticks,
        desired_num_ticks = desired_num_ticks, tags = tags,
        subscribed_events = subscribed_events, interval = interval,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["DaysTicker"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The intervals of days to use.
    # > Seq(Int)
    days = NULL
  )
)

# With the EdgesAndLinkedNodes policy, inspection or selection of graph
# edges will result in the inspection or selection of the edge and of the
# linked graph nodes. There is no direct selection or inspection of graph
# nodes.
EdgesAndLinkedNodes <- R6::R6Class("EdgesAndLinkedNodes",
  inherit = GraphHitTestPolicy,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["EdgesAndLinkedNodes"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# A GroupFilter represents the rows of a ColumnDataSource where the
# values of the categorical column column_name match the group variable.
GroupFilter <- R6::R6Class("GroupFilter",
  inherit = Filter,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      filter = NULL, column_name = NULL, group = NULL, tags = list(),
      subscribed_events = list(), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        filter = filter, tags = tags, subscribed_events = subscribed_events,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["GroupFilter"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The name of the column to perform the group filtering operation on.
    # > String
    column_name = NULL,
    # The value of the column indicating the rows of data to keep.
    # > String
    group = NULL
  )
)

# Render square markers with an 'X' cross through the center.
# 
# Example -------
# 
# .. bokeh-plot:: ../examples/reference/models/SquareX.py
# :source-position: below
SquareX <- R6::R6Class("SquareX",
  inherit = Marker,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      line_join = "miter", x = NULL, size = 4L, line_width = 1L,
      line_cap = "butt", fill_alpha = 1, y = NULL, angle = 0, line_dash = list(),
      fill_color = "gray", line_color = "black", tags = list(),
      subscribed_events = list(), line_alpha = 1, angle_units = "rad",
      name = NULL, line_dash_offset = 0L, id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks,
        line_join = line_join, x = x, size = size, line_width = line_width,
        line_cap = line_cap, fill_alpha = fill_alpha, y = y, angle = angle,
        line_dash = line_dash, fill_color = fill_color,
        line_color = line_color, tags = tags,
        subscribed_events = subscribed_events, line_alpha = line_alpha,
        angle_units = angle_units, name = name,
        line_dash_offset = line_dash_offset, id = id)
      types <- bk_prop_types[["SquareX"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# An expression for generating arrays by summing different columns from a
# ``ColumnDataSource``.
# 
# This expression is useful for implementing stacked bar charts at a low
# level.
Stack <- R6::R6Class("Stack",
  inherit = Expression,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      tags = list(), subscribed_events = list(), fields = NULL, name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Stack"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # A sequence of fields from a ``ColumnDataSource`` to sum (elementwise).
    # For example:
    # 
    # .. code-block:: python
    # 
    # Stack(fields=['sales', 'marketing'])
    # 
    # Will compute an array of values (in the browser) by adding the elements
    # of the ``'sales'`` and ``'marketing'`` columns of a data source.
    # > Seq(String)
    fields = NULL
  )
)

# Base class for arrow heads.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
ArrowHead <- R6::R6Class("ArrowHead",
  inherit = Annotation,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      visible = TRUE, name = NULL, plot = NULL, tags = list(),
      subscribed_events = list(), level = "annotation",
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        visible = visible, name = name, plot = plot, tags = tags,
        subscribed_events = subscribed_events, level = level,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["ArrowHead"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# *toolbar icon*: |box_zoom_icon|
# 
# The box zoom tool allows users to define a rectangular region of a Plot
# to zoom to by dragging he mouse or a finger over the plot region. The
# end of the drag event indicates the selection region is ready.
# 
# .. |box_zoom_icon| image:: /_images/icons/BoxZoom.png :height: 18pt
BoxZoomTool <- R6::R6Class("BoxZoomTool",
  inherit = Drag,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      dimensions = "both", subscribed_events = list(), match_aspect = FALSE,
      tags = list(), overlay = NULL, name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["BoxZoomTool"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Which dimensions the zoom box is to be free in. By default, users may
    # freely draw zoom boxes with any dimensions. If only "width" is
    # supplied, the box will be constrained to span the entire vertical space
    # of the plot, only the horizontal dimension can be controlled. If only
    # "height" is supplied, the box will be constrained to span the entire
    # horizontal space of the plot, and the vertical dimension can be
    # controlled.
    # > Enum('width', 'height', 'both')
    dimensions = NULL,
    # Whether the box zoom region should be restricted to have the same
    # aspect ratio as the plot region.
    # 
    # .. note:: If the tool is restricted to one dimension, this value has no
    # effect.
    # > Bool
    match_aspect = NULL,
    # A shaded annotation drawn to indicate the selection region.
    # > Instance(BoxAnnotation)
    overlay = NULL
  )
)

# Number cell formatter.
NumberFormatter <- R6::R6Class("NumberFormatter",
  inherit = StringFormatter,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      language = "en", text_color = NULL, font_style = "normal",
      rounding = "round", tags = list(), format = "0,0",
      subscribed_events = list(), text_align = "left", name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        text_color = text_color, font_style = font_style, tags = tags,
        subscribed_events = subscribed_events, text_align = text_align,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["NumberFormatter"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The language to use for formatting language-specific features (e.g.
    # thousands separator).
    # > Enum('be-nl', 'chs', 'cs', 'da-dk', 'de-ch', 'de', 'en', 'en-gb', 'es-ES', 'es', 'et', 'fi', 'fr-CA', 'fr-ch', 'fr', 'hu', 'it', 'ja', 'nl-nl', 'pl', 'pt-br', 'pt-pt', 'ru', 'ru-UA', 'sk', 'th', 'tr', 'uk-UA')
    language = NULL,
    # Rounding functions (round, floor, ceil) and their synonyms (nearest,
    # rounddown, roundup).
    # > Enum('round', 'nearest', 'floor', 'rounddown', 'ceil', 'roundup')
    rounding = NULL,
    # The number format, as defined in the following tables:
    # 
    # **NUMBERS**:
    # 
    # ============ ============== =============== Number Format String
    # ============ ============== =============== 10000 '0,0.0000'
    # 10,000.0000 10000.23 '0,0' 10,000 10000.23 '+0,0' +10,000 -10000
    # '0,0.0' -10,000.0 10000.1234 '0.000' 10000.123 10000.1234 '0[.]00000'
    # 10000.12340 -10000 '(0,0.0000)' (10,000.0000) -0.23 '.00' -.23 -0.23
    # '(.00)' (.23) 0.23 '0.00000' 0.23000 0.23 '0.0[0000]' 0.23 1230974
    # '0.0a' 1.2m 1460 '0 a' 1 k -104000 '0a' -104k 1 '0o' 1st 52 '0o' 52nd
    # 23 '0o' 23rd 100 '0o' 100th ============ ============== ===============
    # 
    # **CURRENCY**:
    # 
    # =========== =============== ============= Number Format String
    # =========== =============== ============= 1000.234 '$0,0.00' $1,000.23
    # 1000.2 '0,0[.]00 $' 1,000.20 $ 1001 '$ 0,0[.]00' $ 1,001 -1000.234
    # '($0,0)' ($1,000) -1000.234 '$0.00' -$1000.23 1230974 '($ 0.00 a)' $
    # 1.23 m =========== =============== =============
    # 
    # **BYTES**:
    # 
    # =============== =========== ============ Number Format String
    # =============== =========== ============ 100 '0b' 100B 2048 '0 b' 2 KB
    # 7884486213 '0.0b' 7.3GB 3467479682787 '0.000 b' 3.154 TB
    # =============== =========== ============
    # 
    # **PERCENTAGES**:
    # 
    # ============= ============= =========== Number Format String
    # ============= ============= =========== 1 '0%' 100% 0.974878234
    # '0.000%' 97.488% -0.43 '0 %' -43 % 0.43 '(0.000 %)' 43.000 %
    # ============= ============= ===========
    # 
    # **TIME**:
    # 
    # ============ ============== ============ Number Format String
    # ============ ============== ============ 25 '00:00:00' 0:00:25 238
    # '00:00:00' 0:03:58 63846 '00:00:00' 17:44:06 ============
    # ============== ============
    # 
    # For the complete specification, see http://numbrojs.com/format.html
    # > String
    format = NULL
  )
)

# Render circle markers with an 'X' cross through the center.
# 
# Example -------
# 
# .. bokeh-plot:: ../examples/reference/models/CircleX.py
# :source-position: below
CircleX <- R6::R6Class("CircleX",
  inherit = Marker,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      line_join = "miter", x = NULL, size = 4L, line_width = 1L,
      line_cap = "butt", fill_alpha = 1, y = NULL, angle = 0, line_dash = list(),
      fill_color = "gray", line_color = "black", tags = list(),
      subscribed_events = list(), line_alpha = 1, angle_units = "rad",
      name = NULL, line_dash_offset = 0L, id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks,
        line_join = line_join, x = x, size = size, line_width = line_width,
        line_cap = line_cap, fill_alpha = fill_alpha, y = y, angle = angle,
        line_dash = line_dash, fill_color = fill_color,
        line_color = line_color, tags = tags,
        subscribed_events = subscribed_events, line_alpha = line_alpha,
        angle_units = angle_units, name = name,
        line_dash_offset = line_dash_offset, id = id)
      types <- bk_prop_types[["CircleX"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Single-select widget.
Select <- R6::R6Class("Select",
  inherit = InputWidget,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      width = NULL, height = NULL, disabled = FALSE, callback = NULL,
      options = list(), sizing_mode = "fixed", title = "", tags = list(),
      subscribed_events = list(), css_classes = NULL, name = NULL, value = "",
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        width = width, height = height, disabled = disabled,
        sizing_mode = sizing_mode, title = title, tags = tags,
        subscribed_events = subscribed_events, css_classes = css_classes,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["Select"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # A callback to run in the browser whenever the current Select dropdown
    # value changes.
    # > Instance(Callback)
    callback = NULL,
    # Available selection options. Options may be provided either as a list
    # of possible string values, or as a list of tuples, each of the form
    # ``(value, label)``. In the latter case, the visible widget text for
    # each value will be corresponding given label.
    # > List(Either(String, Tuple(String, String)))
    options = NULL,
    # Initial or selected value.
    # > String
    value = NULL
  )
)

# A BooleanFilter filters data by returning the subset of data
# corresponding to indices where the values of the booleans array is
# True.
BooleanFilter <- R6::R6Class("BooleanFilter",
  inherit = Filter,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      filter = NULL, subscribed_events = list(), tags = list(),
      booleans = NULL, name = NULL, js_property_callbacks = structure(list(),
      .Names = character(0)), id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        filter = filter, tags = tags, subscribed_events = subscribed_events,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["BooleanFilter"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # A list of booleans indicating which rows of data to select.
    # > Seq(Bool)
    booleans = NULL
  )
)

# A base class for data source types, which can be mapped onto a columnar
# format.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
ColumnarDataSource <- R6::R6Class("ColumnarDataSource",
  inherit = DataSource,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      column_names = list(), callback = NULL, tags = list(),
      subscribed_events = list(), selected = structure(list(`0d` =
      structure(list(indices = list(), glyph = NULL), .Names =
      c("indices", "glyph")), `2d` = structure(list(indices =
      structure(list(), .Names = character(0))), .Names = "indices"),
      `1d` = structure(list(indices = list()), .Names = "indices")),
      .Names = c("0d", "2d", "1d")), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(callback = callback,
        js_event_callbacks = js_event_callbacks, tags = tags,
        subscribed_events = subscribed_events, selected = selected,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["ColumnarDataSource"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # An list of names for all the columns in this DataSource.
    # > List(String)
    column_names = NULL
  )
)

# A base class for all tick formatter types.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
TickFormatter <- R6::R6Class("TickFormatter",
  inherit = Model,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["TickFormatter"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# *toolbar icon*: |wheel_zoom_icon|
# 
# The wheel zoom tool will zoom the plot in and out, centered on the
# current mouse location.
# 
# The wheel zoom tool also activates the border regions of a Plot for
# "single axis" zooming. For instance, zooming in the vertical border or
# axis will effect a zoom in the vertical direction only, with the
# horizontal dimension kept fixed.
# 
# .. |wheel_zoom_icon| image:: /_images/icons/WheelZoom.png :height: 18pt
WheelZoomTool <- R6::R6Class("WheelZoomTool",
  inherit = Scroll,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      tags = list(), dimensions = "both", subscribed_events = list(),
      name = NULL, js_property_callbacks = structure(list(), .Names =
      character(0)), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["WheelZoomTool"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Which dimensions the wheel zoom tool is constrained to act in. By
    # default the wheel zoom tool will zoom in any dimension, but can be
    # configured to only zoom horizontally across the width of the plot, or
    # vertically across the height of the plot.
    # > Enum('width', 'height', 'both')
    dimensions = NULL
  )
)

# Render a 'X' cross markers.
# 
# Example -------
# 
# .. bokeh-plot:: ../examples/reference/models/X.py :source-position:
# below
X <- R6::R6Class("X",
  inherit = Marker,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      line_join = "miter", x = NULL, size = 4L, line_width = 1L,
      line_cap = "butt", fill_alpha = 1, y = NULL, angle = 0, line_dash = list(),
      fill_color = "gray", line_color = "black", tags = list(),
      subscribed_events = list(), line_alpha = 1, angle_units = "rad",
      name = NULL, line_dash_offset = 0L, id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks,
        line_join = line_join, x = x, size = size, line_width = line_width,
        line_cap = line_cap, fill_alpha = fill_alpha, y = y, angle = angle,
        line_dash = line_dash, fill_color = fill_color,
        line_color = line_color, tags = tags,
        subscribed_events = subscribed_events, line_alpha = line_alpha,
        angle_units = angle_units, name = name,
        line_dash_offset = line_dash_offset, id = id)
      types <- bk_prop_types[["X"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Abstract base class for input widgets.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
InputWidget <- R6::R6Class("InputWidget",
  inherit = Widget,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      width = NULL, height = NULL, disabled = FALSE, sizing_mode = "fixed",
      title = "", tags = list(), subscribed_events = list(),
      css_classes = NULL, name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        width = width, height = height, disabled = disabled,
        sizing_mode = sizing_mode, tags = tags,
        subscribed_events = subscribed_events, css_classes = css_classes,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["InputWidget"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Widget's label.
    # > String
    title = NULL
  )
)

# Generate ticks for categorical ranges.
CategoricalTicker <- R6::R6Class("CategoricalTicker",
  inherit = Ticker,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["CategoricalTicker"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# *toolbar icon*: |save_icon|
# 
# The save tool is an action. When activated, the tool opens a download
# dialog which allows to save an image reproduction of the plot in PNG
# format. If automatic download is not support by a web browser, the tool
# falls back to opening the generated image in a new tab or window. User
# then can manually save it by right clicking on the image and choosing
# "Save As" (or similar) menu item.
# 
# .. |save_icon| image:: /_images/icons/Save.png :height: 18pt
SaveTool <- R6::R6Class("SaveTool",
  inherit = Action,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["SaveTool"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# A block (div) of text.
# 
# This Bokeh model corresponds to an HTML ``<div>`` element.
# 
# Example -------
# 
# .. bokeh-plot::
# ../sphinx/source/docs/user_guide/examples/interaction_div.py
# :source-position: below
Div <- R6::R6Class("Div",
  inherit = Markup,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      text = "", width = NULL, height = NULL, disabled = FALSE,
      render_as_text = FALSE, sizing_mode = "fixed", tags = list(),
      subscribed_events = list(), css_classes = NULL,
      style = structure(list(), .Names = character(0)), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        text = text, width = width, height = height, disabled = disabled,
        sizing_mode = sizing_mode, tags = tags,
        subscribed_events = subscribed_events, css_classes = css_classes,
        style = style, name = name,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["Div"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Whether the contents should be rendered as raw text or as interpreted
    # HTML.  The default value is ``False``, meaning contents are rendered as
    # HTML.
    # > Bool
    render_as_text = NULL
  )
)

# Render images loaded from given URLs.
# 
# Example -------
# 
# .. bokeh-plot:: ../examples/reference/models/ImageURL.py
# :source-position: below
ImageURL <- R6::R6Class("ImageURL",
  inherit = XYGlyph,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      h_units = "data", js_property_callbacks = structure(list(), .Names =
      character(0)), w = NULL, h = NULL, x = NULL, w_units = "data",
      dilate = FALSE, y = NULL, angle = 0L, retry_timeout = 0L,
      retry_attempts = 0L, tags = list(), global_alpha = 1, url = NULL,
      subscribed_events = list(), angle_units = "rad", name = NULL,
      anchor = "top_left", id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["ImageURL"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # 
    # > Enum('screen', 'data')
    h_units = NULL,
    # The height of the plot region that the image will occupy in data space.
    # 
    # The default value is ``None``, in which case the image will be
    # displayed at its actual image size (regardless of the units specified
    # here).
    # > DistanceSpec(units_default='data')
    w = NULL,
    # The height of the plot region that the image will occupy in data space.
    # 
    # The default value is ``None``, in which case the image will be
    # displayed at its actual image size (regardless of the units specified
    # here).
    # > DistanceSpec(units_default='data')
    h = NULL,
    # The x-coordinates to locate the image anchors.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    x = NULL,
    # 
    # > Enum('screen', 'data')
    w_units = NULL,
    # Whether to always round fractional pixel locations in such a way as to
    # make the images bigger.
    # 
    # This setting may be useful if pixel rounding errors are causing images
    # to have a gap between them, when they should appear flush.
    # > Bool
    dilate = NULL,
    # The y-coordinates to locate the image anchors.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    y = NULL,
    # The angles to rotate the images, as measured from the horizontal.
    # > AngleSpec(units_default='rad')
    angle = NULL,
    # Timeout (in ms) between retry attempts to load the image from the
    # specified URL. Default is zero ms.
    # > Int
    retry_timeout = NULL,
    # Number of attempts to retry loading the images from the specified URL.
    # Default is zero.
    # > Int
    retry_attempts = NULL,
    # An overall opacity that each image is rendered with (in addition to any
    # inherent alpha values in the image itself).
    # > Float
    global_alpha = NULL,
    # The URLs to retrieve images from.
    # 
    # .. note:: The actual retrieving and loading of the images happens on
    # the client.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    url = NULL,
    # 
    # > Enum('deg', 'rad')
    angle_units = NULL,
    # What position of the image should be anchored at the `x`, `y`
    # coordinates.
    # > Enum('top_left', 'top_center', 'top_right', 'center_left', 'center', 'center_right', 'bottom_left', 'bottom_center', 'bottom_right')
    anchor = NULL
  )
)

# A ``TickFormatter`` for displaying datetime values nicely across a
# range of scales.
# 
# ``DatetimeTickFormatter`` has the following properties (listed together
# with their default values) that can be used to control the formatting
# of axis ticks at different scales scales:
# 
# .. code-block:: python
# 
# microseconds = ['%fus']
# 
# milliseconds = ['%3Nms', '%S.%3Ns']
# 
# seconds = ['%Ss']
# 
# minsec = [':%M:%S']
# 
# minutes = [':%M', '%Mm']
# 
# hourmin = ['%H:%M']
# 
# hours = ['%Hh', '%H:%M']
# 
# days = ['%m/%d', '%a%d']
# 
# months = ['%m/%Y', '%b%y']
# 
# years = ['%Y']
# 
# Each scale property can be set to format or list of formats to use for
# formatting datetime tick values that fall in in that "time scale".  By
# default, only the first format string passed for each time scale will
# be used. By default, all leading zeros are stripped away from the
# formatted labels.
# 
# This list of supported `strftime`_ formats is reproduced below.
# 
# %a The abbreviated name of the day of the week according to the current
# locale.
# 
# %A The full name of the day of the week according to the current
# locale.
# 
# %b The abbreviated month name according to the current locale.
# 
# %B The full month name according to the current locale.
# 
# %c The preferred date and time representation for the current locale.
# 
# %C The century number (year/100) as a 2-digit integer.
# 
# %d The day of the month as a decimal number (range 01 to 31).
# 
# %D Equivalent to %m/%d/%y.  (Americans should note that in many other
# countries %d/%m/%y is rather common. This means that in international
# context this format is ambiguous and should not be used.)
# 
# %e Like %d, the day of the month as a decimal number, but a leading
# zero is replaced by a space.
# 
# %f Microsecond as a decimal number, zero-padded on the left (range
# 000000-999999). This is an extension to the set of directives available
# to `timezone`_.
# 
# %F Equivalent to %Y-%m-%d (the ISO 8601 date format).
# 
# %G The ISO 8601 week-based year with century as a decimal number.  The
# 4-digit year corresponding to the ISO week number (see %V).  This has
# the same format and value as %Y, except that if the ISO week number
# belongs to the previous or next year, that year is used instead.
# 
# %g Like %G, but without century, that is, with a 2-digit year (00-99).
# 
# %h Equivalent to %b.
# 
# %H The hour as a decimal number using a 24-hour clock (range 00 to 23).
# 
# %I The hour as a decimal number using a 12-hour clock (range 01 to 12).
# 
# %j The day of the year as a decimal number (range 001 to 366).
# 
# %k The hour (24-hour clock) as a decimal number (range 0 to 23).
# Single digits are preceded by a blank.  (See also %H.)
# 
# %l The hour (12-hour clock) as a decimal number (range 1 to 12).
# Single digits are preceded by a blank.  (See also %I.)  (TZ)
# 
# %m The month as a decimal number (range 01 to 12).
# 
# %M The minute as a decimal number (range 00 to 59).
# 
# %n A newline character. Bokeh text does not currently support newline
# characters.
# 
# %N Nanosecond as a decimal number, zero-padded on the left (range
# 000000000-999999999). Supports a padding width specifier, i.e.  %3N
# displays 3 leftmost digits. However, this is only accurate to the
# millisecond level of precision due to limitations of `timezone`_.
# 
# %p Either "AM" or "PM" according to the given time value, or the
# corresponding strings for the current locale.  Noon is treated as "PM"
# and midnight as "AM".
# 
# %P Like %p but in lowercase: "am" or "pm" or a corresponding string for
# the current locale.
# 
# %r The time in a.m. or p.m. notation.  In the POSIX locale this is
# equivalent to %I:%M:%S %p.
# 
# %R The time in 24-hour notation (%H:%M). For a version including the
# seconds, see %T below.
# 
# %s The number of seconds since the Epoch, 1970-01-01 00:00:00 +0000
# (UTC).
# 
# %S The second as a decimal number (range 00 to 60).  (The range is up
# to 60 to allow for occasional leap seconds.)
# 
# %t A tab character. Bokeh text does not currently support tab
# characters.
# 
# %T The time in 24-hour notation (%H:%M:%S).
# 
# %u The day of the week as a decimal, range 1 to 7, Monday being 1.  See
# also %w.
# 
# %U The week number of the current year as a decimal number, range 00 to
# 53, starting with the first Sunday as the first day of week 01.  See
# also %V and %W.
# 
# %V The ISO 8601 week number (see NOTES) of the current year as a
# decimal number, range 01 to 53, where week 1 is the first week that has
# at least 4 days in the new year.  See also %U and %W.
# 
# %w The day of the week as a decimal, range 0 to 6, Sunday being 0.  See
# also %u.
# 
# %W The week number of the current year as a decimal number, range 00 to
# 53, starting with the first Monday as the first day of week 01.
# 
# %x The preferred date representation for the current locale without the
# time.
# 
# %X The preferred time representation for the current locale without the
# date.
# 
# %y The year as a decimal number without a century (range 00 to 99).
# 
# %Y The year as a decimal number including the century.
# 
# %z The +hhmm or -hhmm numeric timezone (that is, the hour and minute
# offset from UTC).
# 
# %Z The timezone name or abbreviation.
# 
# %% A literal '%' character.
# 
# .. warning:: The client library BokehJS uses the `timezone`_ library to
# format datetimes. The inclusion of the list below is based on the claim
# that `timezone`_ makes to support "the full compliment of GNU date
# format specifiers." However, this claim has not been tested
# exhaustively against this list. If you find formats that do not
# function as expected, please submit a `github issue`_, so that the
# documentation can be updated appropriately.
# 
# .. _strftime: http://man7.org/linux/man-pages/man3/strftime.3.html ..
# _timezone: http://bigeasy.github.io/timezone/ .. _github issue:
# https://github.com/bokeh/bokeh/issues
DatetimeTickFormatter <- R6::R6Class("DatetimeTickFormatter",
  inherit = TickFormatter,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      minsec = list(":%M:%S"), years = list("%Y"),
      microseconds = list("%fus"), hours = list("%Hh", "%H:%M"),
      months = list("%m/%Y", "%b%y"), days = list("%m/%d", "%a%d"),
      minutes = list(":%M", "%Mm"), name = NULL, tags = list(),
      subscribed_events = list(), hourmin = list("%H:%M"),
      seconds = list("%Ss"), milliseconds = list("%3Nms", "%S.%3Ns"),
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["DatetimeTickFormatter"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Formats for displaying datetime values in the ``minsec`` (for combined
    # minutes and seconds) range.
    # 
    # See the :class:`~bokeh.models.formatters.DatetimeTickFormatter` help
    # for a list of all supported formats.
    # > List(String)
    minsec = NULL,
    # Formats for displaying datetime values in the ``years`` range.
    # 
    # See the :class:`~bokeh.models.formatters.DatetimeTickFormatter` help
    # for a list of all supported formats.
    # > List(String)
    years = NULL,
    # Formats for displaying datetime values in the ``microseconds`` range.
    # 
    # See the :class:`~bokeh.models.formatters.DatetimeTickFormatter` help
    # for a list of all supported formats.
    # > List(String)
    microseconds = NULL,
    # Formats for displaying datetime values in the ``hours`` range.
    # 
    # See the :class:`~bokeh.models.formatters.DatetimeTickFormatter` help
    # for a list of all supported formats.
    # > List(String)
    hours = NULL,
    # Formats for displaying datetime values in the ``months`` range.
    # 
    # See the :class:`~bokeh.models.formatters.DatetimeTickFormatter` help
    # for a list of all supported formats.
    # > List(String)
    months = NULL,
    # Formats for displaying datetime values in the ``days`` range.
    # 
    # See the :class:`~bokeh.models.formatters.DatetimeTickFormatter` help
    # for a list of all supported formats.
    # > List(String)
    days = NULL,
    # Formats for displaying datetime values in the ``minutes`` range.
    # 
    # See the :class:`~bokeh.models.formatters.DatetimeTickFormatter` help
    # for a list of all supported formats.
    # > List(String)
    minutes = NULL,
    # Formats for displaying datetime values in the ``hourmin`` (for combined
    # hours and minutes) range.
    # 
    # See the :class:`~bokeh.models.formatters.DatetimeTickFormatter` help
    # for a list of all supported formats.
    # > List(String)
    hourmin = NULL,
    # Formats for displaying datetime values in the ``seconds`` range.
    # 
    # See the :class:`~bokeh.models.formatters.DatetimeTickFormatter` help
    # for a list of all supported formats.
    # > List(String)
    seconds = NULL,
    # Formats for displaying datetime values in the ``milliseconds`` range.
    # 
    # See the :class:`~bokeh.models.formatters.DatetimeTickFormatter` help
    # for a list of all supported formats.
    # > List(String)
    milliseconds = NULL
  )
)

# Render circle markers.
# 
# Example -------
# 
# .. bokeh-plot:: ../examples/reference/models/Circle.py
# :source-position: below
Circle <- R6::R6Class("Circle",
  inherit = Marker,
  public = list(
    specified_args = NULL,
    initialize = function(
      radius_units = "data", size = 4L, line_cap = "butt", fill_alpha = 1,
      angle = 0, radius = NULL, subscribed_events = list(), line_alpha = 1,
      angle_units = "rad", line_dash_offset = 0L,
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      name = NULL, line_join = "miter", x = NULL, line_width = 1L, y = NULL,
      line_dash = list(), fill_color = "gray", line_color = "black",
      tags = list(), radius_dimension = "x", id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks,
        line_join = line_join, x = x, size = size, line_width = line_width,
        line_cap = line_cap, fill_alpha = fill_alpha, y = y, angle = angle,
        line_dash = line_dash, fill_color = fill_color,
        line_color = line_color, tags = tags,
        subscribed_events = subscribed_events, line_alpha = line_alpha,
        angle_units = angle_units, name = name,
        line_dash_offset = line_dash_offset, id = id)
      types <- bk_prop_types[["Circle"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # 
    # > Enum('screen', 'data')
    radius_units = NULL,
    # The radius values for circle markers (in "data space" units, by
    # default).
    # 
    # .. note:: Circle markers are slightly unusual in that they support
    # specifying a radius in addition to a size. Only one of ``radius`` or
    # ``size`` should be given.
    # 
    # .. warning:: Note that ``Circle`` glyphs are always drawn as circles on
    # the screen, even in cases where the data space aspect ratio is not 1-1.
    # In all cases where radius values are specified, the "distance" for the
    # radius is measured along the dimension specified by
    # ``radius_dimension``. If the aspect ratio is very large or small, the
    # drawn circles may appear much larger or smaller than expected. See
    # :bokeh-issue:`626` for more information.
    # > DistanceSpec(units_default='data')
    radius = NULL,
    # What dimension to measure circle radii along.
    # 
    # When the data space aspect ratio is not 1-1, then the size of the drawn
    # circles depends on what direction is used to measure the "distance" of
    # the radius. This property allows that direction to be controlled.
    # > Enum('x', 'y')
    radius_dimension = NULL
  )
)

# Render an arrow as an annotation.
Arrow <- R6::R6Class("Arrow",
  inherit = Annotation,
  public = list(
    specified_args = NULL,
    initialize = function(
      x_end = NULL, end = NULL, line_cap = "butt", x_range_name = "default",
      y_range_name = "default", subscribed_events = list(), line_alpha = 1,
      start = NULL, y_start = NULL, line_dash_offset = 0L,
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      name = NULL, visible = TRUE, line_join = "miter", line_width = 1L,
      source = NULL, line_dash = list(), y_end = NULL, plot = NULL,
      line_color = "black", tags = list(), start_units = "data",
      end_units = "data", x_start = NULL, level = "annotation", id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        visible = visible, name = name, plot = plot, tags = tags,
        subscribed_events = subscribed_events, level = level,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["Arrow"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The x-coordinates to locate the end of the arrows.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    x_end = NULL,
    # Instance of ArrowHead.
    # > Instance(ArrowHead)
    end = NULL,
    # The line cap values for the arrow body.
    # > Enum('butt', 'round', 'square')
    line_cap = NULL,
    # A particular (named) x-range to use for computing screen locations when
    # rendering annotations on the plot. If unset, use the default x-range.
    # > String
    x_range_name = NULL,
    # A particular (named) y-range to use for computing screen locations when
    # rendering annotations on the plot. If unset, use the default y-range.
    # > String
    y_range_name = NULL,
    # The line alpha values for the arrow body.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_alpha = NULL,
    # Instance of ArrowHead.
    # > Instance(ArrowHead)
    start = NULL,
    # The y-coordinates to locate the start of the arrows.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    y_start = NULL,
    # The line dash offset values for the arrow body.
    # > Int
    line_dash_offset = NULL,
    # The line join values for the arrow body.
    # > Enum('miter', 'round', 'bevel')
    line_join = NULL,
    # The line width values for the arrow body.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_width = NULL,
    # Local data source to use when rendering annotations on the plot.
    # > Instance(DataSource)
    source = NULL,
    # The line dash values for the arrow body.
    # > DashPattern
    line_dash = NULL,
    # The y-coordinates to locate the end of the arrows.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    y_end = NULL,
    # The line color values for the arrow body.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    line_color = NULL,
    # The unit type for the start_x and start_y attributes. Interpreted as
    # "data space" units by default.
    # > Enum('screen', 'data')
    start_units = NULL,
    # The unit type for the end_x and end_y attributes. Interpreted as "data
    # space" units by default.
    # > Enum('screen', 'data')
    end_units = NULL,
    # The x-coordinates to locate the start of the arrows.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    x_start = NULL
  )
)

# Render a tooltip.
# 
# .. note:: This model is currently managed by BokehJS and is not useful
# directly from python.
Tooltip <- R6::R6Class("Tooltip",
  inherit = Annotation,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      visible = TRUE, name = NULL, attachment = "horizontal",
      inner_only = TRUE, plot = NULL, tags = list(),
      subscribed_events = list(), show_arrow = TRUE, level = "overlay",
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        visible = visible, name = name, plot = plot, tags = tags,
        subscribed_events = subscribed_events, level = level,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["Tooltip"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Whether the tooltip should display to the left or right off the cursor
    # position or above or below it, or if it should be automatically placed
    # in the horizontal or vertical dimension.
    # > Enum('horizontal', 'vertical', 'left', 'right', 'above', 'below')
    attachment = NULL,
    # Whether to display outside a central plot frame area.
    # > Bool
    inner_only = NULL,
    # Whether tooltip's arrow should be showed.
    # > Bool
    show_arrow = NULL
  )
)

# Abstract base class for all kinds of groups.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
AbstractGroup <- R6::R6Class("AbstractGroup",
  inherit = Widget,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      width = NULL, height = NULL, disabled = FALSE, callback = NULL,
      sizing_mode = "fixed", subscribed_events = list(), tags = list(),
      labels = list(), css_classes = NULL, name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        width = width, height = height, disabled = disabled,
        sizing_mode = sizing_mode, tags = tags,
        subscribed_events = subscribed_events, css_classes = css_classes,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["AbstractGroup"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # A callback to run in the browser whenever a button group is
    # manipulated.
    # > Instance(Callback)
    callback = NULL,
    # List of text labels contained in this group.
    # > List(String)
    labels = NULL
  )
)

# A base class for tools that perform "inspections", e.g. ``HoverTool``.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
Inspection <- R6::R6Class("Inspection",
  inherit = Tool,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      toggleable = TRUE, tags = list(), subscribed_events = list(),
      name = NULL, js_property_callbacks = structure(list(), .Names =
      character(0)), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Inspection"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Whether an on/off toggle button should appear in the toolbar for this
    # inpection tool. If ``False``, the viewers of a plot will not be able to
    # toggle the inspector on or off using the toolbar.
    # > Bool
    toggleable = NULL
  )
)

# A base class for all interactive widget types.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
Widget <- R6::R6Class("Widget",
  inherit = LayoutDOM,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      width = NULL, height = NULL, disabled = FALSE, sizing_mode = "fixed",
      tags = list(), subscribed_events = list(), css_classes = NULL,
      name = NULL, js_property_callbacks = structure(list(), .Names =
      character(0)), id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        width = width, height = height, disabled = disabled,
        sizing_mode = sizing_mode, tags = tags,
        subscribed_events = subscribed_events, css_classes = css_classes,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["Widget"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Render several patches.
# 
# The data for the ``Patches`` glyph is different in that the vector of
# values is not a vector of scalars. Rather, it is a "list of lists".
# 
# Example -------
# 
# .. bokeh-plot:: ../examples/reference/models/Patches.py
# :source-position: below
Patches <- R6::R6Class("Patches",
  inherit = Glyph,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      line_join = "miter", xs = NULL, line_width = 1L, line_cap = "butt",
      fill_alpha = 1, tags = list(), line_dash = list(), ys = NULL,
      line_color = "black", fill_color = "gray", subscribed_events = list(),
      line_alpha = 1, name = NULL, line_dash_offset = 0L, id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Patches"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The line join values for the patches.
    # > Enum('miter', 'round', 'bevel')
    line_join = NULL,
    # The x-coordinates for all the patches, given as a "list of lists".
    # 
    # .. note:: Individual patches may comprise multiple polygons. In this
    # case the x-coordinates for each polygon should be separated by NaN
    # values in the sublists.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    xs = NULL,
    # The line width values for the patches.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_width = NULL,
    # The line cap values for the patches.
    # > Enum('butt', 'round', 'square')
    line_cap = NULL,
    # The fill alpha values for the patches.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    fill_alpha = NULL,
    # The line dash values for the patches.
    # > DashPattern
    line_dash = NULL,
    # The y-coordinates for all the patches, given as a "list of lists".
    # 
    # .. note:: Individual patches may comprise multiple polygons. In this
    # case the y-coordinates for each polygon should be separated by NaN
    # values in the sublists.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    ys = NULL,
    # The line color values for the patches.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    line_color = NULL,
    # The fill color values for the patches.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    fill_color = NULL,
    # The line alpha values for the patches.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_alpha = NULL,
    # The line dash offset values for the patches.
    # > Int
    line_dash_offset = NULL
  )
)

# Abstract base class for data table (data grid) widgets.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
TableWidget <- R6::R6Class("TableWidget",
  inherit = Widget,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      view = NULL, width = NULL, height = NULL, source = NULL, disabled = FALSE,
      sizing_mode = "fixed", tags = list(), subscribed_events = list(),
      css_classes = NULL, name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        width = width, height = height, disabled = disabled,
        sizing_mode = sizing_mode, tags = tags,
        subscribed_events = subscribed_events, css_classes = css_classes,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["TableWidget"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # A view into the data source to use when rendering table rows. A default
    # view of the entire data source is created if a view is not passed in
    # during initialization.
    # > Instance(CDSView)
    view = NULL,
    # The source of data for the widget.
    # > Instance(DataSource)
    source = NULL
  )
)

# A base class for tools that respond to drag events.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
Drag <- R6::R6Class("Drag",
  inherit = Tool,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Drag"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Single-line password input widget.  Note: Despite PasswordInput
# inheriting from TextInput the password cannot be inspected on the field
# ``value``. Also, note that this field functionally just hides the input
# on the browser, transmiting safely a password as a callback, e.g., to
# the a bokeh server would require some secure connection.
PasswordInput <- R6::R6Class("PasswordInput",
  inherit = TextInput,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      width = NULL, height = NULL, disabled = FALSE, callback = NULL,
      tags = list(), sizing_mode = "fixed", title = "", placeholder = "",
      subscribed_events = list(), css_classes = NULL, name = NULL, value = "",
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, width = width,
        height = height, disabled = disabled, callback = callback, tags = tags,
        sizing_mode = sizing_mode, title = title, placeholder = placeholder,
        subscribed_events = subscribed_events, css_classes = css_classes,
        name = name, value = value, id = id)
      types <- bk_prop_types[["PasswordInput"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Base class for interactive callback.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
Callback <- R6::R6Class("Callback",
  inherit = Model,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Callback"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Render rectangles.
# 
# Example -------
# 
# .. bokeh-plot:: ../examples/reference/models/Rect.py :source-position:
# below
Rect <- R6::R6Class("Rect",
  inherit = XYGlyph,
  public = list(
    specified_args = NULL,
    initialize = function(
      dilate = FALSE, line_cap = "butt", fill_alpha = 1, angle = 0,
      width_units = "data", subscribed_events = list(), line_alpha = 1,
      angle_units = "rad", line_dash_offset = 0L,
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      name = NULL, line_join = "miter", width = NULL, x = NULL,
      height_units = "data", line_width = 1L, height = NULL, y = NULL,
      line_dash = list(), fill_color = "gray", line_color = "black",
      tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Rect"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Whether to always round fractional pixel locations in such a way as to
    # make the rectangles bigger.
    # 
    # This setting may be useful if pixel rounding errors are causing
    # rectangles to have a gap between them, when they should appear flush.
    # > Bool
    dilate = NULL,
    # The line cap values for the rectangles.
    # > Enum('butt', 'round', 'square')
    line_cap = NULL,
    # The fill alpha values for the rectangles.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    fill_alpha = NULL,
    # The angles to rotate the rectangles, as measured from the horizontal.
    # > AngleSpec(units_default='rad')
    angle = NULL,
    # 
    # > Enum('screen', 'data')
    width_units = NULL,
    # The line alpha values for the rectangles.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_alpha = NULL,
    # 
    # > Enum('deg', 'rad')
    angle_units = NULL,
    # The line dash offset values for the rectangles.
    # > Int
    line_dash_offset = NULL,
    # The line join values for the rectangles.
    # > Enum('miter', 'round', 'bevel')
    line_join = NULL,
    # The overall widths of the rectangles.
    # > DistanceSpec(units_default='data')
    width = NULL,
    # The x-coordinates of the centers of the rectangles.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    x = NULL,
    # 
    # > Enum('screen', 'data')
    height_units = NULL,
    # The line width values for the rectangles.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_width = NULL,
    # The overall heights of the rectangles.
    # > DistanceSpec(units_default='data')
    height = NULL,
    # The y-coordinates of the centers of the rectangles.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    y = NULL,
    # The line dash values for the rectangles.
    # > DashPattern
    line_dash = NULL,
    # The fill color values for the rectangles.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    fill_color = NULL,
    # The line color values for the rectangles.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    line_color = NULL
  )
)

# *toolbar icon*: |wheel_pan_icon|
# 
# The wheel pan tool allows the user to pan the plot along the configured
# dimension using the scroll wheel.
# 
# .. |wheel_pan_icon| image:: /_images/icons/WheelPan.png :height: 18pt
WheelPanTool <- R6::R6Class("WheelPanTool",
  inherit = Scroll,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      tags = list(), subscribed_events = list(), dimension = "width",
      name = NULL, js_property_callbacks = structure(list(), .Names =
      character(0)), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["WheelPanTool"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Which dimension the wheel pan tool is constrained to act in. By default
    # the wheel pan tool will pan the plot along the x-axis.
    # > Enum('width', 'height')
    dimension = NULL
  )
)

# A Range of values for a categorical dimension.
# 
# In addition to supplying ``factors`` as a keyword argument to the
# ``FactorRange`` initializer, you may also instantiate with a sequence
# of positional arguments:
# 
# .. code-block:: python
# 
# FactorRange("foo", "bar") # equivalent to FactorRange(factors=["foo",
# "bar"])
# 
# Users will normally supply categorical values directly:
# 
# .. code-block:: python
# 
# p.circle(x=["foo", "bar"], ...)
# 
# BokehJS will create a mapping from ``"foo"`` and ``"bar"`` to a
# numerical coordinate system called *synthetic coordinates*. In the
# simplest cases, factors are separated by a distance of 1.0 in synthetic
# coordinates, however the exact mapping from factors to synthetic
# coordinates is affected by he padding properties as well as whether the
# number of levels the factors have.
# 
# Users typically do not need to worry about the details of this mapping,
# however it can be useful to fine tune positions by adding offsets. When
# supplying factors as coordinates or values, it is possible to add an
# offset in the synthetic coordinate space by adding a final number value
# to a factor tuple. For example:
# 
# .. code-block:: python
# 
# p.circle(x=[("foo", 0.3), ...], ...)
# 
# will position the first circle at an ``x`` position that is offset by
# adding 0.3 to the synthetic coordinate for ``"foo"``.
FactorRange <- R6::R6Class("FactorRange",
  inherit = Range,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      end = NULL, factor_padding = 0, group_padding = 1.4,
      subgroup_padding = 0.8, bounds = NULL, range_padding = 0L,
      min_interval = NULL, max_interval = NULL, callback = NULL,
      range_padding_units = "percent", tags = list(),
      subscribed_events = list(), start = NULL, factors = list(), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(callback = callback,
        js_event_callbacks = js_event_callbacks, tags = tags,
        subscribed_events = subscribed_events, name = name,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["FactorRange"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The end of the range, in synthetic coordinates.
    # 
    # .. note:: Synthetic coordinates are only computed in the browser, based
    # on the factors and various padding properties. The value of ``end``
    # will only be available in situations where bidirectional communication
    # is available (e.g. server, notebook).
    # > Float
    end = NULL,
    # How much padding to add in between all lowest-level factors. When
    # ``factor_padding`` is non-zero, every factor in every group will have
    # the padding value applied.
    # > Float
    factor_padding = NULL,
    # How much padding to add in between top-level groups of factors. This
    # property only applies when the overall range factors have either two or
    # three levels. For example, with:
    # 
    # .. code-block:: python
    # 
    # FactorRange(factors=[["foo", "1'], ["foo", "2'], ["bar", "1"]])
    # 
    # The top level groups correspond to ``"foo"` and ``"bar"``, and the
    # group padding will be applied between the factors``["foo", "2']`` and
    # ``["bar", "1"]``
    # > Float
    group_padding = NULL,
    # How much padding to add in between mid-level groups of factors. This
    # property only applies when the overall factors have three levels. For
    # example with:
    # 
    # .. code-block:: python
    # 
    # FactorRange(factors=[ ['foo', 'A', '1'], ['foo', 'A', '2'], ['foo',
    # 'A', '3'], ['foo', 'B', '2'], ['bar', 'A', '1'], ['bar', 'A', '2'] ])
    # 
    # This property dictates how much padding to add between the three
    # factors in the `['foo', 'A']` group, and between the two factors in the
    # the [`bar`]
    # > Float
    subgroup_padding = NULL,
    # The bounds (in synthetic coordinates) that the range is allowed to go
    # to.  Typically used to prevent the user from panning/zooming/etc away
    # from the data.
    # 
    # .. note:: Synthetic coordinates are only computed in the browser, based
    # on the factors and various padding properties. Some experimentation may
    # be required to arrive at bounds suitable for specific situations.
    # 
    # By default, the bounds will be None, allowing your plot to pan/zoom as
    # far as you want. If bounds are 'auto' they will be computed to be the
    # same as the start and end of the FactorRange.
    # > MinMaxBounds(Auto, Tuple(Float, Float))
    bounds = NULL,
    # How much padding to add around the outside of computed range bounds.
    # 
    # When ``range_padding_units`` is set to ``"percent"``, the span of the
    # range span is expanded to make the range ``range_padding`` percent
    # larger.
    # 
    # When ``range_padding_units`` is set to ``"absolute"``, the start and
    # end of the range span are extended by the amount ``range_padding``.
    # > Float
    range_padding = NULL,
    # The level that the range is allowed to zoom in, expressed as the
    # minimum visible interval in synthetic coordinates. If set to ``None``
    # (default), the minimum interval is not bounded.
    # 
    # The default "width" of a category is 1.0 in synthetic coordinates.
    # However, the distance between factors is affected by the various
    # padding properties and whether or not factors are grouped.
    # > Float
    min_interval = NULL,
    # The level that the range is allowed to zoom out, expressed as the
    # maximum visible interval in synthetic coordinates.. Note that
    # ``bounds`` can impose an implicit constraint on the maximum interval as
    # well.
    # 
    # The default "width" of a category is 1.0 in synthetic coordinates.
    # However, the distance between factors is affected by the various
    # padding properties and whether or not factors are grouped.
    # > Float
    max_interval = NULL,
    # Whether the ``range_padding`` should be interpreted as a percentage, or
    # as an absolute quantity. (default: ``"percent"``)
    # > Enum('percent', 'absolute')
    range_padding_units = NULL,
    # The start of the range, in synthetic coordinates.
    # 
    # Synthetic coordinates are only computed in the browser, based on the
    # factors and various padding properties. The value of ``end`` will only
    # be available in situations where bidirectional communication is
    # available (e.g. server, notebook).
    # > Float
    start = NULL,
    # A sequence of factors to define this categorical range.
    # 
    # Factors may have 1, 2, or 3 levels. For 1-level factors, each factor is
    # simply a string. For example:
    # 
    # .. code-block: python
    # 
    # FactorRange(factors=["sales", "marketing", "engineering"])
    # 
    # defines a range with three simple factors that might represent
    # different units of a business.
    # 
    # For 2- and 3- level factors, each factor is a tuple of strings:
    # 
    # .. code-block:: python
    # 
    # FactorRange(factors=[ ["2016", "sales'], ["2016", "marketing'],
    # ["2016", "engineering"], ["2017", "sales'], ["2017", "marketing'],
    # ["2017", "engineering"], ])
    # 
    # defines a range with six 2-level factors that might represent the three
    # business units, grouped by year.
    # 
    # Note that factors and sub-factors *may only be strings*.
    # > Either(Seq(String), Seq(Tuple(String, String)), Seq(Tuple(String, String, String)))
    factors = NULL
  )
)

# Render text.
# 
# Example -------
# 
# .. bokeh-plot:: ../examples/reference/models/Text.py :source-position:
# below
Text <- R6::R6Class("Text",
  inherit = XYGlyph,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      text = "text", text_font_style = "normal", text_baseline = "bottom",
      x_offset = 0L, text_color = "#444444", x = NULL, text_font = "helvetica",
      y = NULL, angle = 0L, tags = list(), text_line_height = 1.2, y_offset = 0L,
      subscribed_events = list(), text_font_size = list(value = "12pt"),
      text_align = "left", text_alpha = 1, angle_units = "rad", name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Text"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The text values to render.
    # > StringSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), List(String))), List(String))
    text = NULL,
    # The text font style values for the text.
    # > Enum('normal', 'italic', 'bold')
    text_font_style = NULL,
    # The text baseline values for the text.
    # > Enum('top', 'middle', 'bottom', 'alphabetic', 'hanging', 'ideographic')
    text_baseline = NULL,
    # Offset values to apply to the x-coordinates.
    # 
    # This is useful, for instance, if it is desired to "float" text a fixed
    # distance in screen units from a given data position.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    x_offset = NULL,
    # The text color values for the text.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    text_color = NULL,
    # The x-coordinates to locate the text anchors.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    x = NULL,
    # The text font values for the text.
    # > String
    text_font = NULL,
    # The y-coordinates to locate the text anchors.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    y = NULL,
    # The angles to rotate the text, as measured from the horizontal.
    # > AngleSpec(units_default='rad')
    angle = NULL,
    # The text line height values for the text.
    # > Float
    text_line_height = NULL,
    # Offset values to apply to the y-coordinates.
    # 
    # This is useful, for instance, if it is desired to "float" text a fixed
    # distance in screen units from a given data position.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    y_offset = NULL,
    # The text font size values for the text.
    # > FontSizeSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), List(String))), List(String))
    text_font_size = NULL,
    # The text align values for the text.
    # > Enum('left', 'right', 'center')
    text_align = NULL,
    # The text alpha values for the text.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    text_alpha = NULL,
    # 
    # > Enum('deg', 'rad')
    angle_units = NULL
  )
)

# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
LayoutProvider <- R6::R6Class("LayoutProvider",
  inherit = Model,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["LayoutProvider"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Boolean (check mark) cell formatter.
BooleanFormatter <- R6::R6Class("BooleanFormatter",
  inherit = CellFormatter,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      icon = "check", tags = list(), subscribed_events = list(), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["BooleanFormatter"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The icon visualizing the check mark.
    # > Enum('check', 'check-circle', 'check-circle-o', 'check-square', 'check-square-o')
    icon = NULL
  )
)

# Model representing a plot, containing glyphs, guides, annotations.
Plot <- R6::R6Class("Plot",
  inherit = LayoutDOM,
  public = list(
    specified_args = NULL,
    initialize = function(
      right = list(), toolbar_sticky = TRUE, title_location = "above",
      x_scale = NULL, min_border_right = NULL, outline_line_alpha = 1,
      inner_width = NULL, background_fill_color = "#ffffff", title = NULL,
      above = list(), subscribed_events = list(),
      outline_line_color = "#e5e5e5", outline_line_dash = list(),
      js_event_callbacks = structure(list(), .Names = character(0)),
      toolbar_location = "right", layout_width = NULL, y_scale = NULL,
      width = NULL, renderers = list(), toolbar = NULL, x_range = NULL,
      outline_line_cap = "butt", plot_height = 600L, y_range = NULL,
      tags = list(), min_border_top = NULL, layout_height = NULL,
      outline_line_width = 1L, aspect_scale = 1L,
      outline_line_dash_offset = 0L, min_border_bottom = NULL,
      output_backend = "canvas", lod_threshold = 2000L, v_symmetry = FALSE,
      lod_factor = 10L, lod_interval = 300L, border_fill_alpha = 1,
      background_fill_alpha = 1, hidpi = TRUE, outline_line_join = "miter",
      left = list(), sizing_mode = "fixed", border_fill_color = "#ffffff",
      css_classes = NULL, min_border_left = NULL, h_symmetry = TRUE,
      min_border = 5L, js_property_callbacks = structure(list(), .Names =
      character(0)), name = NULL, lod_timeout = 500L,
      extra_x_ranges = structure(list(), .Names = character(0)),
      plot_width = 600L, below = list(), height = NULL, disabled = FALSE,
      extra_y_ranges = structure(list(), .Names = character(0)),
      match_aspect = FALSE, inner_height = NULL, id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        width = width, height = height, disabled = disabled,
        sizing_mode = sizing_mode, tags = tags,
        subscribed_events = subscribed_events, css_classes = css_classes,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["Plot"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # A list of renderers to occupy the area to the right of the plot.
    # > List(Instance(Renderer))
    right = NULL,
    # Stick the toolbar to the edge of the plot. Default: True. If False, the
    # toolbar will be outside of the axes, titles etc.
    # > Bool
    toolbar_sticky = NULL,
    # Where the title will be located. Titles on the left or right side will
    # be rotated.
    # > Enum('above', 'below', 'left', 'right')
    title_location = NULL,
    # What kind of scale to use to convert x-coordinates in data space into
    # x-coordinates in screen space.
    # > Instance(Scale)
    x_scale = NULL,
    # Minimum size in pixels of the padding region to the right of the
    # central plot region.
    # 
    # .. note:: This is a *minimum*. The padding region may expand as needed
    # to accommodate titles or axes, etc.
    # > Int
    min_border_right = NULL,
    # The line alpha for the plot border outline.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    outline_line_alpha = NULL,
    # This is the exact width of the plotting canvas, i.e. the width of the
    # actual plot, without toolbars etc. Note this is computed in a web
    # browser, so this property will work only in backends capable of
    # bidirectional communication (server, notebook).
    # 
    # .. note:: This is an experimental feature and the API may change in
    # near future.
    # > Int
    inner_width = NULL,
    # The fill color for the plot background style.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    background_fill_color = NULL,
    # A title for the plot. Can be a text string or a Title annotation.
    # > Instance(Title)
    title = NULL,
    # A list of renderers to occupy the area above of the plot.
    # > List(Instance(Renderer))
    above = NULL,
    # The line color for the plot border outline.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    outline_line_color = NULL,
    # The line dash for the plot border outline.
    # > DashPattern
    outline_line_dash = NULL,
    # Where the toolbar will be located. If set to None, no toolbar will be
    # attached to the plot.
    # > Enum('above', 'below', 'left', 'right')
    toolbar_location = NULL,
    # This is the exact width of the layout, i.e. the height of the actual
    # plot, with toolbars etc. Note this is computed in a web browser, so
    # this property will work only in backends capable of bidirectional
    # communication (server, notebook).
    # 
    # .. note:: This is an experimental feature and the API may change in
    # near future.
    # > Int
    layout_width = NULL,
    # What kind of scale to use to convert y-coordinates in data space into
    # y-coordinates in screen space.
    # > Instance(Scale)
    y_scale = NULL,
    # A list of all renderers for this plot, including guides and annotations
    # in addition to glyphs and markers.
    # 
    # This property can be manipulated by hand, but the ``add_glyph`` and
    # ``add_layout`` methods are recommended to help make sure all necessary
    # setup is performed.
    # > List(Instance(Renderer))
    renderers = NULL,
    # The toolbar associated with this plot which holds all the tools.
    # 
    # The toolbar is automatically created with the plot.
    # > Instance(Toolbar)
    toolbar = NULL,
    # The (default) data range of the horizontal dimension of the plot.
    # > Instance(Range)
    x_range = NULL,
    # The line cap for the plot border outline.
    # > Enum('butt', 'round', 'square')
    outline_line_cap = NULL,
    # Total height of the entire plot (including any axes, titles, border
    # padding, etc.)
    # 
    # .. note:: This corresponds directly to the height of the HTML canvas
    # that will be used.
    # > Int
    plot_height = NULL,
    # The (default) data range of the vertical dimension of the plot.
    # > Instance(Range)
    y_range = NULL,
    # Minimum size in pixels of the padding region above the top of the
    # central plot region.
    # 
    # .. note:: This is a *minimum*. The padding region may expand as needed
    # to accommodate titles or axes, etc.
    # > Int
    min_border_top = NULL,
    # This is the exact height of the layout, i.e. the height of the actual
    # plot, with toolbars etc. Note this is computed in a web browser, so
    # this property will work only in backends capable of bidirectional
    # communication (server, notebook).
    # 
    # .. note:: This is an experimental feature and the API may change in
    # near future.
    # > Int
    layout_height = NULL,
    # The line width for the plot border outline.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    outline_line_width = NULL,
    # A value to be given for increased aspect ratio control. This value is
    # added multiplicatively to the calculated value required for
    # ``match_aspect``.  ``aspect_scale`` is defined as the ratio of width
    # over height of the figure.
    # 
    # For example, a plot with ``aspect_scale`` value of 2 will result in a
    # square in *data units* to be drawn on the screen as a rectangle with a
    # pixel width twice as long as its pixel height.
    # 
    # .. note:: This setting only takes effect if ``match_aspect`` is set to
    # ``True``.
    # > Float
    aspect_scale = NULL,
    # The line dash offset for the plot border outline.
    # > Int
    outline_line_dash_offset = NULL,
    # Minimum size in pixels of the padding region below the bottom of the
    # central plot region.
    # 
    # .. note:: This is a *minimum*. The padding region may expand as needed
    # to accommodate titles or axes, etc.
    # > Int
    min_border_bottom = NULL,
    # Specify the output backend for the plot area. Default is HTML5 Canvas.
    # 
    # .. note:: When set to ``webgl``, glyphs without a WebGL rendering
    # implementation will fall back to rendering onto 2D canvas.
    # > Enum('canvas', 'svg', 'webgl')
    output_backend = NULL,
    # A number of data points, above which level-of-detail downsampling may
    # be performed by glyph renderers. Set to ``None`` to disable any
    # level-of-detail downsampling.
    # > Int
    lod_threshold = NULL,
    # Whether the total vertical padding on both sides of the plot will be
    # made equal (the top or bottom padding amount, whichever is larger).
    # > Bool
    v_symmetry = NULL,
    # Decimation factor to use when applying level-of-detail decimation.
    # > Int
    lod_factor = NULL,
    # Interval (in ms) during which an interactive tool event will enable
    # level-of-detail downsampling.
    # > Int
    lod_interval = NULL,
    # The fill alpha for the plot border style.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    border_fill_alpha = NULL,
    # The fill alpha for the plot background style.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    background_fill_alpha = NULL,
    # Whether to use HiDPI mode when available.
    # > Bool
    hidpi = NULL,
    # The line join for the plot border outline.
    # > Enum('miter', 'round', 'bevel')
    outline_line_join = NULL,
    # A list of renderers to occupy the area to the left of the plot.
    # > List(Instance(Renderer))
    left = NULL,
    # The fill color for the plot border style.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    border_fill_color = NULL,
    # Minimum size in pixels of the padding region to the left of the central
    # plot region.
    # 
    # .. note:: This is a *minimum*. The padding region may expand as needed
    # to accommodate titles or axes, etc.
    # > Int
    min_border_left = NULL,
    # Whether the total horizontal padding on both sides of the plot will be
    # made equal (the left or right padding amount, whichever is larger).
    # > Bool
    h_symmetry = NULL,
    # A convenience property to set all all the ``min_border_X`` properties
    # to the same value. If an individual border property is explicitly set,
    # it will override ``min_border``.
    # > Int
    min_border = NULL,
    # Timeout (in ms) for checking whether interactive tool events are still
    # occurring. Once level-of-detail mode is enabled, a check is made every
    # ``lod_timeout`` ms. If no interactive tool events have happened,
    # level-of-detail mode is disabled.
    # > Int
    lod_timeout = NULL,
    # Additional named ranges to make available for mapping x-coordinates.
    # 
    # This is useful for adding additional axes.
    # > Dict(String, Instance(Range))
    extra_x_ranges = NULL,
    # Total width of the entire plot (including any axes, titles, border
    # padding, etc.)
    # 
    # .. note:: This corresponds directly to the width of the HTML canvas
    # that will be used.
    # > Int
    plot_width = NULL,
    # A list of renderers to occupy the area below of the plot.
    # > List(Instance(Renderer))
    below = NULL,
    # Additional named ranges to make available for mapping y-coordinates.
    # 
    # This is useful for adding additional axes.
    # > Dict(String, Instance(Range))
    extra_y_ranges = NULL,
    # Specify the aspect ratio behavior of the plot. Aspect ratio is defined
    # as the ratio of width over height. This property controls whether Bokeh
    # should attempt the match the (width/height) of *data space* to the
    # (width/height) in pixels of *screen space*.
    # 
    # Default is ``False`` which indicates that the *data* aspect ratio and
    # the *screen* aspect ratio vary independently. ``True`` indicates that
    # the plot aspect ratio of the axes will match the aspect ratio of the
    # pixel extent the axes. The end result is that a 1x1 area in data space
    # is a square in pixels, and conversely that a 1x1 pixel is a square in
    # data units.
    # 
    # .. note:: This setting only takes effect when there are two dataranges.
    # This setting only sets the initial plot draw and subsequent resets. It
    # is possible for tools (single axis zoom, unconstrained box zoom) to
    # change the aspect ratio.
    # > Bool
    match_aspect = NULL,
    # This is the exact height of the plotting canvas, i.e. the height of the
    # actual plot, without toolbars etc. Note this is computed in a web
    # browser, so this property will work only in backends capable of
    # bidirectional communication (server, notebook).
    # 
    # .. note:: This is an experimental feature and the API may change in
    # near future.
    # > Int
    inner_height = NULL
  )
)

# A base class for data source types.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
DataSource <- R6::R6Class("DataSource",
  inherit = Model,
  public = list(
    specified_args = NULL,
    initialize = function(
      callback = NULL, js_event_callbacks = structure(list(), .Names =
      character(0)), tags = list(), subscribed_events = list(),
      selected = structure(list(`0d` = structure(list(indices = list(),
      glyph = NULL), .Names = c("indices", "glyph")), `2d` =
      structure(list(indices = structure(list(), .Names =
      character(0))), .Names = "indices"), `1d` =
      structure(list(indices = list()), .Names = "indices")), .Names =
      c("0d", "2d", "1d")), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["DataSource"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # A callback to run in the browser whenever the selection is changed.
    # > Instance(Callback)
    callback = NULL,
    # A dict to indicate selected indices on different dimensions on this
    # DataSource. Keys are:
    # 
    # .. code-block:: python
    # 
    # # selection information for line and patch glyphs '0d' : { # the glyph
    # that was selected 'glyph': None
    # 
    # # array with the [smallest] index of the segment of the line that was
    # hit 'indices': [] }
    # 
    # # selection for most (point-like) glyphs, except lines and patches
    # '1d': { # indices of the points included in the selection indices: [] }
    # 
    # # selection information for multiline and patches glyphs '2d': { #
    # mapping of indices of the multiglyph to array of glyph indices that
    # were hit # e.g. {3: [5, 6], 4: [5]} indices: {} }
    # > Dict(String, Dict(String, Any))
    selected = NULL
  )
)

# Abstract base class for data table's cell editors.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
CellEditor <- R6::R6Class("CellEditor",
  inherit = Model,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["CellEditor"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# An axis that picks evenly spaced tick locations for a collection of
# categories/factors.
CategoricalAxis <- R6::R6Class("CategoricalAxis",
  inherit = Axis,
  public = list(
    specified_args = NULL,
    initialize = function(
      major_tick_line_color = "black", subgroup_text_align = "left",
      axis_line_join = "miter", minor_tick_line_dash = list(),
      formatter = NULL, y_range_name = "default", axis_line_dash = list(),
      subgroup_text_font = "helvetica", axis_line_alpha = 1,
      js_event_callbacks = structure(list(), .Names = character(0)),
      minor_tick_line_join = "miter", major_label_text_font = "helvetica",
      subgroup_text_color = "#444444", group_text_font_size = list(value =
      "8pt"), major_label_text_line_height = 1.2,
      major_label_text_font_size = list(value = "8pt"), plot = NULL,
      separator_line_dash_offset = 0L, axis_label_text_color = "#444444",
      major_label_text_baseline = "alphabetic", axis_line_dash_offset = 0L,
      major_label_standoff = 5L, x_range_name = "default",
      major_label_text_color = "#444444", axis_line_color = "black",
      axis_line_cap = "butt", major_label_orientation = "horizontal",
      axis_label_standoff = 5L, name = NULL, visible = TRUE,
      group_text_align = "left", group_text_font_style = "bold",
      major_tick_line_join = "miter", group_text_font = "helvetica",
      group_text_baseline = "bottom", separator_line_cap = "butt",
      separator_line_width = 2L, axis_line_width = 1L,
      major_tick_line_dash_offset = 0L, major_tick_line_dash = list(),
      major_tick_line_cap = "butt", subgroup_text_line_height = 1.2,
      axis_label_text_font_style = "italic", separator_line_join = "miter",
      major_tick_line_width = 1L, axis_label = "",
      minor_tick_line_color = "black", axis_label_text_font = "helvetica",
      ticker = NULL, subscribed_events = list(), separator_line_alpha = 1,
      major_label_text_font_style = "normal", group_text_line_height = 1.2,
      minor_tick_out = 4L, major_tick_in = 2L, axis_label_text_alpha = 1,
      minor_tick_line_dash_offset = 0L, separator_line_dash = list(),
      tags = list(), group_text_color = "grey",
      subgroup_text_font_size = list(value = "8pt"),
      axis_label_text_font_size = list(value = "10pt"),
      major_tick_out = 6L, minor_tick_line_alpha = 1,
      major_label_text_alpha = 1, major_label_overrides = structure(list(),
      .Names = character(0)), bounds = "auto", major_tick_line_alpha = 1,
      separator_line_color = "lightgrey",
      major_label_text_align = "center",
      js_property_callbacks = structure(list(), .Names = character(0)),
      group_text_alpha = 1, axis_label_text_align = "left",
      subgroup_text_baseline = "bottom", minor_tick_line_width = 1L,
      minor_tick_line_cap = "butt", minor_tick_in = 0L,
      axis_label_text_line_height = 1.2,
      axis_label_text_baseline = "bottom",
      subgroup_text_font_style = "bold", subgroup_text_alpha = 1,
      level = "overlay", id = NULL
    ) {
      super$initialize(major_tick_line_color = major_tick_line_color,
        major_tick_line_dash_offset = major_tick_line_dash_offset,
        major_tick_line_dash = major_tick_line_dash,
        major_tick_line_cap = major_tick_line_cap,
        axis_line_join = axis_line_join,
        axis_label_text_font_style = axis_label_text_font_style,
        minor_tick_line_dash = minor_tick_line_dash, formatter = formatter,
        major_tick_line_width = major_tick_line_width,
        axis_line_dash = axis_line_dash, axis_label = axis_label,
        minor_tick_line_color = minor_tick_line_color,
        axis_label_text_font = axis_label_text_font,
        y_range_name = y_range_name, ticker = ticker,
        subscribed_events = subscribed_events,
        axis_line_alpha = axis_line_alpha,
        js_event_callbacks = js_event_callbacks,
        major_label_text_font_style = major_label_text_font_style,
        minor_tick_line_join = minor_tick_line_join,
        major_label_text_font = major_label_text_font,
        minor_tick_out = minor_tick_out,
        major_label_text_line_height = major_label_text_line_height,
        major_tick_in = major_tick_in,
        major_label_text_font_size = major_label_text_font_size,
        axis_label_text_alpha = axis_label_text_alpha,
        minor_tick_line_dash_offset = minor_tick_line_dash_offset,
        plot = plot, tags = tags,
        axis_label_text_font_size = axis_label_text_font_size,
        major_tick_out = major_tick_out,
        axis_label_text_color = axis_label_text_color,
        minor_tick_line_alpha = minor_tick_line_alpha,
        major_label_text_baseline = major_label_text_baseline,
        major_label_text_alpha = major_label_text_alpha,
        major_label_overrides = major_label_overrides,
        axis_line_dash_offset = axis_line_dash_offset, bounds = bounds,
        x_range_name = x_range_name,
        major_tick_line_alpha = major_tick_line_alpha,
        major_label_text_color = major_label_text_color,
        axis_line_color = axis_line_color, axis_line_cap = axis_line_cap,
        major_label_text_align = major_label_text_align,
        major_label_orientation = major_label_orientation,
        axis_label_standoff = axis_label_standoff,
        js_property_callbacks = js_property_callbacks, name = name,
        axis_label_text_align = axis_label_text_align, visible = visible,
        minor_tick_line_width = minor_tick_line_width,
        major_tick_line_join = major_tick_line_join,
        minor_tick_line_cap = minor_tick_line_cap,
        minor_tick_in = minor_tick_in,
        axis_label_text_line_height = axis_label_text_line_height,
        axis_label_text_baseline = axis_label_text_baseline,
        major_label_standoff = major_label_standoff,
        axis_line_width = axis_line_width, level = level, id = id)
      types <- bk_prop_types[["CategoricalAxis"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The text align of the group top-level categorical groups.
    # > Enum('left', 'right', 'center')
    subgroup_text_align = NULL,
    # The text font of the group top-level categorical groups.
    # > String
    subgroup_text_font = NULL,
    # The text color of the group top-level categorical groups.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    subgroup_text_color = NULL,
    # The text font size of the group top-level categorical groups.
    # > FontSizeSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), List(String))), List(String))
    group_text_font_size = NULL,
    # The line dash offset of the separator line between top-level
    # categorical groups.
    # > Int
    separator_line_dash_offset = NULL,
    # The text align of the group top-level categorical groups.
    # > Enum('left', 'right', 'center')
    group_text_align = NULL,
    # The text font style of the group top-level categorical groups.
    # > Enum('normal', 'italic', 'bold')
    group_text_font_style = NULL,
    # The text font of the group top-level categorical groups.
    # > String
    group_text_font = NULL,
    # The text baseline of the group top-level categorical groups.
    # > Enum('top', 'middle', 'bottom', 'alphabetic', 'hanging', 'ideographic')
    group_text_baseline = NULL,
    # The line cap of the separator line between top-level categorical
    # groups.
    # > Enum('butt', 'round', 'square')
    separator_line_cap = NULL,
    # The line width of the separator line between top-level categorical
    # groups.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    separator_line_width = NULL,
    # The text line height of the group top-level categorical groups.
    # > Float
    subgroup_text_line_height = NULL,
    # The line join of the separator line between top-level categorical
    # groups.
    # > Enum('miter', 'round', 'bevel')
    separator_line_join = NULL,
    # The line alpha of the separator line between top-level categorical
    # groups.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    separator_line_alpha = NULL,
    # The text line height of the group top-level categorical groups.
    # > Float
    group_text_line_height = NULL,
    # The line dash of the separator line between top-level categorical
    # groups.
    # > DashPattern
    separator_line_dash = NULL,
    # The text color of the group top-level categorical groups.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    group_text_color = NULL,
    # The text font size of the group top-level categorical groups.
    # > FontSizeSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), List(String))), List(String))
    subgroup_text_font_size = NULL,
    # The line color of the separator line between top-level categorical
    # groups.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    separator_line_color = NULL,
    # The text alpha of the group top-level categorical groups.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    group_text_alpha = NULL,
    # The text baseline of the group top-level categorical groups.
    # > Enum('top', 'middle', 'bottom', 'alphabetic', 'hanging', 'ideographic')
    subgroup_text_baseline = NULL,
    # The text font style of the group top-level categorical groups.
    # > Enum('normal', 'italic', 'bold')
    subgroup_text_font_style = NULL,
    # The text alpha of the group top-level categorical groups.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    subgroup_text_alpha = NULL
  )
)

# Render arcs.
# 
# Example -------
# 
# .. bokeh-plot:: ../examples/reference/models/Arc.py :source-position:
# below
Arc <- R6::R6Class("Arc",
  inherit = XYGlyph,
  public = list(
    specified_args = NULL,
    initialize = function(
      radius_units = "data", line_cap = "butt", start_angle_units = "rad",
      radius = NULL, start_angle = NULL, line_alpha = 1,
      subscribed_events = list(), line_dash_offset = 0L,
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      name = NULL, end_angle_units = "rad", line_join = "miter", x = NULL,
      line_width = 1L, y = NULL, end_angle = NULL, line_dash = list(),
      line_color = "black", tags = list(), direction = "anticlock", id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Arc"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # 
    # > Enum('screen', 'data')
    radius_units = NULL,
    # The line cap values for the arcs.
    # > Enum('butt', 'round', 'square')
    line_cap = NULL,
    # 
    # > Enum('deg', 'rad')
    start_angle_units = NULL,
    # Radius of the arc.
    # > DistanceSpec(units_default='data')
    radius = NULL,
    # The angles to start the arcs, as measured from the horizontal.
    # > AngleSpec(units_default='rad')
    start_angle = NULL,
    # The line alpha values for the arcs.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_alpha = NULL,
    # The line dash offset values for the arcs.
    # > Int
    line_dash_offset = NULL,
    # 
    # > Enum('deg', 'rad')
    end_angle_units = NULL,
    # The line join values for the arcs.
    # > Enum('miter', 'round', 'bevel')
    line_join = NULL,
    # The x-coordinates of the center of the arcs.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    x = NULL,
    # The line width values for the arcs.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_width = NULL,
    # The y-coordinates of the center of the arcs.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    y = NULL,
    # The angles to end the arcs, as measured from the horizontal.
    # > AngleSpec(units_default='rad')
    end_angle = NULL,
    # The line dash values for the arcs.
    # > DashPattern
    line_dash = NULL,
    # The line color values for the arcs.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    line_color = NULL,
    # Which direction to stroke between the start and end angles.
    # > Enum('clock', 'anticlock')
    direction = NULL
  )
)

# Single-line input widget with auto-completion.
AutocompleteInput <- R6::R6Class("AutocompleteInput",
  inherit = TextInput,
  public = list(
    specified_args = NULL,
    initialize = function(
      completions = list(), js_event_callbacks = structure(list(), .Names =
      character(0)), js_property_callbacks = structure(list(), .Names =
      character(0)), width = NULL, height = NULL, disabled = FALSE,
      callback = NULL, tags = list(), sizing_mode = "fixed", title = "",
      placeholder = "", subscribed_events = list(), css_classes = NULL,
      name = NULL, value = "", id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, width = width,
        height = height, disabled = disabled, callback = callback, tags = tags,
        sizing_mode = sizing_mode, title = title, placeholder = placeholder,
        subscribed_events = subscribed_events, css_classes = css_classes,
        name = name, value = value, id = id)
      types <- bk_prop_types[["AutocompleteInput"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # A list of completion strings. This will be used to guide the user upon
    # typing the beginning of a desired value.
    # > List(String)
    completions = NULL
  )
)

# Render asterisk '*' markers.
# 
# Example -------
# 
# .. bokeh-plot:: ../examples/reference/models/Asterisk.py
# :source-position: below
Asterisk <- R6::R6Class("Asterisk",
  inherit = Marker,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      line_join = "miter", x = NULL, size = 4L, line_width = 1L,
      line_cap = "butt", fill_alpha = 1, y = NULL, angle = 0, line_dash = list(),
      fill_color = "gray", line_color = "black", tags = list(),
      subscribed_events = list(), line_alpha = 1, angle_units = "rad",
      name = NULL, line_dash_offset = 0L, id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks,
        line_join = line_join, x = x, size = size, line_width = line_width,
        line_cap = line_cap, fill_alpha = fill_alpha, y = y, angle = angle,
        line_dash = line_dash, fill_color = fill_color,
        line_color = line_color, tags = tags,
        subscribed_events = subscribed_events, line_alpha = line_alpha,
        angle_units = angle_units, name = name,
        line_dash_offset = line_dash_offset, id = id)
      types <- bk_prop_types[["Asterisk"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Generate ticks spaced apart by specific, even multiples of months.
MonthsTicker <- R6::R6Class("MonthsTicker",
  inherit = SingleIntervalTicker,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      num_minor_ticks = 5L, desired_num_ticks = 6L, months = list(),
      tags = list(), subscribed_events = list(), interval = NULL, name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        num_minor_ticks = num_minor_ticks,
        desired_num_ticks = desired_num_ticks, tags = tags,
        subscribed_events = subscribed_events, interval = interval,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["MonthsTicker"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The intervals of months to use.
    # > Seq(Int)
    months = NULL
  )
)

# Apply either fixed dodge amount to data.
Dodge <- R6::R6Class("Dodge",
  inherit = Transform,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      subscribed_events = list(), tags = list(), range = NULL, name = NULL,
      value = 0L, id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Dodge"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # When applying ``Dodge`` to categorical data values, the corresponding
    # ``FactorRange`` must be supplied as the ``range`` property.
    # > Instance(Range)
    range = NULL,
    # The amount to dodge the input data.
    # > Float
    value = NULL
  )
)

# An abstract base class for icon widgets.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
AbstractIcon <- R6::R6Class("AbstractIcon",
  inherit = Widget,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      width = NULL, height = NULL, disabled = FALSE, sizing_mode = "fixed",
      tags = list(), subscribed_events = list(), css_classes = NULL,
      name = NULL, js_property_callbacks = structure(list(), .Names =
      character(0)), id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        width = width, height = height, disabled = disabled,
        sizing_mode = sizing_mode, tags = tags,
        subscribed_events = subscribed_events, css_classes = css_classes,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["AbstractIcon"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
RemoteSource <- R6::R6Class("RemoteSource",
  inherit = ColumnDataSource,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      data_url = NULL, column_names = list(), callback = NULL, tags = list(),
      data = structure(list(), .Names = character(0)),
      polling_interval = NULL, subscribed_events = list(),
      selected = structure(list(`0d` = structure(list(indices = list(),
      glyph = NULL), .Names = c("indices", "glyph")), `2d` =
      structure(list(indices = structure(list(), .Names =
      character(0))), .Names = "indices"), `1d` =
      structure(list(indices = list()), .Names = "indices")), .Names =
      c("0d", "2d", "1d")), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        column_names = column_names, callback = callback, data = data,
        tags = tags, subscribed_events = subscribed_events,
        selected = selected, name = name,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["RemoteSource"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The URL to the endpoint for the data.
    # > String
    data_url = NULL,
    # polling interval for updating data source in milliseconds
    # > Int
    polling_interval = NULL
  )
)

# Spinner-based number cell editor.
NumberEditor <- R6::R6Class("NumberEditor",
  inherit = CellEditor,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      step = 0.01, tags = list(), subscribed_events = list(), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["NumberEditor"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The major step value.
    # > Float
    step = NULL
  )
)

# A fixed, closed range [start, end] in a continuous scalar dimension.
# 
# In addition to supplying ``start`` and ``end`` keyword arguments to the
# ``Range1d`` initializer, you can also instantiate with the convenience
# syntax::
# 
# Range(0, 10) # equivalent to Range(start=0, end=10)
Range1d <- R6::R6Class("Range1d",
  inherit = Range,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      end = 1L, min_interval = NULL, bounds = NULL, max_interval = NULL,
      callback = NULL, tags = list(), subscribed_events = list(), start = 0L,
      name = NULL, js_property_callbacks = structure(list(), .Names =
      character(0)), id = NULL
    ) {
      super$initialize(callback = callback,
        js_event_callbacks = js_event_callbacks, tags = tags,
        subscribed_events = subscribed_events, name = name,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["Range1d"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The end of the range.
    # > Either(Float, Datetime, Int)
    end = NULL,
    # The level that the range is allowed to zoom in, expressed as the
    # minimum visible interval. If set to ``None`` (default), the minimum
    # interval is not bound. Can be a timedelta.
    # > Either(Float, TimeDelta, Int)
    min_interval = NULL,
    # The bounds that the range is allowed to go to. Typically used to
    # prevent the user from panning/zooming/etc away from the data.
    # 
    # If set to ``'auto'``, the bounds will be computed to the start and end
    # of the Range.
    # 
    # Bounds are provided as a tuple of ``(min, max)`` so regardless of
    # whether your range is increasing or decreasing, the first item should
    # be the minimum value of the range and the second item should be the
    # maximum. Setting min > max will result in a ``ValueError``.
    # 
    # By default, bounds are ``None`` and your plot to pan/zoom as far as you
    # want. If you only want to constrain one end of the plot, you can set
    # min or max to None.
    # 
    # Examples:
    # 
    # Range1d(0, 1, bounds='auto') # Auto-bounded to 0 and 1 (Default
    # behavior) Range1d(start=0, end=1, bounds=(0, None)) # Maximum is
    # unbounded, minimum bounded to 0
    # > MinMaxBounds(Auto, Tuple(Float, Float), Tuple(Datetime, Datetime))
    bounds = NULL,
    # The level that the range is allowed to zoom out, expressed as the
    # maximum visible interval. Can be a timedelta. Note that ``bounds`` can
    # impose an implicit constraint on the maximum interval as well.
    # > Either(Float, TimeDelta, Int)
    max_interval = NULL,
    # The start of the range.
    # > Either(Float, Datetime, Int)
    start = NULL
  )
)

# A button tool to provide a "help" link to users.
# 
# The hover text can be customized through the ``help_tooltip`` attribute
# and the redirect site overridden as well.
HelpTool <- R6::R6Class("HelpTool",
  inherit = Action,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      redirect = "https://bokeh.pydata.org/en/latest/docs/user_guide/tools.html#built-in-tools",
      tags = list(), subscribed_events = list(), help_tooltip = "Click the
      question mark to learn more about Bokeh plot tools.", name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["HelpTool"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Site to be redirected through upon click.
    # > String
    redirect = NULL,
    # Tooltip displayed when hovering over the help icon.
    # > String
    help_tooltip = NULL
  )
)

# Compute a linear interpolation between the control points provided
# through the ``x``, ``y``, and ``data`` parameters.
LinearInterpolator <- R6::R6Class("LinearInterpolator",
  inherit = Interpolator,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      x = NULL, y = NULL, data = NULL, tags = list(), subscribed_events = list(),
      clip = TRUE, name = NULL, js_property_callbacks = structure(list(),
      .Names = character(0)), id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks, x = x, y = y,
        data = data, tags = tags, subscribed_events = subscribed_events,
        clip = clip, name = name,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["LinearInterpolator"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Render informational legends for a plot.
Legend <- R6::R6Class("Legend",
  inherit = Annotation,
  public = list(
    specified_args = NULL,
    initialize = function(
      label_text_color = "#444444", border_line_dash_offset = 0L,
      border_line_dash = list(), label_text_line_height = 1.2,
      border_line_width = 1L, inactive_fill_alpha = 0.9,
      level = "annotation", label_text_align = "left",
      inactive_fill_color = "white", label_standoff = 5L, label_width = 20L,
      background_fill_alpha = 0.95, background_fill_color = "#ffffff",
      items = list(), click_policy = "none", subscribed_events = list(),
      glyph_height = 20L, label_text_font_style = "normal",
      js_event_callbacks = structure(list(), .Names = character(0)),
      orientation = "vertical", js_property_callbacks = structure(list(),
      .Names = character(0)), visible = TRUE, glyph_width = 20L, name = NULL,
      border_line_join = "miter", label_text_font = "helvetica",
      border_line_color = "#e5e5e5", spacing = 3L, border_line_alpha = 0.5,
      label_text_font_size = list(value = "10pt"),
      label_text_baseline = "middle", location = "top_right", plot = NULL,
      border_line_cap = "butt", tags = list(), label_height = 20L,
      label_text_alpha = 1, padding = 10L, margin = 10L, id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        visible = visible, name = name, plot = plot, tags = tags,
        subscribed_events = subscribed_events, level = level,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["Legend"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The text color for the legend labels.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    label_text_color = NULL,
    # The line dash offset for the legend border outline.
    # > Int
    border_line_dash_offset = NULL,
    # The line dash for the legend border outline.
    # > DashPattern
    border_line_dash = NULL,
    # The text line height for the legend labels.
    # > Float
    label_text_line_height = NULL,
    # The line width for the legend border outline.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    border_line_width = NULL,
    # The fill alpha for the legend background style when inactive.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    inactive_fill_alpha = NULL,
    # The text align for the legend labels.
    # > Enum('left', 'right', 'center')
    label_text_align = NULL,
    # The fill color for the legend background style when inactive.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    inactive_fill_color = NULL,
    # The distance (in pixels) to separate the label from its associated
    # glyph.
    # > Int
    label_standoff = NULL,
    # The minimum width (in pixels) of the area that legend labels should
    # occupy.
    # > Int
    label_width = NULL,
    # The fill alpha for the legend background style.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    background_fill_alpha = NULL,
    # The fill color for the legend background style.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    background_fill_color = NULL,
    # A list of :class:`~bokeh.model.annotations.LegendItem` instances to be
    # rendered in the legend.
    # 
    # This can be specified explicitly, for instance:
    # 
    # .. code-block:: python
    # 
    # legend = Legend(items=[ LegendItem(label="sin(x)" , renderers=[r0,
    # r1]), LegendItem(label="2*sin(x)" , renderers=[r2]),
    # LegendItem(label="3*sin(x)" , renderers=[r3, r4]) ])
    # 
    # But as a convenience, can also be given more compactly as a list of
    # tuples:
    # 
    # .. code-block:: python
    # 
    # legend = Legend(items=[ ("sin(x)" , [r0, r1]), ("2*sin(x)" , [r2]),
    # ("3*sin(x)" , [r3, r4]) ])
    # 
    # where each tuple is of the form: *(label, renderers)*.
    # > List(Instance(LegendItem))
    items = NULL,
    # Defines what happens when a lengend's item is clicked.
    # > Enum('none', 'hide', 'mute')
    click_policy = NULL,
    # The height (in pixels) that the rendered legend glyph should occupy.
    # > Int
    glyph_height = NULL,
    # The text font style for the legend labels.
    # > Enum('normal', 'italic', 'bold')
    label_text_font_style = NULL,
    # Whether the legend entries should be placed vertically or horizontally
    # when they are drawn.
    # > Enum('horizontal', 'vertical')
    orientation = NULL,
    # The width (in pixels) that the rendered legend glyph should occupy.
    # > Int
    glyph_width = NULL,
    # The line join for the legend border outline.
    # > Enum('miter', 'round', 'bevel')
    border_line_join = NULL,
    # The text font for the legend labels.
    # > String
    label_text_font = NULL,
    # The line color for the legend border outline.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    border_line_color = NULL,
    # Amount of spacing (in pixles) between legend entries.
    # > Int
    spacing = NULL,
    # The line alpha for the legend border outline.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    border_line_alpha = NULL,
    # The text font size for the legend labels.
    # > FontSizeSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), List(String))), List(String))
    label_text_font_size = NULL,
    # The text baseline for the legend labels.
    # > Enum('top', 'middle', 'bottom', 'alphabetic', 'hanging', 'ideographic')
    label_text_baseline = NULL,
    # The location where the legend should draw itself. It's either one of
    # ``bokeh.core.enums.LegendLocation``'s enumerated values, or a ``(x,
    # y)`` tuple indicating an absolute location absolute location in screen
    # coordinates (pixels from the bottom-left corner).
    # > Either(Enum('top_left', 'top_center', 'top_right', 'center_left', 'center', 'center_right', 'bottom_left', 'bottom_center', 'bottom_right'), Tuple(Float, Float))
    location = NULL,
    # The line cap for the legend border outline.
    # > Enum('butt', 'round', 'square')
    border_line_cap = NULL,
    # The minimum height (in pixels) of the area that legend labels should
    # occupy.
    # > Int
    label_height = NULL,
    # The text alpha for the legend labels.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    label_text_alpha = NULL,
    # Amount of padding around the contents of the legend. Only applicable
    # when when border is visible, otherwise collapses to 0.
    # > Int
    padding = NULL,
    # Amount of margin around the legend.
    # > Int
    margin = NULL
  )
)

# Render diamond markers with a '+' cross through the center.
# 
# Example -------
# 
# .. bokeh-plot:: ../examples/reference/models/DiamondCross.py
# :source-position: below
DiamondCross <- R6::R6Class("DiamondCross",
  inherit = Marker,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      line_join = "miter", x = NULL, size = 4L, line_width = 1L,
      line_cap = "butt", fill_alpha = 1, y = NULL, angle = 0, line_dash = list(),
      fill_color = "gray", line_color = "black", tags = list(),
      subscribed_events = list(), line_alpha = 1, angle_units = "rad",
      name = NULL, line_dash_offset = 0L, id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks,
        line_join = line_join, x = x, size = size, line_width = line_width,
        line_cap = line_cap, fill_alpha = fill_alpha, y = y, angle = angle,
        line_dash = line_dash, fill_color = fill_color,
        line_color = line_color, tags = tags,
        subscribed_events = subscribed_events, line_alpha = line_alpha,
        angle_units = angle_units, name = name,
        line_dash_offset = line_dash_offset, id = id)
      types <- bk_prop_types[["DiamondCross"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Display tick values from continuous ranges as "basic numbers", using
# scientific notation when appropriate by default.
BasicTickFormatter <- R6::R6Class("BasicTickFormatter",
  inherit = TickFormatter,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      precision = "auto", power_limit_high = 5L, use_scientific = TRUE,
      subscribed_events = list(), tags = list(), power_limit_low = -3L,
      name = NULL, js_property_callbacks = structure(list(), .Names =
      character(0)), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["BasicTickFormatter"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # How many digits of precision to display in tick labels.
    # > Either(Auto, Int)
    precision = NULL,
    # Limit the use of scientific notation to when::
    # 
    # log(x) >= power_limit_high
    # > Int
    power_limit_high = NULL,
    # Whether to ever display scientific notation. If ``True``, then when to
    # use scientific notation is controlled by ``power_limit_low`` and
    # ``power_limit_high``.
    # > Bool
    use_scientific = NULL,
    # Limit the use of scientific notation to when::
    # 
    # log(x) <= power_limit_low
    # > Int
    power_limit_low = NULL
  )
)

# A base class for all numeric, non-categorical axes types.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
ContinuousAxis <- R6::R6Class("ContinuousAxis",
  inherit = Axis,
  public = list(
    specified_args = NULL,
    initialize = function(
      major_tick_line_color = "black", major_tick_line_dash_offset = 0L,
      major_tick_line_dash = list(), major_tick_line_cap = "butt",
      axis_line_join = "miter", axis_label_text_font_style = "italic",
      minor_tick_line_dash = list(), formatter = NULL,
      major_tick_line_width = 1L, axis_line_dash = list(), axis_label = "",
      minor_tick_line_color = "black", axis_label_text_font = "helvetica",
      y_range_name = "default", ticker = NULL, subscribed_events = list(),
      axis_line_alpha = 1, js_event_callbacks = structure(list(), .Names =
      character(0)), major_label_text_font_style = "normal",
      minor_tick_line_join = "miter", major_label_text_font = "helvetica",
      minor_tick_out = 4L, major_label_text_line_height = 1.2,
      major_tick_in = 2L, major_label_text_font_size = list(value = "8pt"),
      axis_label_text_alpha = 1, minor_tick_line_dash_offset = 0L,
      plot = NULL, tags = list(), axis_label_text_font_size = list(value =
      "10pt"), major_tick_out = 6L, axis_label_text_color = "#444444",
      minor_tick_line_alpha = 1, major_label_text_baseline = "alphabetic",
      major_label_text_alpha = 1, major_label_overrides = structure(list(),
      .Names = character(0)), axis_line_dash_offset = 0L, bounds = "auto",
      x_range_name = "default", major_tick_line_alpha = 1,
      major_label_text_color = "#444444", axis_line_color = "black",
      axis_line_cap = "butt", major_label_text_align = "center",
      major_label_orientation = "horizontal", axis_label_standoff = 5L,
      js_property_callbacks = structure(list(), .Names = character(0)),
      name = NULL, axis_label_text_align = "left", visible = TRUE,
      minor_tick_line_width = 1L, major_tick_line_join = "miter",
      minor_tick_line_cap = "butt", minor_tick_in = 0L,
      axis_label_text_line_height = 1.2,
      axis_label_text_baseline = "bottom", major_label_standoff = 5L,
      axis_line_width = 1L, level = "overlay", id = NULL
    ) {
      super$initialize(major_tick_line_color = major_tick_line_color,
        major_tick_line_dash_offset = major_tick_line_dash_offset,
        major_tick_line_dash = major_tick_line_dash,
        major_tick_line_cap = major_tick_line_cap,
        axis_line_join = axis_line_join,
        axis_label_text_font_style = axis_label_text_font_style,
        minor_tick_line_dash = minor_tick_line_dash, formatter = formatter,
        major_tick_line_width = major_tick_line_width,
        axis_line_dash = axis_line_dash, axis_label = axis_label,
        minor_tick_line_color = minor_tick_line_color,
        axis_label_text_font = axis_label_text_font,
        y_range_name = y_range_name, ticker = ticker,
        subscribed_events = subscribed_events,
        axis_line_alpha = axis_line_alpha,
        js_event_callbacks = js_event_callbacks,
        major_label_text_font_style = major_label_text_font_style,
        minor_tick_line_join = minor_tick_line_join,
        major_label_text_font = major_label_text_font,
        minor_tick_out = minor_tick_out,
        major_label_text_line_height = major_label_text_line_height,
        major_tick_in = major_tick_in,
        major_label_text_font_size = major_label_text_font_size,
        axis_label_text_alpha = axis_label_text_alpha,
        minor_tick_line_dash_offset = minor_tick_line_dash_offset,
        plot = plot, tags = tags,
        axis_label_text_font_size = axis_label_text_font_size,
        major_tick_out = major_tick_out,
        axis_label_text_color = axis_label_text_color,
        minor_tick_line_alpha = minor_tick_line_alpha,
        major_label_text_baseline = major_label_text_baseline,
        major_label_text_alpha = major_label_text_alpha,
        major_label_overrides = major_label_overrides,
        axis_line_dash_offset = axis_line_dash_offset, bounds = bounds,
        x_range_name = x_range_name,
        major_tick_line_alpha = major_tick_line_alpha,
        major_label_text_color = major_label_text_color,
        axis_line_color = axis_line_color, axis_line_cap = axis_line_cap,
        major_label_text_align = major_label_text_align,
        major_label_orientation = major_label_orientation,
        axis_label_standoff = axis_label_standoff,
        js_property_callbacks = js_property_callbacks, name = name,
        axis_label_text_align = axis_label_text_align, visible = visible,
        minor_tick_line_width = minor_tick_line_width,
        major_tick_line_join = major_tick_line_join,
        minor_tick_line_cap = minor_tick_line_cap,
        minor_tick_in = minor_tick_in,
        axis_label_text_line_height = axis_label_text_line_height,
        axis_label_text_baseline = axis_label_text_baseline,
        major_label_standoff = major_label_standoff,
        axis_line_width = axis_line_width, level = level, id = id)
      types <- bk_prop_types[["ContinuousAxis"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Render a square marker, optionally rotated.
# 
# Example -------
# 
# .. bokeh-plot:: ../examples/reference/models/Square.py
# :source-position: below
Square <- R6::R6Class("Square",
  inherit = Marker,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      line_join = "miter", x = NULL, size = 4L, line_width = 1L,
      line_cap = "butt", fill_alpha = 1, y = NULL, angle = 0, line_dash = list(),
      fill_color = "gray", line_color = "black", tags = list(),
      subscribed_events = list(), line_alpha = 1, angle_units = "rad",
      name = NULL, line_dash_offset = 0L, id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks,
        line_join = line_join, x = x, size = size, line_width = line_width,
        line_cap = line_cap, fill_alpha = fill_alpha, y = y, angle = angle,
        line_dash = line_dash, fill_color = fill_color,
        line_color = line_color, tags = tags,
        subscribed_events = subscribed_events, line_alpha = line_alpha,
        angle_units = angle_units, name = name,
        line_dash_offset = line_dash_offset, id = id)
      types <- bk_prop_types[["Square"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Render a single patch.
# 
# The ``Patch`` glyph is different from most other glyphs in that the
# vector of values only produces one glyph on the Plot.
# 
# Example -------
# 
# .. bokeh-plot:: ../examples/reference/models/Patch.py :source-position:
# below
Patch <- R6::R6Class("Patch",
  inherit = XYGlyph,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      line_join = "miter", x = NULL, line_width = 1L, line_cap = "butt",
      y = NULL, fill_alpha = 1, line_dash = list(), fill_color = "gray",
      line_color = "black", tags = list(), subscribed_events = list(),
      line_alpha = 1, name = NULL, line_dash_offset = 0L, id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Patch"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The line join values for the patch.
    # > Enum('miter', 'round', 'bevel')
    line_join = NULL,
    # The x-coordinates for the points of the patch.
    # 
    # .. note:: A patch may comprise multiple polygons. In this case the
    # x-coordinates for each polygon should be separated by NaN values in the
    # sequence.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    x = NULL,
    # The line width values for the patch.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_width = NULL,
    # The line cap values for the patch.
    # > Enum('butt', 'round', 'square')
    line_cap = NULL,
    # The y-coordinates for the points of the patch.
    # 
    # .. note:: A patch may comprise multiple polygons. In this case the
    # y-coordinates for each polygon should be separated by NaN values in the
    # sequence.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    y = NULL,
    # The fill alpha values for the patch.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    fill_alpha = NULL,
    # The line dash values for the patch.
    # > DashPattern
    line_dash = NULL,
    # The fill color values for the patch.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    fill_color = NULL,
    # The line color values for the patch.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    line_color = NULL,
    # The line alpha values for the patch.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_alpha = NULL,
    # The line dash offset values for the patch.
    # > Int
    line_dash_offset = NULL
  )
)

# The TMSTileSource contains tile config info and provides urls for tiles
# based on a templated url e.g.
# ``http://your.tms.server.host/{Z}/{X}/{Y}.png``.  The defining feature
# of TMS is the tile-origin in located at the bottom-left.
# 
# The TMSTileSource can also be helpful in implementing tile renderers
# for custom tile sets, including non-spatial datasets.
TMSTileSource <- R6::R6Class("TMSTileSource",
  inherit = MercatorTileSource,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      extra_url_vars = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      max_zoom = 30L, min_zoom = 0L, tile_size = 256L,
      x_origin_offset = 20037508.34, y_origin_offset = 20037508.34,
      tags = list(), attribution = "", url = "", subscribed_events = list(),
      initial_resolution = 156543.033928041, name = NULL, wrap_around = TRUE,
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        extra_url_vars = extra_url_vars,
        js_property_callbacks = js_property_callbacks, max_zoom = max_zoom,
        min_zoom = min_zoom, tile_size = tile_size,
        x_origin_offset = x_origin_offset,
        y_origin_offset = y_origin_offset, tags = tags,
        attribution = attribution, url = url,
        subscribed_events = subscribed_events,
        initial_resolution = initial_resolution, name = name,
        wrap_around = wrap_around, id = id)
      types <- bk_prop_types[["TMSTileSource"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Generate evenly spaced ticks at a fixed interval regardless of scale.
SingleIntervalTicker <- R6::R6Class("SingleIntervalTicker",
  inherit = ContinuousTicker,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      num_minor_ticks = 5L, desired_num_ticks = 6L, tags = list(),
      subscribed_events = list(), interval = NULL, name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        name = name, num_minor_ticks = num_minor_ticks, tags = tags,
        subscribed_events = subscribed_events,
        desired_num_ticks = desired_num_ticks,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["SingleIntervalTicker"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The interval between adjacent ticks.
    # > Float
    interval = NULL
  )
)

# Map categories to colors. Values that are passed to this mapper that
# aren't in factors will be assigned the nan_color.
CategoricalColorMapper <- R6::R6Class("CategoricalColorMapper",
  inherit = ColorMapper,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      end = NULL, nan_color = "gray", tags = list(),
      subscribed_events = list(), palette = NULL, start = 0L, factors = NULL,
      name = NULL, js_property_callbacks = structure(list(), .Names =
      character(0)), id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        nan_color = nan_color, tags = tags,
        subscribed_events = subscribed_events, palette = palette,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["CategoricalColorMapper"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # A start index to "slice" data factors with before color mapping.
    # 
    # For example, if the data to color map consists of 2-level factors such
    # as ``["2016", "sales"]`` and ``["2017", "marketing"]``, then setting
    # ``end=1`` will perform color mapping only based on the first sub-factor
    # (i.e. in this case based on the year ``"2016"`` or ``"2017"``)
    # 
    # If ``None`` then all sub-factors from ``start`` to the end of the
    # factor will be used for color mapping.
    # > Int
    end = NULL,
    # A start index to "slice" data factors with before color mapping.
    # 
    # For example, if the data to color map consists of 2-level factors such
    # as ``["2016", "sales"]`` and ``["2016", "marketing"]``, then setting
    # ``start=1`` will perform color mapping only based on the second
    # sub-factor (i.e. in this case based on the department ``"sales"`` or
    # ``"marketing"``)
    # > Int
    start = NULL,
    # A sequence of factors / categories that map to the color palette. For
    # example the following color mapper:
    # 
    # .. code-block:: python
    # 
    # mapper = CategoricalColorMapper(palette=["red", "blue"],
    # factors=["foo", "bar"])
    # 
    # will map the factor ``"foo"`` to red and the factor ``"bar"`` to blue.
    # > Either(Seq(String), Seq(Tuple(String, String)), Seq(Tuple(String, String, String)))
    factors = NULL
  )
)

# The ``WMTSTileSource`` behaves much like ``TMSTileSource`` but has its
# tile-origin in the top-left.
# 
# This is the most common used tile source for web mapping applications.
# Such companies as Google, MapQuest, Stamen, Esri, and OpenStreetMap
# provide service which use the WMTS specification e.g.
# ``http://c.tile.openstreetmap.org/{Z}/{X}/{Y}.png``.
WMTSTileSource <- R6::R6Class("WMTSTileSource",
  inherit = MercatorTileSource,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      extra_url_vars = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      max_zoom = 30L, min_zoom = 0L, tile_size = 256L,
      x_origin_offset = 20037508.34, y_origin_offset = 20037508.34,
      tags = list(), attribution = "", url = "", subscribed_events = list(),
      initial_resolution = 156543.033928041, name = NULL, wrap_around = TRUE,
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        extra_url_vars = extra_url_vars,
        js_property_callbacks = js_property_callbacks, max_zoom = max_zoom,
        min_zoom = min_zoom, tile_size = tile_size,
        x_origin_offset = x_origin_offset,
        y_origin_offset = y_origin_offset, tags = tags,
        attribution = attribution, url = url,
        subscribed_events = subscribed_events,
        initial_resolution = initial_resolution, name = name,
        wrap_around = wrap_around, id = id)
      types <- bk_prop_types[["WMTSTileSource"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# 
LegendItem <- R6::R6Class("LegendItem",
  inherit = Model,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      tags = list(), subscribed_events = list(), renderers = list(),
      label = NULL, name = NULL, js_property_callbacks = structure(list(),
      .Names = character(0)), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["LegendItem"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # A list of the glyph renderers to draw in the legend. If ``label`` is a
    # field, then all data_sources of renderers must be the same.
    # > List(Instance(GlyphRenderer))
    renderers = NULL,
    # A label for this legend. Can be a string, or a column of a
    # ColumnDataSource. If ``label`` is a field, then it must be in the
    # renderers' data_source.
    # > StringSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), List(String))), List(String))
    label = NULL
  )
)

# A Bokeh Plot with a `Google Map`_ displayed underneath.
# 
# Data placed on this plot should be specified in decimal lat long
# coordinates e.g. 37.123, -122.404.  It will be automatically converted
# into the web mercator projection to display properly over google maps
# tiles.
# 
# .. _Google Map: https://www.google.com/maps/
GMapPlot <- R6::R6Class("GMapPlot",
  inherit = MapPlot,
  public = list(
    specified_args = NULL,
    initialize = function(
      right = list(), toolbar_sticky = TRUE, title_location = "above",
      x_scale = NULL, min_border_right = NULL, outline_line_alpha = 1,
      inner_width = NULL, background_fill_color = "#ffffff", title = NULL,
      above = list(), subscribed_events = list(),
      outline_line_color = "#e5e5e5", map_options = NULL,
      outline_line_dash = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), toolbar_location = "right",
      layout_width = NULL, y_scale = NULL, width = NULL, renderers = list(),
      toolbar = NULL, x_range = NULL, inner_height = NULL,
      outline_line_cap = "butt", plot_height = 600L, y_range = NULL,
      tags = list(), min_border_top = NULL, layout_height = NULL,
      outline_line_width = 1L, aspect_scale = 1L,
      outline_line_dash_offset = 0L, min_border_bottom = NULL,
      output_backend = "canvas", lod_threshold = 2000L, lod_factor = 10L,
      lod_interval = 300L, border_fill_alpha = 1, background_fill_alpha = 1,
      hidpi = TRUE, outline_line_join = "miter", left = list(),
      sizing_mode = "fixed", border_fill_color = "#ffffff",
      css_classes = NULL, min_border_left = NULL, h_symmetry = TRUE,
      min_border = 5L, js_property_callbacks = structure(list(), .Names =
      character(0)), name = NULL, lod_timeout = 500L,
      extra_x_ranges = structure(list(), .Names = character(0)),
      plot_width = 600L, below = list(), height = NULL, disabled = FALSE,
      api_key = NULL, extra_y_ranges = structure(list(), .Names =
      character(0)), match_aspect = FALSE, v_symmetry = FALSE, id = NULL
    ) {
      super$initialize(right = right, toolbar_sticky = toolbar_sticky,
        title_location = title_location, x_scale = x_scale,
        min_border_right = min_border_right,
        outline_line_alpha = outline_line_alpha, inner_width = inner_width,
        background_fill_color = background_fill_color, title = title,
        above = above, subscribed_events = subscribed_events,
        outline_line_color = outline_line_color,
        outline_line_dash = outline_line_dash,
        js_event_callbacks = js_event_callbacks,
        toolbar_location = toolbar_location, layout_width = layout_width,
        y_scale = y_scale, width = width, renderers = renderers,
        toolbar = toolbar, x_range = x_range,
        outline_line_cap = outline_line_cap, plot_height = plot_height,
        y_range = y_range, tags = tags, min_border_top = min_border_top,
        layout_height = layout_height,
        outline_line_width = outline_line_width,
        aspect_scale = aspect_scale,
        outline_line_dash_offset = outline_line_dash_offset,
        min_border_bottom = min_border_bottom,
        output_backend = output_backend, lod_threshold = lod_threshold,
        v_symmetry = v_symmetry, lod_factor = lod_factor,
        lod_interval = lod_interval, border_fill_alpha = border_fill_alpha,
        background_fill_alpha = background_fill_alpha, hidpi = hidpi,
        outline_line_join = outline_line_join, left = left,
        sizing_mode = sizing_mode, border_fill_color = border_fill_color,
        css_classes = css_classes, min_border_left = min_border_left,
        h_symmetry = h_symmetry, min_border = min_border,
        js_property_callbacks = js_property_callbacks, name = name,
        lod_timeout = lod_timeout, extra_x_ranges = extra_x_ranges,
        plot_width = plot_width, below = below, height = height,
        disabled = disabled, extra_y_ranges = extra_y_ranges,
        match_aspect = match_aspect, inner_height = inner_height, id = id)
      types <- bk_prop_types[["GMapPlot"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Options for displaying the plot.
    # > Instance(GMapOptions)
    map_options = NULL,
    # Google Maps API requires an API key. See
    # https://developers.google.com/maps/documentation/javascript/get-api-key
    # for more information on how to obtain your own.
    # > String
    api_key = NULL
  )
)

# Render images given as scalar data together with a color mapper.
# 
# In addition to the defined model properties, ``Image`` also can accept
# a keyword argument ``palette`` in place of an explicit
# ``color_mapper``.  The value should be a list of colors, or the name of
# one of the built-in palettes in ``bokeh.palettes``. This palette will
# be used to automatically construct a ``ColorMapper`` model for the
# ``color_mapper`` property.
# 
# If both ``palette`` and ``color_mapper`` are passed, a ``ValueError``
# exception will be raised. If neither is passed, then the ``Greys9``
# palette will be used as a default.
Image <- R6::R6Class("Image",
  inherit = XYGlyph,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      dw = NULL, x = NULL, dh = NULL, dilate = FALSE, color_mapper = NULL,
      image = NULL, y = NULL, dh_units = "data", tags = list(),
      subscribed_events = list(), dw_units = "data", name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Image"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The widths of the plot regions that the images will occupy.
    # 
    # .. note:: This is not the number of pixels that an image is wide.  That
    # number is fixed by the image itself.
    # > DistanceSpec(units_default='data')
    dw = NULL,
    # The x-coordinates to locate the image anchors.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    x = NULL,
    # The height of the plot region that the image will occupy.
    # 
    # .. note:: This is not the number of pixels that an image is tall.  That
    # number is fixed by the image itself.
    # > DistanceSpec(units_default='data')
    dh = NULL,
    # Whether to always round fractional pixel locations in such a way as to
    # make the images bigger.
    # 
    # This setting may be useful if pixel rounding errors are causing images
    # to have a gap between them, when they should appear flush.
    # > Bool
    dilate = NULL,
    # A ``ColorMapper`` to use to map the scalar data from ``image`` into
    # RGBA values for display.
    # 
    # .. note:: The color mapping step happens on the client.
    # > Instance(ColorMapper)
    color_mapper = NULL,
    # The arrays of scalar data for the images to be colormapped.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    image = NULL,
    # The y-coordinates to locate the image anchors.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    y = NULL,
    # 
    # > Enum('screen', 'data')
    dh_units = NULL,
    # 
    # > Enum('screen', 'data')
    dw_units = NULL
  )
)

# Base class for continuous color mapper types.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
ContinuousColorMapper <- R6::R6Class("ContinuousColorMapper",
  inherit = ColorMapper,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      high = NULL, low_color = NULL, high_color = NULL, nan_color = "gray",
      tags = list(), subscribed_events = list(), palette = NULL, low = NULL,
      name = NULL, js_property_callbacks = structure(list(), .Names =
      character(0)), id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        nan_color = nan_color, tags = tags,
        subscribed_events = subscribed_events, palette = palette,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["ContinuousColorMapper"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The maximum value of the range to map into the palette. Values above
    # this are clamped to ``high``.
    # > Float
    high = NULL,
    # Color to be used if data is lower than ``low`` value. If None, values
    # lower than ``low`` are mapped to the first color in the palette.
    # > Color
    low_color = NULL,
    # Color to be used if data is lower than ``high`` value. If None, values
    # lower than ``high`` are mapped to the last color in the palette.
    # > Color
    high_color = NULL,
    # The minimum value of the range to map into the palette. Values below
    # this are clamped to ``low``.
    # > Float
    low = NULL
  )
)

# Render several lines.
# 
# The data for the ``MultiLine`` glyph is different in that the vector of
# values is not a vector of scalars. Rather, it is a "list of lists".
# 
# Example -------
# 
# .. bokeh-plot:: ../examples/reference/models/MultiLine.py
# :source-position: below
MultiLine <- R6::R6Class("MultiLine",
  inherit = Glyph,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      line_join = "miter", xs = NULL, line_width = 1L, line_cap = "butt",
      line_dash = list(), ys = NULL, line_color = "black", tags = list(),
      subscribed_events = list(), line_alpha = 1, name = NULL,
      line_dash_offset = 0L, id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["MultiLine"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The line join values for the lines.
    # > Enum('miter', 'round', 'bevel')
    line_join = NULL,
    # The x-coordinates for all the lines, given as a "list of lists".
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    xs = NULL,
    # The line width values for the lines.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_width = NULL,
    # The line cap values for the lines.
    # > Enum('butt', 'round', 'square')
    line_cap = NULL,
    # The line dash values for the lines.
    # > DashPattern
    line_dash = NULL,
    # The y-coordinates for all the lines, given as a "list of lists".
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    ys = NULL,
    # The line color values for the lines.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    line_color = NULL,
    # The line alpha values for the lines.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_alpha = NULL,
    # The line dash offset values for the lines.
    # > Int
    line_dash_offset = NULL
  )
)

# Base class for all annotation models.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
Annotation <- R6::R6Class("Annotation",
  inherit = Renderer,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      visible = TRUE, name = NULL, plot = NULL, tags = list(),
      subscribed_events = list(), level = "annotation",
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        name = name, visible = visible, tags = tags,
        subscribed_events = subscribed_events, level = level,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["Annotation"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The plot to which this annotation is attached.
    # > Instance(Plot)
    plot = NULL
  )
)

# Filter data sources with a custom defined JavaScript function.
CustomJSFilter <- R6::R6Class("CustomJSFilter",
  inherit = Filter,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      filter = NULL, code = "", tags = list(), args = structure(list(), .Names
      = character(0)), subscribed_events = list(), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        filter = filter, tags = tags, subscribed_events = subscribed_events,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["CustomJSFilter"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # A snippet of JavaScript code to filter data contained in a columnar
    # data source.  The code is made into the body of a function, and all of
    # of the named objects in ``args`` are available as parameters that the
    # code can use. The variable ``source`` will contain the data source that
    # is associated with the CDSView this filter is added to.
    # 
    # The code should either return the indices of the subset or an array of
    # booleans to use to subset data source rows.
    # 
    # Example:
    # 
    # .. code-block:: javascript
    # 
    # code = ''' var indices = []; for (var i = 0; i <=
    # source.data['some_column'].length; i++){ if
    # (source.data['some_column'][i] == 'some_value') { indices.push(i) } }
    # return indices; '''
    # 
    # .. note:: Use ``CustomJS.from_coffeescript()`` for CoffeeScript source
    # code.
    # > String
    code = NULL,
    # A mapping of names to Bokeh plot objects. These objects are made
    # available to the callback code snippet as the values of named
    # parameters to the callback.
    # > Dict(String, Instance(Model))
    args = NULL
  )
)

# Basic string cell editor with auto-completion.
StringEditor <- R6::R6Class("StringEditor",
  inherit = CellEditor,
  public = list(
    specified_args = NULL,
    initialize = function(
      completions = list(), js_event_callbacks = structure(list(), .Names =
      character(0)), tags = list(), subscribed_events = list(), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["StringEditor"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # An optional list of completion strings.
    # > List(String)
    completions = NULL
  )
)

# Base class for all glyph models.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
Glyph <- R6::R6Class("Glyph",
  inherit = Model,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Glyph"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Render images given as RGBA data.
ImageRGBA <- R6::R6Class("ImageRGBA",
  inherit = XYGlyph,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      dw = NULL, x = NULL, dh = NULL, dilate = FALSE, image = NULL, y = NULL,
      dh_units = "data", tags = list(), subscribed_events = list(),
      dw_units = "data", name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["ImageRGBA"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The widths of the plot regions that the images will occupy.
    # 
    # .. note:: This is not the number of pixels that an image is wide.  That
    # number is fixed by the image itself.
    # > DistanceSpec(units_default='data')
    dw = NULL,
    # The x-coordinates to locate the image anchors.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    x = NULL,
    # The height of the plot region that the image will occupy.
    # 
    # .. note:: This is not the number of pixels that an image is tall.  That
    # number is fixed by the image itself.
    # > DistanceSpec(units_default='data')
    dh = NULL,
    # Whether to always round fractional pixel locations in such a way as to
    # make the images bigger.
    # 
    # This setting may be useful if pixel rounding errors are causing images
    # to have a gap between them, when they should appear flush.
    # > Bool
    dilate = NULL,
    # The arrays of RGBA data for the images.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    image = NULL,
    # The y-coordinates to locate the image anchors.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    y = NULL,
    # 
    # > Enum('screen', 'data')
    dh_units = NULL,
    # 
    # > Enum('screen', 'data')
    dw_units = NULL
  )
)

# Base class for color mapper types.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
ColorMapper <- R6::R6Class("ColorMapper",
  inherit = Transform,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      nan_color = "gray", tags = list(), subscribed_events = list(),
      palette = NULL, name = NULL, js_property_callbacks = structure(list(),
      .Names = character(0)), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["ColorMapper"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Color to be used if data is NaN. Default: 'gray'
    # > Color
    nan_color = NULL,
    # A sequence of colors to use as the target palette for mapping.
    # 
    # This property can also be set as a ``String``, to the name of any of
    # the palettes shown in :ref:`bokeh.palettes`.
    # > Seq(Color)
    palette = NULL
  )
)

# Render vertical bars, given a center coordinate, width and (top,
# bottom) coordinates.
# 
# Example -------
# 
# .. bokeh-plot:: ../examples/reference/models/VBar.py :source-position:
# below
VBar <- R6::R6Class("VBar",
  inherit = Glyph,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      line_join = "miter", width = NULL, x = NULL, line_width = 1L,
      line_cap = "butt", fill_alpha = 1, bottom = 0L, line_dash = list(),
      top = NULL, fill_color = "gray", line_color = "black", tags = list(),
      subscribed_events = list(), line_alpha = 1, name = NULL,
      line_dash_offset = 0L, id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["VBar"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The line join values for the vertical bars.
    # > Enum('miter', 'round', 'bevel')
    line_join = NULL,
    # The widths of the vertical bars.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    width = NULL,
    # The x-coordinates of the centers of the vertical bars.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    x = NULL,
    # The line width values for the vertical bars.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_width = NULL,
    # The line cap values for the vertical bars.
    # > Enum('butt', 'round', 'square')
    line_cap = NULL,
    # The fill alpha values for the vertical bars.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    fill_alpha = NULL,
    # The y-coordinates of the bottom edges.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    bottom = NULL,
    # The line dash values for the vertical bars.
    # > DashPattern
    line_dash = NULL,
    # The y-coordinates of the top edges.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    top = NULL,
    # The fill color values for the vertical bars.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    fill_color = NULL,
    # The line color values for the vertical bars.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    line_color = NULL,
    # The line alpha values for the vertical bars.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_alpha = NULL,
    # The line dash offset values for the vertical bars.
    # > Int
    line_dash_offset = NULL
  )
)

# A base class for Mercator tile services (e.g.``WMTSTileSource``).
MercatorTileSource <- R6::R6Class("MercatorTileSource",
  inherit = TileSource,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      extra_url_vars = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      max_zoom = 30L, min_zoom = 0L, tile_size = 256L,
      x_origin_offset = 20037508.34, y_origin_offset = 20037508.34,
      tags = list(), attribution = "", url = "", subscribed_events = list(),
      initial_resolution = 156543.033928041, name = NULL, wrap_around = TRUE,
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        extra_url_vars = extra_url_vars, max_zoom = max_zoom,
        min_zoom = min_zoom, tile_size = tile_size,
        x_origin_offset = x_origin_offset,
        y_origin_offset = y_origin_offset, tags = tags,
        attribution = attribution, url = url,
        subscribed_events = subscribed_events,
        initial_resolution = initial_resolution, name = name,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["MercatorTileSource"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Enables continuous horizontal panning by wrapping the x-axis based on
    # bounds of map.
    # 
    # ..note:: Axis coordinates are not wrapped. To toggle axis label
    # visibility, use ``plot.axis.visible = False``.
    # > Bool
    wrap_around = NULL
  )
)

# An LinearAxis that picks nice numbers for tick locations on a datetime
# scale. Configured with a ``DatetimeTickFormatter`` by default.
DatetimeAxis <- R6::R6Class("DatetimeAxis",
  inherit = LinearAxis,
  public = list(
    specified_args = NULL,
    initialize = function(
      major_tick_line_color = "black", major_tick_line_dash_offset = 0L,
      major_tick_line_dash = list(), major_tick_line_cap = "butt",
      axis_line_join = "miter", axis_label_text_font_style = "italic",
      minor_tick_line_dash = list(), formatter = NULL,
      major_tick_line_width = 1L, axis_line_dash = list(), axis_label = "",
      minor_tick_line_color = "black", axis_label_text_font = "helvetica",
      y_range_name = "default", ticker = NULL, subscribed_events = list(),
      axis_line_alpha = 1, js_event_callbacks = structure(list(), .Names =
      character(0)), major_label_text_font_style = "normal",
      minor_tick_line_join = "miter", major_label_text_font = "helvetica",
      minor_tick_out = 4L, major_label_text_line_height = 1.2,
      major_tick_in = 2L, major_label_text_font_size = list(value = "8pt"),
      axis_label_text_alpha = 1, minor_tick_line_dash_offset = 0L,
      plot = NULL, tags = list(), axis_label_text_font_size = list(value =
      "10pt"), major_tick_out = 6L, axis_label_text_color = "#444444",
      minor_tick_line_alpha = 1, major_label_text_baseline = "alphabetic",
      major_label_text_alpha = 1, major_label_overrides = structure(list(),
      .Names = character(0)), axis_line_dash_offset = 0L, bounds = "auto",
      x_range_name = "default", major_tick_line_alpha = 1,
      major_label_text_color = "#444444", axis_line_color = "black",
      axis_line_cap = "butt", major_label_text_align = "center",
      major_label_orientation = "horizontal", axis_label_standoff = 5L,
      js_property_callbacks = structure(list(), .Names = character(0)),
      name = NULL, axis_label_text_align = "left", visible = TRUE,
      minor_tick_line_width = 1L, major_tick_line_join = "miter",
      minor_tick_line_cap = "butt", minor_tick_in = 0L,
      axis_label_text_line_height = 1.2,
      axis_label_text_baseline = "bottom", major_label_standoff = 5L,
      axis_line_width = 1L, level = "overlay", id = NULL
    ) {
      super$initialize(major_tick_line_color = major_tick_line_color,
        major_tick_line_dash_offset = major_tick_line_dash_offset,
        major_tick_line_dash = major_tick_line_dash,
        major_tick_line_cap = major_tick_line_cap,
        axis_line_join = axis_line_join,
        axis_label_text_font_style = axis_label_text_font_style,
        minor_tick_line_dash = minor_tick_line_dash, formatter = formatter,
        major_tick_line_width = major_tick_line_width,
        axis_line_dash = axis_line_dash, axis_label = axis_label,
        minor_tick_line_color = minor_tick_line_color,
        axis_label_text_font = axis_label_text_font,
        y_range_name = y_range_name, ticker = ticker,
        subscribed_events = subscribed_events,
        axis_line_alpha = axis_line_alpha,
        js_event_callbacks = js_event_callbacks,
        major_label_text_font_style = major_label_text_font_style,
        minor_tick_line_join = minor_tick_line_join,
        major_label_text_font = major_label_text_font,
        minor_tick_out = minor_tick_out,
        major_label_text_line_height = major_label_text_line_height,
        major_tick_in = major_tick_in,
        major_label_text_font_size = major_label_text_font_size,
        axis_label_text_alpha = axis_label_text_alpha,
        minor_tick_line_dash_offset = minor_tick_line_dash_offset,
        plot = plot, tags = tags,
        axis_label_text_font_size = axis_label_text_font_size,
        major_tick_out = major_tick_out,
        axis_label_text_color = axis_label_text_color,
        minor_tick_line_alpha = minor_tick_line_alpha,
        major_label_text_baseline = major_label_text_baseline,
        major_label_text_alpha = major_label_text_alpha,
        major_label_overrides = major_label_overrides,
        axis_line_dash_offset = axis_line_dash_offset, bounds = bounds,
        x_range_name = x_range_name,
        major_tick_line_alpha = major_tick_line_alpha,
        major_label_text_color = major_label_text_color,
        axis_line_color = axis_line_color, axis_line_cap = axis_line_cap,
        major_label_text_align = major_label_text_align,
        major_label_orientation = major_label_orientation,
        axis_label_standoff = axis_label_standoff,
        js_property_callbacks = js_property_callbacks, name = name,
        axis_label_text_align = axis_label_text_align, visible = visible,
        minor_tick_line_width = minor_tick_line_width,
        major_tick_line_join = major_tick_line_join,
        minor_tick_line_cap = minor_tick_line_cap,
        minor_tick_in = minor_tick_in,
        axis_label_text_line_height = axis_label_text_line_height,
        axis_label_text_baseline = axis_label_text_baseline,
        major_label_standoff = major_label_standoff,
        axis_line_width = axis_line_width, level = level, id = id)
      types <- bk_prop_types[["DatetimeAxis"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# A base class for all guide renderer types. ``GuideRenderer`` is not
# generally useful to instantiate on its own.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
GuideRenderer <- R6::R6Class("GuideRenderer",
  inherit = Renderer,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      visible = TRUE, name = NULL, plot = NULL, tags = list(),
      subscribed_events = list(), level = "overlay",
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        name = name, visible = visible, tags = tags,
        subscribed_events = subscribed_events, level = level,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["GuideRenderer"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The plot to which this guide renderer is attached.
    # > Instance(Plot)
    plot = NULL
  )
)

# Generate "nice" round ticks at any magnitude.
# 
# Creates ticks that are "base" multiples of a set of given mantissas.
# For example, with ``base=10`` and ``mantissas=[1, 2, 5]``, the ticker
# will generate the sequence::
# 
# ..., 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, 100, ...
AdaptiveTicker <- R6::R6Class("AdaptiveTicker",
  inherit = ContinuousTicker,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      mantissas = list(1L, 2L, 5L), base = 10, num_minor_ticks = 5L,
      min_interval = 0, desired_num_ticks = 6L, max_interval = NULL,
      tags = list(), subscribed_events = list(), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        name = name, num_minor_ticks = num_minor_ticks, tags = tags,
        subscribed_events = subscribed_events,
        desired_num_ticks = desired_num_ticks,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["AdaptiveTicker"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The acceptable list numbers to generate multiples of.
    # > Seq(Float)
    mantissas = NULL,
    # The multiplier to use for scaling mantissas.
    # > Float
    base = NULL,
    # The smallest allowable interval between two adjacent ticks.
    # > Float
    min_interval = NULL,
    # The largest allowable interval between two adjacent ticks.
    # 
    # .. note:: To specify an unbounded interval, set to ``None``.
    # > Float
    max_interval = NULL
  )
)

# *toolbar icon*: |zoom_in_icon|
# 
# The zoom-in tool allows users to click a button to zoom in by a fixed
# amount.
# 
# .. |zoom_in_icon| image:: /_images/icons/ZoomIn.png :height: 18pt
ZoomInTool <- R6::R6Class("ZoomInTool",
  inherit = Action,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      tags = list(), dimensions = "both", subscribed_events = list(),
      factor = 0.1, name = NULL, js_property_callbacks = structure(list(),
      .Names = character(0)), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["ZoomInTool"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Which dimensions the zoom-in tool is constrained to act in. By default
    # the zoom-in zoom tool will zoom in any dimension, but can be configured
    # to only zoom horizontally across the width of the plot, or vertically
    # across the height of the plot.
    # > Enum('width', 'height', 'both')
    dimensions = NULL,
    # Percentage to zoom for each click of the zoom-in tool.
    # > Percent
    factor = NULL
  )
)

# Render diamond markers.
# 
# Example -------
# 
# .. bokeh-plot:: ../examples/reference/models/Diamond.py
# :source-position: below
Diamond <- R6::R6Class("Diamond",
  inherit = Marker,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      line_join = "miter", x = NULL, size = 4L, line_width = 1L,
      line_cap = "butt", fill_alpha = 1, y = NULL, angle = 0, line_dash = list(),
      fill_color = "gray", line_color = "black", tags = list(),
      subscribed_events = list(), line_alpha = 1, angle_units = "rad",
      name = NULL, line_dash_offset = 0L, id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks,
        line_join = line_join, x = x, size = size, line_width = line_width,
        line_cap = line_cap, fill_alpha = fill_alpha, y = y, angle = angle,
        line_dash = line_dash, fill_color = fill_color,
        line_color = line_color, tags = tags,
        subscribed_events = subscribed_events, line_alpha = line_alpha,
        angle_units = angle_units, name = name,
        line_dash_offset = line_dash_offset, id = id)
      types <- bk_prop_types[["Diamond"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Multi-line string cell editor.
TextEditor <- R6::R6Class("TextEditor",
  inherit = CellEditor,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["TextEditor"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# A dropdown button.
Dropdown <- R6::R6Class("Dropdown",
  inherit = AbstractButton,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      menu = list(), icon = NULL, default_value = NULL, width = NULL,
      height = NULL, disabled = FALSE, callback = NULL, sizing_mode = "fixed",
      tags = list(), subscribed_events = list(), css_classes = NULL,
      label = "Dropdown", button_type = "default", name = NULL, value = NULL,
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        icon = icon, width = width, height = height, disabled = disabled,
        callback = callback, sizing_mode = sizing_mode, tags = tags,
        subscribed_events = subscribed_events, css_classes = css_classes,
        label = label, button_type = button_type, name = name,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["Dropdown"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Button's dropdown menu consisting of entries containing item's text and
    # value name. Use ``None`` as a menu separator.
    # > List(Tuple(String, String))
    menu = NULL,
    # The default value, otherwise the first item in ``menu`` will be used.
    # > String
    default_value = NULL,
    # A private property used to trigger ``on_click`` event handler.
    # > String
    value = NULL
  )
)

# Render rays.
# 
# Example -------
# 
# .. bokeh-plot:: ../examples/reference/models/Ray.py :source-position:
# below
Ray <- R6::R6Class("Ray",
  inherit = XYGlyph,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      line_join = "miter", length = NULL, x = NULL, line_width = 1L,
      line_cap = "butt", y = NULL, angle = NULL, line_dash = list(),
      length_units = "data", line_color = "black", tags = list(),
      subscribed_events = list(), line_alpha = 1, angle_units = "rad",
      name = NULL, line_dash_offset = 0L, id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Ray"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The line join values for the rays.
    # > Enum('miter', 'round', 'bevel')
    line_join = NULL,
    # The length to extend the ray. Note that this ``length`` defaults to
    # screen units.
    # > DistanceSpec(units_default='data')
    length = NULL,
    # The x-coordinates to start the rays.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    x = NULL,
    # The line width values for the rays.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_width = NULL,
    # The line cap values for the rays.
    # > Enum('butt', 'round', 'square')
    line_cap = NULL,
    # The y-coordinates to start the rays.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    y = NULL,
    # The angles in radians to extend the rays, as measured from the
    # horizontal.
    # > AngleSpec(units_default='rad')
    angle = NULL,
    # The line dash values for the rays.
    # > DashPattern
    line_dash = NULL,
    # 
    # > Enum('screen', 'data')
    length_units = NULL,
    # The line color values for the rays.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    line_color = NULL,
    # The line alpha values for the rays.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_alpha = NULL,
    # 
    # > Enum('deg', 'rad')
    angle_units = NULL,
    # The line dash offset values for the rays.
    # > Int
    line_dash_offset = NULL
  )
)

# Generate nice ticks across different date and time scales.
DatetimeTicker <- R6::R6Class("DatetimeTicker",
  inherit = CompositeTicker,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      num_minor_ticks = 0L, tickers = list("{\"id\":
      \"a0bbcd9e-2160-436b-8a2c-985399b98e8c\", \"mantissas\": [1, 2,
      5], \"max_interval\": 500.0, \"num_minor_ticks\": 0}",
      "{\"base\": 60, \"id\": \"de05be91-37f8-4673-8e1d-42691d328f32\",
      \"mantissas\": [1, 2, 5, 10, 15, 20, 30], \"max_interval\":
      1800000.0, \"min_interval\": 1000.0, \"num_minor_ticks\": 0}",
      "{\"base\": 24, \"id\": \"2cdf799f-ec90-49d6-9325-bbfcda5f56c9\",
      \"mantissas\": [1, 2, 4, 6, 8, 12], \"max_interval\": 43200000.0,
      \"min_interval\": 3600000.0, \"num_minor_ticks\": 0}",
      "{\"days\": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
      16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31],
      \"id\": \"95ab64b9-19a7-4a20-8464-cadd3b0398f3\"}", "{\"days\":
      [1, 4, 7, 10, 13, 16, 19, 22, 25, 28], \"id\":
      \"8e474ca0-d9f8-46dd-bd20-dd699013c746\"}", "{\"days\": [1, 8,
      15, 22], \"id\": \"e6ed6f38-771b-4330-9679-62b029e53ccc\"}",
      "{\"days\": [1, 15], \"id\":
      \"160ed531-fd94-4ac2-8cb6-b97b381ab378\"}", "{\"id\":
      \"4b2fb932-ea28-48a5-a9be-6dedafbdab4d\", \"months\": [0, 1, 2,
      3, 4, 5, 6, 7, 8, 9, 10, 11]}", "{\"id\":
      \"25f3f152-3a63-4701-b7a6-644a43369042\", \"months\": [0, 2, 4,
      6, 8, 10]}", "{\"id\": \"0833aeda-33b6-43c5-a2ae-be6ea35642ad\",
      \"months\": [0, 4, 8]}", "{\"id\":
      \"72296a40-7290-4c9f-b294-804c0639d0ab\", \"months\": [0, 6]}",
      "{\"id\": \"ef85ffdc-f3c8-48e8-8be4-821a5d5cbc9d\"}"),
      desired_num_ticks = 6L, tags = list(), subscribed_events = list(),
      name = NULL, js_property_callbacks = structure(list(), .Names =
      character(0)), id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        num_minor_ticks = num_minor_ticks, tickers = tickers,
        desired_num_ticks = desired_num_ticks, tags = tags,
        subscribed_events = subscribed_events, name = name,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["DatetimeTicker"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Compute a step-wise interpolation between the points provided through
# the ``x``, ``y``, and ``data`` parameters.
StepInterpolator <- R6::R6Class("StepInterpolator",
  inherit = Interpolator,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      mode = "after", x = NULL, y = NULL, data = NULL, tags = list(),
      subscribed_events = list(), clip = TRUE, name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks, x = x, y = y,
        data = data, tags = tags, subscribed_events = subscribed_events,
        clip = clip, name = name,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["StepInterpolator"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Adjust the behavior of the returned value in relation to the control
    # points.  The parameter can assume one of three values:
    # 
    # * ``after`` (default): Assume the y-value associated with the nearest
    # x-value which is less than or equal to the point to transform.  *
    # ``before``: Assume the y-value associated with the nearest x-value
    # which is greater than the point to transform.  * ``center``: Assume the
    # y-value associated with the nearest x-value to the point to transform.
    # > Enum('before', 'after', 'center')
    mode = NULL
  )
)

# TickFormatter for values in WebMercator units.
# 
# Some map plot types internally use WebMercator to describe coordinates,
# plot bounds, etc. These units are not very human-friendly. This tick
# formatter will convert WebMercator units into Latitude and Longitude
# for display on axes.
MercatorTickFormatter <- R6::R6Class("MercatorTickFormatter",
  inherit = BasicTickFormatter,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      precision = "auto", power_limit_high = 5L, use_scientific = TRUE,
      subscribed_events = list(), tags = list(), power_limit_low = -3L,
      dimension = NULL, name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        precision = precision, power_limit_high = power_limit_high,
        use_scientific = use_scientific,
        subscribed_events = subscribed_events, tags = tags,
        power_limit_low = power_limit_low, name = name,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["MercatorTickFormatter"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Specify whether to format ticks for Latitude or Longitude.
    # 
    # Projected coordinates are not separable, computing Latitude and
    # Longitude tick labels from Web Mercator requires considering
    # coordinates from both dimensions together. Use this property to specify
    # which result should be used for display.
    # 
    # Typically, if the formatter is for an x-axis, then dimension should be
    # ``"lon"`` and if the formatter is for a y-axis, then the dimension
    # should be `"lat"``.
    # 
    # In order to prevent hard to debug errors, there is no default value for
    # dimension. Using an un-configured MercatorTickFormatter will result in
    # a validation error and a JavaScript console error.
    # > Enum('lat', 'lon')
    dimension = NULL
  )
)

# Select cell editor.
SelectEditor <- R6::R6Class("SelectEditor",
  inherit = CellEditor,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      options = list(), tags = list(), subscribed_events = list(), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["SelectEditor"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The list of options to select from.
    # > List(String)
    options = NULL
  )
)

# A group of radio boxes.
RadioGroup <- R6::R6Class("RadioGroup",
  inherit = Group,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      width = NULL, height = NULL, disabled = FALSE, inline = FALSE,
      callback = NULL, sizing_mode = "fixed", tags = list(),
      subscribed_events = list(), active = NULL, labels = list(),
      css_classes = NULL, name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        width = width, height = height, disabled = disabled, inline = inline,
        callback = callback, sizing_mode = sizing_mode,
        subscribed_events = subscribed_events, tags = tags, labels = labels,
        css_classes = css_classes, name = name,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["RadioGroup"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The index of the selected radio box, or ``None`` if nothing is
    # selected.
    # > Int
    active = NULL
  )
)

# A panel widget with navigation tabs.
# 
# Example -------
# 
# .. bokeh-plot::
# ../sphinx/source/docs/user_guide/examples/interaction_tab_panes.py
# :source-position: below
Tabs <- R6::R6Class("Tabs",
  inherit = Widget,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      tabs = list(), width = NULL, height = NULL, disabled = FALSE,
      callback = NULL, tags = list(), sizing_mode = "fixed", active = 0L,
      subscribed_events = list(), css_classes = NULL, name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        width = width, height = height, disabled = disabled,
        sizing_mode = sizing_mode, tags = tags,
        subscribed_events = subscribed_events, css_classes = css_classes,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["Tabs"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The list of child panel widgets.
    # > List(Instance(Panel))
    tabs = NULL,
    # A callback to run in the browser whenever the button is activated.
    # > Instance(Callback)
    callback = NULL,
    # The index of the active tab.
    # > Int
    active = NULL
  )
)

# A base class for different toolbars.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
ToolbarBase <- R6::R6Class("ToolbarBase",
  inherit = LayoutDOM,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      width = NULL, height = NULL, disabled = FALSE, sizing_mode = NULL,
      tools = list(), subscribed_events = list(), tags = list(),
      logo = "normal", css_classes = NULL, name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        width = width, height = height, disabled = disabled,
        sizing_mode = sizing_mode, tags = tags,
        subscribed_events = subscribed_events, css_classes = css_classes,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["ToolbarBase"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # A list of tools to add to the plot.
    # > List(Instance(Tool))
    tools = NULL,
    # What version of the Bokeh logo to display on the toolbar. If set to
    # None, no logo will be displayed.
    # > Enum('normal', 'grey')
    logo = NULL
  )
)

# A click button.
Button <- R6::R6Class("Button",
  inherit = AbstractButton,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      icon = NULL, width = NULL, height = NULL, disabled = FALSE,
      callback = NULL, sizing_mode = "fixed", tags = list(),
      subscribed_events = list(), css_classes = NULL, label = "Button",
      clicks = 0L, button_type = "default", name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        icon = icon, width = width, height = height, disabled = disabled,
        callback = callback, sizing_mode = sizing_mode, tags = tags,
        subscribed_events = subscribed_events, css_classes = css_classes,
        label = label, button_type = button_type, name = name,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["Button"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # A private property used to trigger ``on_click`` event handler.
    # > Int
    clicks = NULL
  )
)

# Base class for Bokeh models that represent HTML markup elements.
# 
# Markups include e.g., ``<div>``, ``<p>``, and ``<pre>``.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
Markup <- R6::R6Class("Markup",
  inherit = Widget,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      text = "", width = NULL, height = NULL, disabled = FALSE,
      sizing_mode = "fixed", tags = list(), subscribed_events = list(),
      css_classes = NULL, style = structure(list(), .Names = character(0)),
      name = NULL, js_property_callbacks = structure(list(), .Names =
      character(0)), id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        width = width, height = height, disabled = disabled,
        sizing_mode = sizing_mode, tags = tags,
        subscribed_events = subscribed_events, css_classes = css_classes,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["Markup"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The contents of the widget.
    # > String
    text = NULL,
    # Raw CSS style declaration. Note this may be web browser dependent.
    # > Dict(String, Any)
    style = NULL
  )
)

# An axis that picks nice numbers for tick locations on a log scale.
# Configured with a ``LogTickFormatter`` by default.
LogAxis <- R6::R6Class("LogAxis",
  inherit = ContinuousAxis,
  public = list(
    specified_args = NULL,
    initialize = function(
      major_tick_line_color = "black", major_tick_line_dash_offset = 0L,
      major_tick_line_dash = list(), major_tick_line_cap = "butt",
      axis_line_join = "miter", axis_label_text_font_style = "italic",
      minor_tick_line_dash = list(), formatter = NULL,
      major_tick_line_width = 1L, axis_line_dash = list(), axis_label = "",
      minor_tick_line_color = "black", axis_label_text_font = "helvetica",
      y_range_name = "default", ticker = NULL, subscribed_events = list(),
      axis_line_alpha = 1, js_event_callbacks = structure(list(), .Names =
      character(0)), major_label_text_font_style = "normal",
      minor_tick_line_join = "miter", major_label_text_font = "helvetica",
      minor_tick_out = 4L, major_label_text_line_height = 1.2,
      major_tick_in = 2L, major_label_text_font_size = list(value = "8pt"),
      axis_label_text_alpha = 1, minor_tick_line_dash_offset = 0L,
      plot = NULL, tags = list(), axis_label_text_font_size = list(value =
      "10pt"), major_tick_out = 6L, axis_label_text_color = "#444444",
      minor_tick_line_alpha = 1, major_label_text_baseline = "alphabetic",
      major_label_text_alpha = 1, major_label_overrides = structure(list(),
      .Names = character(0)), axis_line_dash_offset = 0L, bounds = "auto",
      x_range_name = "default", major_tick_line_alpha = 1,
      major_label_text_color = "#444444", axis_line_color = "black",
      axis_line_cap = "butt", major_label_text_align = "center",
      major_label_orientation = "horizontal", axis_label_standoff = 5L,
      js_property_callbacks = structure(list(), .Names = character(0)),
      name = NULL, axis_label_text_align = "left", visible = TRUE,
      minor_tick_line_width = 1L, major_tick_line_join = "miter",
      minor_tick_line_cap = "butt", minor_tick_in = 0L,
      axis_label_text_line_height = 1.2,
      axis_label_text_baseline = "bottom", major_label_standoff = 5L,
      axis_line_width = 1L, level = "overlay", id = NULL
    ) {
      super$initialize(major_tick_line_color = major_tick_line_color,
        major_tick_line_dash_offset = major_tick_line_dash_offset,
        major_tick_line_dash = major_tick_line_dash,
        major_tick_line_cap = major_tick_line_cap,
        axis_line_join = axis_line_join,
        axis_label_text_font_style = axis_label_text_font_style,
        minor_tick_line_dash = minor_tick_line_dash, formatter = formatter,
        major_tick_line_width = major_tick_line_width,
        axis_line_dash = axis_line_dash, axis_label = axis_label,
        minor_tick_line_color = minor_tick_line_color,
        axis_label_text_font = axis_label_text_font,
        y_range_name = y_range_name, ticker = ticker,
        subscribed_events = subscribed_events,
        axis_line_alpha = axis_line_alpha,
        js_event_callbacks = js_event_callbacks,
        major_label_text_font_style = major_label_text_font_style,
        minor_tick_line_join = minor_tick_line_join,
        major_label_text_font = major_label_text_font,
        minor_tick_out = minor_tick_out,
        major_label_text_line_height = major_label_text_line_height,
        major_tick_in = major_tick_in,
        major_label_text_font_size = major_label_text_font_size,
        axis_label_text_alpha = axis_label_text_alpha,
        minor_tick_line_dash_offset = minor_tick_line_dash_offset,
        plot = plot, tags = tags,
        axis_label_text_font_size = axis_label_text_font_size,
        major_tick_out = major_tick_out,
        axis_label_text_color = axis_label_text_color,
        minor_tick_line_alpha = minor_tick_line_alpha,
        major_label_text_baseline = major_label_text_baseline,
        major_label_text_alpha = major_label_text_alpha,
        major_label_overrides = major_label_overrides,
        axis_line_dash_offset = axis_line_dash_offset, bounds = bounds,
        x_range_name = x_range_name,
        major_tick_line_alpha = major_tick_line_alpha,
        major_label_text_color = major_label_text_color,
        axis_line_color = axis_line_color, axis_line_cap = axis_line_cap,
        major_label_text_align = major_label_text_align,
        major_label_orientation = major_label_orientation,
        axis_label_standoff = axis_label_standoff,
        js_property_callbacks = js_property_callbacks, name = name,
        axis_label_text_align = axis_label_text_align, visible = visible,
        minor_tick_line_width = minor_tick_line_width,
        major_tick_line_join = major_tick_line_join,
        minor_tick_line_cap = minor_tick_line_cap,
        minor_tick_in = minor_tick_in,
        axis_label_text_line_height = axis_label_text_line_height,
        axis_label_text_baseline = axis_label_text_baseline,
        major_label_standoff = major_label_standoff,
        axis_line_width = axis_line_width, level = level, id = id)
      types <- bk_prop_types[["LogAxis"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# A base class that defines common properties for all button types.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
AbstractButton <- R6::R6Class("AbstractButton",
  inherit = Widget,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      icon = NULL, width = NULL, height = NULL, disabled = FALSE,
      callback = NULL, sizing_mode = "fixed", tags = list(),
      subscribed_events = list(), css_classes = NULL, label = "Button",
      button_type = "default", name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        width = width, height = height, disabled = disabled,
        sizing_mode = sizing_mode, tags = tags,
        subscribed_events = subscribed_events, css_classes = css_classes,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["AbstractButton"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # An optional image appearing to the left of button's text.
    # > Instance(AbstractIcon)
    icon = NULL,
    # A callback to run in the browser whenever the button is activated.
    # > Instance(Callback)
    callback = NULL,
    # The text label for the button to display.
    # > String
    label = NULL,
    # A style for the button, signifying it's role.
    # > Enum('default', 'primary', 'success', 'warning', 'danger', 'link')
    button_type = NULL
  )
)

# Map numbers in a range [*low*, *high*] linearly into a sequence of
# colors (a palette).
# 
# For example, if the range is [0, 99] and the palette is ``['red',
# 'green', 'blue']``, the values would be mapped as follows::
# 
# x < 0 : 'red' # values < low are clamped 0 >= x < 33 : 'red' 33 >= x <
# 66 : 'green' 66 >= x < 99 : 'blue' 99 >= x : 'blue' # values > high are
# clamped
LinearColorMapper <- R6::R6Class("LinearColorMapper",
  inherit = ContinuousColorMapper,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      high = NULL, low_color = NULL, high_color = NULL, nan_color = "gray",
      tags = list(), subscribed_events = list(), palette = NULL, low = NULL,
      name = NULL, js_property_callbacks = structure(list(), .Names =
      character(0)), id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        high = high, low_color = low_color, high_color = high_color,
        nan_color = nan_color, tags = tags,
        subscribed_events = subscribed_events, palette = palette, low = low,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["LinearColorMapper"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# A block (paragraph) of pre-formatted text.
# 
# This Bokeh model corresponds to an HTML ``<pre>`` element.
# 
# Example -------
# 
# .. bokeh-plot::
# ../sphinx/source/docs/user_guide/examples/interaction_pretext.py
# :source-position: below
PreText <- R6::R6Class("PreText",
  inherit = Paragraph,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      text = "", width = NULL, height = NULL, disabled = FALSE,
      sizing_mode = "fixed", tags = list(), subscribed_events = list(),
      css_classes = NULL, style = structure(list(), .Names = character(0)),
      name = NULL, js_property_callbacks = structure(list(), .Names =
      character(0)), id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        text = text, width = width, height = height, disabled = disabled,
        sizing_mode = sizing_mode, tags = tags,
        subscribed_events = subscribed_events, css_classes = css_classes,
        style = style, name = name,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["PreText"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# A group of check boxes.
CheckboxGroup <- R6::R6Class("CheckboxGroup",
  inherit = Group,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      width = NULL, height = NULL, disabled = FALSE, inline = FALSE,
      callback = NULL, sizing_mode = "fixed", tags = list(),
      subscribed_events = list(), active = list(), labels = list(),
      css_classes = NULL, name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        width = width, height = height, disabled = disabled, inline = inline,
        callback = callback, sizing_mode = sizing_mode,
        subscribed_events = subscribed_events, tags = tags, labels = labels,
        css_classes = css_classes, name = name,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["CheckboxGroup"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The list of indices of selected check boxes.
    # > List(Int)
    active = NULL
  )
)

# Abstract base class for groups with items rendered as check/radio
# boxes.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
Group <- R6::R6Class("Group",
  inherit = AbstractGroup,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      width = NULL, height = NULL, disabled = FALSE, inline = FALSE,
      callback = NULL, sizing_mode = "fixed", subscribed_events = list(),
      tags = list(), labels = list(), css_classes = NULL, name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        width = width, height = height, disabled = disabled,
        callback = callback, sizing_mode = sizing_mode,
        subscribed_events = subscribed_events, tags = tags, labels = labels,
        css_classes = css_classes, name = name,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["Group"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Should items be arrange vertically (``False``) or horizontally in-line
    # (``True``).
    # > Bool
    inline = NULL
  )
)

# Slider-based date selection widget.
DateSlider <- R6::R6Class("DateSlider",
  inherit = AbstractSlider,
  public = list(
    specified_args = NULL,
    initialize = function(
      end = NULL, sizing_mode = "fixed", title = "",
      subscribed_events = list(), callback_policy = "throttle",
      css_classes = NULL, start = NULL, show_value = TRUE,
      bar_color = "#e6e6e6", value = NULL,
      js_event_callbacks = structure(list(), .Names = character(0)),
      orientation = "horizontal", js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, width = NULL,
      callback_throttle = 200L, height = NULL, disabled = FALSE,
      callback = NULL, tags = list(), step = 1L, format = "%d %b %G",
      tooltips = TRUE, direction = "ltr", id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        orientation = orientation, width = width, tooltips = tooltips,
        callback_throttle = callback_throttle, height = height,
        disabled = disabled, callback = callback, css_classes = css_classes,
        sizing_mode = sizing_mode, name = name, tags = tags, title = title,
        format = format, subscribed_events = subscribed_events,
        callback_policy = callback_policy, show_value = show_value,
        bar_color = bar_color, direction = direction,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["DateSlider"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The maximum allowable value.
    # > Date
    end = NULL,
    # The minimum allowable value.
    # > Date
    start = NULL,
    # Initial or selected value.
    # > Date
    value = NULL,
    # The step between consecutive values.
    # > Int
    step = NULL
  )
)

# Calendar-based date cell editor.
DateEditor <- R6::R6Class("DateEditor",
  inherit = CellEditor,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["DateEditor"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# A container for space used to fill an empty spot in a row or column.
Spacer <- R6::R6Class("Spacer",
  inherit = LayoutDOM,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      width = NULL, height = NULL, disabled = FALSE, sizing_mode = "fixed",
      tags = list(), subscribed_events = list(), css_classes = NULL,
      name = NULL, js_property_callbacks = structure(list(), .Names =
      character(0)), id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        width = width, height = height, disabled = disabled,
        sizing_mode = sizing_mode, tags = tags,
        subscribed_events = subscribed_events, css_classes = css_classes,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["Spacer"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# 
LinearScale <- R6::R6Class("LinearScale",
  inherit = Scale,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["LinearScale"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# *toolbar icon*: |crosshair_icon|
# 
# The crosshair tool is a passive inspector tool. It is generally on at
# all times, but can be configured in the inspector's menu associated
# with the *toolbar icon* shown above.
# 
# The crosshair tool draws a crosshair annotation over the plot, centered
# on the current mouse position. The crosshair tool may be configured to
# draw across only one dimension by setting the ``dimension`` property to
# only ``width`` or ``height``.
# 
# .. |crosshair_icon| image:: /_images/icons/Crosshair.png :height: 18pt
CrosshairTool <- R6::R6Class("CrosshairTool",
  inherit = Inspection,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      dimensions = "both", line_width = 1L, toggleable = TRUE,
      line_color = "black", tags = list(), subscribed_events = list(),
      line_alpha = 1, name = NULL, js_property_callbacks = structure(list(),
      .Names = character(0)), id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        toggleable = toggleable, tags = tags,
        subscribed_events = subscribed_events, name = name,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["CrosshairTool"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Which dimensions the crosshair tool is to track. By default, both a
    # vertical and horizontal line will be drawn. If only "width" is
    # supplied, only a horizontal line will be drawn. If only "height" is
    # supplied, only a vertical line will be drawn.
    # > Enum('width', 'height', 'both')
    dimensions = NULL,
    # Stroke width in units of pixels.
    # > Float
    line_width = NULL,
    # A color to use to stroke paths with.
    # 
    # Acceptable values are:
    # 
    # - any of the 147 named `CSS colors`_, e.g ``'green'``, ``'indigo'`` -
    # an RGB(A) hex value, e.g., ``'#FF0000'``, ``'#44444444'`` - a 3-tuple
    # of integers (r,g,b) between 0 and 255 - a 4-tuple of (r,g,b,a) where
    # r,g,b are integers between 0..255 and a is between 0..1
    # 
    # .. _CSS colors: http://www.w3schools.com/cssref/css_colornames.asp
    # > Color
    line_color = NULL,
    # An alpha value to use to stroke paths with.
    # 
    # Acceptable values are floating point numbers between 0 (transparent)
    # and 1 (opaque).
    # > Float
    line_alpha = NULL
  )
)

# Render an open-body arrow head.
OpenHead <- R6::R6Class("OpenHead",
  inherit = ArrowHead,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      visible = TRUE, line_join = "miter", size = 25L, line_width = 1L,
      line_cap = "butt", name = NULL, line_dash = list(), plot = NULL,
      line_color = "black", tags = list(), subscribed_events = list(),
      line_alpha = 1, level = "annotation", line_dash_offset = 0L, id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        visible = visible, name = name, plot = plot, tags = tags,
        subscribed_events = subscribed_events, level = level,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["OpenHead"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The line join values for the arrow head outline.
    # > Enum('miter', 'round', 'bevel')
    line_join = NULL,
    # The size, in pixels, of the arrow head.
    # > Float
    size = NULL,
    # The line width values for the arrow head outline.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_width = NULL,
    # The line cap values for the arrow head outline.
    # > Enum('butt', 'round', 'square')
    line_cap = NULL,
    # The line dash values for the arrow head outline.
    # > DashPattern
    line_dash = NULL,
    # The line color values for the arrow head outline.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    line_color = NULL,
    # The line alpha values for the arrow head outline.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_alpha = NULL,
    # The line dash offset values for the arrow head outline.
    # > Int
    line_dash_offset = NULL
  )
)

# Tick formatter based on a human-readable format string.
NumeralTickFormatter <- R6::R6Class("NumeralTickFormatter",
  inherit = TickFormatter,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      language = "en", rounding = "round", tags = list(), format = "0,0",
      subscribed_events = list(), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["NumeralTickFormatter"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The language to use for formatting language-specific features (e.g.
    # thousands separator).
    # > Enum('be-nl', 'chs', 'cs', 'da-dk', 'de-ch', 'de', 'en', 'en-gb', 'es-ES', 'es', 'et', 'fi', 'fr-CA', 'fr-ch', 'fr', 'hu', 'it', 'ja', 'nl-nl', 'pl', 'pt-br', 'pt-pt', 'ru', 'ru-UA', 'sk', 'th', 'tr', 'uk-UA')
    language = NULL,
    # Rounding functions (round, floor, ceil) and their synonyms (nearest,
    # rounddown, roundup).
    # > Enum('round', 'nearest', 'floor', 'rounddown', 'ceil', 'roundup')
    rounding = NULL,
    # The number format, as defined in the following tables:
    # 
    # **NUMBERS**:
    # 
    # ============ ============== =============== Number Format String
    # ============ ============== =============== 10000 '0,0.0000'
    # 10,000.0000 10000.23 '0,0' 10,000 10000.23 '+0,0' +10,000 -10000
    # '0,0.0' -10,000.0 10000.1234 '0.000' 10000.123 10000.1234 '0[.]00000'
    # 10000.12340 -10000 '(0,0.0000)' (10,000.0000) -0.23 '.00' -.23 -0.23
    # '(.00)' (.23) 0.23 '0.00000' 0.23000 0.23 '0.0[0000]' 0.23 1230974
    # '0.0a' 1.2m 1460 '0 a' 1 k -104000 '0a' -104k 1 '0o' 1st 52 '0o' 52nd
    # 23 '0o' 23rd 100 '0o' 100th ============ ============== ===============
    # 
    # **CURRENCY**:
    # 
    # =========== =============== ============= Number Format String
    # =========== =============== ============= 1000.234 '$0,0.00' $1,000.23
    # 1000.2 '0,0[.]00 $' 1,000.20 $ 1001 '$ 0,0[.]00' $ 1,001 -1000.234
    # '($0,0)' ($1,000) -1000.234 '$0.00' -$1000.23 1230974 '($ 0.00 a)' $
    # 1.23 m =========== =============== =============
    # 
    # **BYTES**:
    # 
    # =============== =========== ============ Number Format String
    # =============== =========== ============ 100 '0b' 100B 2048 '0 b' 2 KB
    # 7884486213 '0.0b' 7.3GB 3467479682787 '0.000 b' 3.154 TB
    # =============== =========== ============
    # 
    # **PERCENTAGES**:
    # 
    # ============= ============= =========== Number Format String
    # ============= ============= =========== 1 '0%' 100% 0.974878234
    # '0.000%' 97.488% -0.43 '0 %' -43 % 0.43 '(0.000 %)' 43.000 %
    # ============= ============= ===========
    # 
    # **TIME**:
    # 
    # ============ ============== ============ Number Format String
    # ============ ============== ============ 25 '00:00:00' 0:00:25 238
    # '00:00:00' 0:03:58 63846 '00:00:00' 17:44:06 ============
    # ============== ============
    # 
    # For the complete specification, see http://numbrojs.com/format.html
    # > String
    format = NULL
  )
)

# 
GlyphRenderer <- R6::R6Class("GlyphRenderer",
  inherit = DataRenderer,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      view = NULL, hover_glyph = NULL, visible = TRUE,
      nonselection_glyph = "auto", selection_glyph = "auto",
      data_source = NULL, x_range_name = "default", glyph = NULL, name = NULL,
      y_range_name = "default", muted = FALSE, tags = list(),
      subscribed_events = list(), muted_glyph = NULL, level = "glyph",
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        name = name, visible = visible, tags = tags,
        subscribed_events = subscribed_events, level = level,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["GlyphRenderer"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # A view into the data source to use when rendering glyphs. A default
    # view of the entire data source is created when a view is not passed in
    # during initialization.
    # > Instance(CDSView)
    view = NULL,
    # An optional glyph used for inspected points, e.g., those that are being
    # hovered over by a HoverTool.
    # > Instance(Glyph)
    hover_glyph = NULL,
    # An optional glyph used for explicitly non-selected points (i.e.,
    # non-selected when there are other points that are selected, but not
    # when no points at all are selected.)
    # 
    # If set to "auto" then a glyph with a low alpha value (0.1) will be used
    # for non-selected points.
    # > Either(Auto, Instance(Glyph))
    nonselection_glyph = NULL,
    # An optional glyph used for selected points.
    # 
    # If set to "auto" then the standard glyph will be used for selected
    # points.
    # > Either(Auto, Instance(Glyph))
    selection_glyph = NULL,
    # Local data source to use when rendering glyphs on the plot.
    # > Instance(DataSource)
    data_source = NULL,
    # A particular (named) x-range to use for computing screen locations when
    # rendering glyphs on the plot. If unset, use the default x-range.
    # > String
    x_range_name = NULL,
    # The glyph to render, in conjunction with the supplied data source and
    # ranges.
    # > Instance(Glyph)
    glyph = NULL,
    # A particular (named) y-range to use for computing screen locations when
    # rendering glyphs on the plot. If unset, use the default -range.
    # > String
    y_range_name = NULL,
    # 
    # > Bool
    muted = NULL,
    # 
    # > Instance(Glyph)
    muted_glyph = NULL
  )
)

# Maps names of columns to sequences or arrays.
# 
# The ``ColumnDataSource`` is a fundamental data structure of Bokeh. Most
# plots, data tables, etc. will be driven by a ``ColumnDataSource``.
# 
# If the ColumnDataSource initializer is called with a single argument
# that can be any of the following:
# 
# * A Python ``dict`` that maps string names to sequences of values, e.g.
# lists, arrays, etc.
# 
# .. code-block:: python
# 
# data = {'x': [1,2,3,4], 'y': np.ndarray([10.0, 20.0, 30.0, 40.0])}
# 
# source = ColumnDataSource(data)
# 
# * A Pandas ``DataFrame`` object
# 
# .. code-block:: python
# 
# source = ColumnDataSource(df)
# 
# In this case the CDS will have columns corresponding to the columns of
# the ``DataFrame``. If the ``DataFrame`` has a named index column, then
# CDS will also have a column with this name. However, if the index name
# (or any subname of a ``MultiIndex``) is ``None``, then the CDS will
# have a column generically named ``index`` for the index.
# 
# * A Pandas ``GroupBy`` object
# 
# .. code-block:: python
# 
# group = df.groupby(('colA', 'ColB'))
# 
# In this case the CDS will have columns corresponding to the result of
# calling ``group.describe()``. The ``describe`` method generates columns
# for statistical measures such as ``mean`` and ``count`` for all the
# non-grouped orginal columns. The CDS columns are formed by joining
# original column names with the computed measure. For example, if a
# ``DataFrame`` has columns ``'year'`` and ``'mpg'``. Then passing
# ``df.groupby('year')`` to a CDS will result in columns such as
# ``'mpg_mean'``
# 
# If the ``GroupBy.describe`` result has a named index column, then CDS
# will also have a column with this name. However, if the index name (or
# any subname of a ``MultiIndex``) is ``None``, then the CDS will have a
# column generically named ``index`` for the index.
# 
# Note this capability to adapt ``GroupBy`` objects may only work with
# Pandas ``>=0.20.0``.
# 
# .. note:: There is an implicit assumption that all the columns in a
# given ``ColumnDataSource`` all have the same length at all times. For
# this reason, it is usually preferable to update the ``.data`` property
# of a data source "all at once".
ColumnDataSource <- R6::R6Class("ColumnDataSource",
  inherit = ColumnarDataSource,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      column_names = list(), callback = NULL, data = structure(list(), .Names
      = character(0)), tags = list(), subscribed_events = list(),
      selected = structure(list(`0d` = structure(list(indices = list(),
      glyph = NULL), .Names = c("indices", "glyph")), `2d` =
      structure(list(indices = structure(list(), .Names =
      character(0))), .Names = "indices"), `1d` =
      structure(list(indices = list()), .Names = "indices")), .Names =
      c("0d", "2d", "1d")), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        column_names = column_names, callback = callback, tags = tags,
        subscribed_events = subscribed_events, selected = selected,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["ColumnDataSource"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Mapping of column names to sequences of data. The data can be, e.g,
    # Python lists or tuples, NumPy arrays, etc.
    # > ColumnData(String, Seq(Any))
    data = NULL
  )
)

# 
DynamicImageRenderer <- R6::R6Class("DynamicImageRenderer",
  inherit = DataRenderer,
  public = list(
    specified_args = NULL,
    initialize = function(
      render_parents = TRUE, js_event_callbacks = structure(list(), .Names
      = character(0)), alpha = 1, visible = TRUE, image_source = NULL,
      name = NULL, tags = list(), subscribed_events = list(),
      level = "underlay", js_property_callbacks = structure(list(), .Names
      = character(0)), id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        name = name, visible = visible, tags = tags,
        subscribed_events = subscribed_events, level = level,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["DynamicImageRenderer"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Flag enable/disable drawing of parent tiles while waiting for new tiles
    # to arrive. Default value is True.
    # > Bool
    render_parents = NULL,
    # tile opacity 0.0 - 1.0
    # > Float
    alpha = NULL,
    # Image source to use when rendering on the plot.
    # > Instance(ImageSource)
    image_source = NULL
  )
)

# Abstract base class for data table's cell formatters.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
CellFormatter <- R6::R6Class("CellFormatter",
  inherit = Model,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["CellFormatter"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# A base class for all interactive tool types.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
Tool <- R6::R6Class("Tool",
  inherit = Model,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Tool"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# *toolbar icon*: |redo_icon|
# 
# Redo tool reverses the last action performed by undo tool.
# 
# .. |redo_icon| image:: /_images/icons/Redo.png :height: 18pt
RedoTool <- R6::R6Class("RedoTool",
  inherit = Action,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["RedoTool"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# A group of check boxes rendered as toggle buttons.
CheckboxButtonGroup <- R6::R6Class("CheckboxButtonGroup",
  inherit = ButtonGroup,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      width = NULL, height = NULL, disabled = FALSE, callback = NULL,
      tags = list(), sizing_mode = "fixed", subscribed_events = list(),
      active = list(), labels = list(), css_classes = NULL,
      button_type = "default", name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        width = width, height = height, disabled = disabled,
        callback = callback, sizing_mode = sizing_mode,
        subscribed_events = subscribed_events, tags = tags, labels = labels,
        css_classes = css_classes, button_type = button_type, name = name,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["CheckboxButtonGroup"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The list of indices of selected check boxes.
    # > List(Int)
    active = NULL
  )
)

# Render a vee-style arrow head.
VeeHead <- R6::R6Class("VeeHead",
  inherit = ArrowHead,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      visible = TRUE, line_join = "miter", size = 25L, line_width = 1L,
      line_cap = "butt", fill_alpha = 1, name = NULL, line_dash = list(),
      plot = NULL, fill_color = "black", line_color = "black", tags = list(),
      subscribed_events = list(), line_alpha = 1, level = "annotation",
      line_dash_offset = 0L, id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        visible = visible, name = name, plot = plot, tags = tags,
        subscribed_events = subscribed_events, level = level,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["VeeHead"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The line join values for the arrow head outline.
    # > Enum('miter', 'round', 'bevel')
    line_join = NULL,
    # The size, in pixels, of the arrow head.
    # > Float
    size = NULL,
    # The line width values for the arrow head outline.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_width = NULL,
    # The line cap values for the arrow head outline.
    # > Enum('butt', 'round', 'square')
    line_cap = NULL,
    # The fill alpha values for the arrow head interior.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    fill_alpha = NULL,
    # The line dash values for the arrow head outline.
    # > DashPattern
    line_dash = NULL,
    # The fill color values for the arrow head interior.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    fill_color = NULL,
    # The line color values for the arrow head outline.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    line_color = NULL,
    # The line alpha values for the arrow head outline.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_alpha = NULL,
    # The line dash offset values for the arrow head outline.
    # > Int
    line_dash_offset = NULL
  )
)

# Slider-based date range selection widget.
DateRangeSlider <- R6::R6Class("DateRangeSlider",
  inherit = AbstractSlider,
  public = list(
    specified_args = NULL,
    initialize = function(
      end = NULL, sizing_mode = "fixed", title = "",
      subscribed_events = list(), callback_policy = "throttle",
      css_classes = NULL, start = NULL, show_value = TRUE,
      bar_color = "#e6e6e6", value = NULL,
      js_event_callbacks = structure(list(), .Names = character(0)),
      orientation = "horizontal", js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, width = NULL,
      callback_throttle = 200L, height = NULL, disabled = FALSE,
      callback = NULL, tags = list(), step = 1L, format = "%d %b %G",
      tooltips = TRUE, direction = "ltr", id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        orientation = orientation, width = width, tooltips = tooltips,
        callback_throttle = callback_throttle, height = height,
        disabled = disabled, callback = callback, css_classes = css_classes,
        sizing_mode = sizing_mode, name = name, tags = tags, title = title,
        format = format, subscribed_events = subscribed_events,
        callback_policy = callback_policy, show_value = show_value,
        bar_color = bar_color, direction = direction,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["DateRangeSlider"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The maximum allowable value.
    # > Date
    end = NULL,
    # The minimum allowable value.
    # > Date
    start = NULL,
    # Initial or selected range.
    # > Tuple(Date, Date)
    value = NULL,
    # The step between consecutive values.
    # > Int
    step = NULL
  )
)

# Render a single text label as an annotation.
# 
# ``Label`` will render a single text label at given ``x`` and ``y``
# coordinates, which can be in either screen (pixel) space, or data (axis
# range) space.
# 
# The label can also be configured with a screen space offset from ``x``
# and ``y``, by using the ``x_offset`` and ``y_offset`` properties.
# 
# Additionally, the label can be rotated with the ``angle`` property.
# 
# There are also standard text, fill, and line properties to control the
# appearance of the text, its background, as well as the rectangular
# bounding box border.
Label <- R6::R6Class("Label",
  inherit = TextAnnotation,
  public = list(
    specified_args = NULL,
    initialize = function(
      text = NULL, text_font_style = "normal", border_line_dash = list(),
      border_line_width = 1L, text_font = "helvetica", y_units = "data",
      level = "annotation", x_range_name = "default", angle = 0L,
      background_fill_alpha = 1, background_fill_color = NULL,
      y_range_name = "default", text_line_height = 1.2, y_offset = 0L,
      subscribed_events = list(), text_font_size = list(value = "12pt"),
      text_align = "left", angle_units = "rad",
      js_property_callbacks = structure(list(), .Names = character(0)),
      js_event_callbacks = structure(list(), .Names = character(0)),
      name = NULL, visible = TRUE, border_line_join = "miter", x_offset = 0L,
      x = NULL, border_line_color = NULL, text_color = "#444444", y = NULL,
      border_line_alpha = 1, tags = list(), x_units = "data",
      border_line_cap = "butt", plot = NULL, text_baseline = "bottom",
      render_mode = "canvas", text_alpha = 1, border_line_dash_offset = 0L,
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        visible = visible, name = name, plot = plot, tags = tags,
        subscribed_events = subscribed_events, level = level,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["Label"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The text value to render.
    # > String
    text = NULL,
    # The text font style values for the text.
    # > Enum('normal', 'italic', 'bold')
    text_font_style = NULL,
    # The line dash values for the text bounding box.
    # > DashPattern
    border_line_dash = NULL,
    # The line width values for the text bounding box.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    border_line_width = NULL,
    # The text font values for the text.
    # > String
    text_font = NULL,
    # The unit type for the y attribute. Interpreted as "data space" units by
    # default.
    # > Enum('screen', 'data')
    y_units = NULL,
    # A particular (named) x-range to use for computing screen location when
    # rendering an annotation on the plot. If unset, use the default x-range.
    # > String
    x_range_name = NULL,
    # The angle to rotate the text, as measured from the horizontal.
    # 
    # .. warning:: The center of rotation for canvas and css render_modes is
    # different.  For `render_mode="canvas"` the label is rotated from the
    # top-left corner of the annotation, while for `render_mode="css"` the
    # annotation is rotated around it's center.
    # > Angle
    angle = NULL,
    # The fill alpha values for the text bounding box.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    background_fill_alpha = NULL,
    # The fill color values for the text bounding box.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    background_fill_color = NULL,
    # A particular (named) y-range to use for computing screen location when
    # rendering an annotation on the plot. If unset, use the default y-range.
    # > String
    y_range_name = NULL,
    # The text line height values for the text.
    # > Float
    text_line_height = NULL,
    # Offset value to apply to the y-coordinate.
    # 
    # This is useful, for instance, if it is desired to "float" text a fixed
    # distance in screen units from a given data position.
    # > Float
    y_offset = NULL,
    # The text font size values for the text.
    # > FontSizeSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), List(String))), List(String))
    text_font_size = NULL,
    # The text align values for the text.
    # > Enum('left', 'right', 'center')
    text_align = NULL,
    # Acceptable values for units are ``"rad"`` and ``"deg"``
    # > Enum('deg', 'rad')
    angle_units = NULL,
    # The line join values for the text bounding box.
    # > Enum('miter', 'round', 'bevel')
    border_line_join = NULL,
    # Offset value to apply to the x-coordinate.
    # 
    # This is useful, for instance, if it is desired to "float" text a fixed
    # distance in screen units from a given data position.
    # > Float
    x_offset = NULL,
    # The x-coordinate in screen coordinates to locate the text anchors.
    # 
    # Datetime values are also accepted, but note that they are immediately
    # converted to milliseconds-since-epoch.
    # > Float
    x = NULL,
    # The line color values for the text bounding box.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    border_line_color = NULL,
    # The text color values for the text.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    text_color = NULL,
    # The y-coordinate in screen coordinates to locate the text anchors.
    # 
    # Datetime values are also accepted, but note that they are immediately
    # converted to milliseconds-since-epoch.
    # > Float
    y = NULL,
    # The line alpha values for the text bounding box.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    border_line_alpha = NULL,
    # The unit type for the x attribute. Interpreted as "data space" units by
    # default.
    # > Enum('screen', 'data')
    x_units = NULL,
    # The line cap values for the text bounding box.
    # > Enum('butt', 'round', 'square')
    border_line_cap = NULL,
    # The text baseline values for the text.
    # > Enum('top', 'middle', 'bottom', 'alphabetic', 'hanging', 'ideographic')
    text_baseline = NULL,
    # Specifies whether the text is rendered as a canvas element or as an css
    # element overlaid on the canvas. The default mode is "canvas".
    # 
    # .. note:: The CSS labels won't be present in the output using the
    # "save" tool.
    # 
    # .. warning:: Not all visual styling properties are supported if the
    # render_mode is set to "css". The border_line_dash property isn't fully
    # supported and border_line_dash_offset isn't supported at all. Setting
    # text_alpha will modify the opacity of the entire background box and
    # border in addition to the text. Finally, clipping Label annotations
    # inside of the plot area isn't supported in "css" mode.
    # > Enum('canvas', 'css')
    render_mode = NULL,
    # The text alpha values for the text.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    text_alpha = NULL,
    # The line dash offset values for the text bounding box.
    # > Int
    border_line_dash_offset = NULL
  )
)

# Abstract base class for map options' models.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
MapOptions <- R6::R6Class("MapOptions",
  inherit = Model,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      lat = NULL, zoom = 12L, tags = list(), subscribed_events = list(),
      lng = NULL, name = NULL, js_property_callbacks = structure(list(),
      .Names = character(0)), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["MapOptions"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The latitude where the map should be centered.
    # > Float
    lat = NULL,
    # The initial zoom level to use when displaying the map.
    # > Int
    zoom = NULL,
    # The longitude where the map should be centered.
    # > Float
    lng = NULL
  )
)

# *toolbar icon*: |zoom_out_icon|
# 
# The zoom-out tool allows users to click a button to zoom out by a fixed
# amount.
# 
# .. |zoom_out_icon| image:: /_images/icons/ZoomOut.png :height: 18pt
ZoomOutTool <- R6::R6Class("ZoomOutTool",
  inherit = Action,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      tags = list(), dimensions = "both", subscribed_events = list(),
      factor = 0.1, name = NULL, js_property_callbacks = structure(list(),
      .Names = character(0)), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["ZoomOutTool"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Which dimensions the zoom-out tool is constrained to act in. By default
    # the zoom-out tool will zoom in any dimension, but can be configured to
    # only zoom horizontally across the width of the plot, or vertically
    # across the height of the plot.
    # > Enum('width', 'height', 'both')
    dimensions = NULL,
    # Percentage to zoom for each click of the zoom-in tool.
    # > Percent
    factor = NULL
  )
)

# A block (paragraph) of text.
# 
# This Bokeh model corresponds to an HTML ``<p>`` element.
# 
# Example -------
# 
# .. bokeh-plot::
# ../sphinx/source/docs/user_guide/examples/interaction_paragraph.py
# :source-position: below
Paragraph <- R6::R6Class("Paragraph",
  inherit = Markup,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      text = "", width = NULL, height = NULL, disabled = FALSE,
      sizing_mode = "fixed", tags = list(), subscribed_events = list(),
      css_classes = NULL, style = structure(list(), .Names = character(0)),
      name = NULL, js_property_callbacks = structure(list(), .Names =
      character(0)), id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        text = text, width = width, height = height, disabled = disabled,
        sizing_mode = sizing_mode, tags = tags,
        subscribed_events = subscribed_events, css_classes = css_classes,
        style = style, name = name,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["Paragraph"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# 
GeoJSONDataSource <- R6::R6Class("GeoJSONDataSource",
  inherit = ColumnarDataSource,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      column_names = list(), geojson = NULL, callback = NULL, tags = list(),
      subscribed_events = list(), selected = structure(list(`0d` =
      structure(list(indices = list(), glyph = NULL), .Names =
      c("indices", "glyph")), `2d` = structure(list(indices =
      structure(list(), .Names = character(0))), .Names = "indices"),
      `1d` = structure(list(indices = list()), .Names = "indices")),
      .Names = c("0d", "2d", "1d")), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        column_names = column_names, callback = callback, tags = tags,
        subscribed_events = subscribed_events, selected = selected,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["GeoJSONDataSource"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # GeoJSON that contains features for plotting. Currently
    # GeoJSONDataSource can only process a FeatureCollection or
    # GeometryCollection.
    # > JSON
    geojson = NULL
  )
)

# Spinner-based integer cell editor.
IntEditor <- R6::R6Class("IntEditor",
  inherit = CellEditor,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      step = 1L, tags = list(), subscribed_events = list(), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["IntEditor"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The major step value.
    # > Int
    step = NULL
  )
)

# Render a shaded polygonal region as an annotation.
PolyAnnotation <- R6::R6Class("PolyAnnotation",
  inherit = Annotation,
  public = list(
    specified_args = NULL,
    initialize = function(
      xs = list(), line_cap = "butt", x_range_name = "default",
      fill_alpha = 0.4, y_range_name = "default", ys = list(),
      subscribed_events = list(), line_alpha = 0.3, ys_units = "data",
      line_dash_offset = 0L, js_event_callbacks = structure(list(), .Names
      = character(0)), js_property_callbacks = structure(list(), .Names =
      character(0)), name = NULL, visible = TRUE, line_join = "miter",
      line_width = 1L, xs_units = "data", line_dash = list(), plot = NULL,
      fill_color = "#fff9ba", line_color = "#cccccc", tags = list(),
      level = "annotation", id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        visible = visible, name = name, plot = plot, tags = tags,
        subscribed_events = subscribed_events, level = level,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["PolyAnnotation"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The x-coordinates of the region to draw.
    # > Seq(Float)
    xs = NULL,
    # The line cap values for the polygon.
    # > Enum('butt', 'round', 'square')
    line_cap = NULL,
    # A particular (named) x-range to use for computing screen locations when
    # rendering box annotations on the plot. If unset, use the default
    # x-range.
    # > String
    x_range_name = NULL,
    # The fill alpha values for the polygon.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    fill_alpha = NULL,
    # A particular (named) y-range to use for computing screen locations when
    # rendering box annotations on the plot. If unset, use the default
    # y-range.
    # > String
    y_range_name = NULL,
    # The y-coordinates of the region to draw.
    # > Seq(Float)
    ys = NULL,
    # The line alpha values for the polygon.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_alpha = NULL,
    # The unit type for the ys attribute. Interpreted as "data space" units
    # by default.
    # > Enum('screen', 'data')
    ys_units = NULL,
    # The line dash offset values for the polygon.
    # > Int
    line_dash_offset = NULL,
    # The line join values for the polygon.
    # > Enum('miter', 'round', 'bevel')
    line_join = NULL,
    # The line width values for the polygon.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_width = NULL,
    # The unit type for the xs attribute. Interpreted as "data space" units
    # by default.
    # > Enum('screen', 'data')
    xs_units = NULL,
    # The line dash values for the polygon.
    # > DashPattern
    line_dash = NULL,
    # The fill color values for the polygon.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    fill_color = NULL,
    # The line color values for the polygon.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    line_color = NULL
  )
)

# Map numbers in a range [*low*, *high*] into a sequence of colors (a
# palette) on a natural logarithm scale.
# 
# For example, if the range is [0, 25] and the palette is ``['red',
# 'green', 'blue']``, the values would be mapped as follows::
# 
# x < 0 : 'red' # values < low are clamped 0 >= x < 2.72 : 'red' # math.e
# ** 1 2.72 >= x < 7.39 : 'green' # math.e ** 2 7.39 >= x < 20.09 :
# 'blue' # math.e ** 3 20.09 >= x : 'blue' # values > high are clamped
# 
# .. warning:: The LogColorMapper only works for images with scalar
# values that are non-negative.
LogColorMapper <- R6::R6Class("LogColorMapper",
  inherit = ContinuousColorMapper,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      high = NULL, low_color = NULL, high_color = NULL, nan_color = "gray",
      tags = list(), subscribed_events = list(), palette = NULL, low = NULL,
      name = NULL, js_property_callbacks = structure(list(), .Names =
      character(0)), id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        high = high, low_color = low_color, high_color = high_color,
        nan_color = nan_color, tags = tags,
        subscribed_events = subscribed_events, palette = palette, low = low,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["LogColorMapper"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Spinner-based time cell editor.
TimeEditor <- R6::R6Class("TimeEditor",
  inherit = CellEditor,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["TimeEditor"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Render circle markers with a '+' cross through the center.
# 
# Example -------
# 
# .. bokeh-plot:: ../examples/reference/models/CircleCross.py
# :source-position: below
CircleCross <- R6::R6Class("CircleCross",
  inherit = Marker,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      line_join = "miter", x = NULL, size = 4L, line_width = 1L,
      line_cap = "butt", fill_alpha = 1, y = NULL, angle = 0, line_dash = list(),
      fill_color = "gray", line_color = "black", tags = list(),
      subscribed_events = list(), line_alpha = 1, angle_units = "rad",
      name = NULL, line_dash_offset = 0L, id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks,
        line_join = line_join, x = x, size = size, line_width = line_width,
        line_cap = line_cap, fill_alpha = fill_alpha, y = y, angle = angle,
        line_dash = line_dash, fill_color = fill_color,
        line_color = line_color, tags = tags,
        subscribed_events = subscribed_events, line_alpha = line_alpha,
        angle_units = angle_units, name = name,
        line_dash_offset = line_dash_offset, id = id)
      types <- bk_prop_types[["CircleCross"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Date cell formatter.
DateFormatter <- R6::R6Class("DateFormatter",
  inherit = CellFormatter,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      tags = list(), format = "ISO-8601", subscribed_events = list(),
      name = NULL, js_property_callbacks = structure(list(), .Names =
      character(0)), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["DateFormatter"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The date format can be any standard `strftime`_ format string, as well
    # as any of the following predefined format names:
    # 
    # ================================================ ==================
    # =================== Format name(s) Format string Example Output
    # ================================================ ==================
    # =================== ``ATOM`` / ``W3C`` / ``RFC-3339`` / ``ISO-8601``
    # ``"%Y-%m-%d"`` 2014-03-01 ``COOKIE`` ``"%a, %d %b %Y"`` Sat, 01 Mar
    # 2014 ``RFC-850`` ``"%A, %d-%b-%y"`` Saturday, 01-Mar-14 ``RFC-1123`` /
    # ``RFC-2822`` ``"%a, %e %b %Y"`` Sat, 1 Mar 2014 ``RSS`` / ``RFC-822`` /
    # ``RFC-1036`` ``"%a, %e %b %y"`` Sat, 1 Mar 14 ``TIMESTAMP`` (ms since
    # epoch) 1393632000000 ================================================
    # ================== ===================
    # 
    # Note that in the table some of the format names are synonymous, with
    # identical format names separated by slashes.
    # 
    # This list of supported `strftime`_ format codes is reproduced below.
    # 
    # %a The abbreviated name of the day of the week according to the current
    # locale.
    # 
    # %A The full name of the day of the week according to the current
    # locale.
    # 
    # %b The abbreviated month name according to the current locale.
    # 
    # %B The full month name according to the current locale.
    # 
    # %c The preferred date and time representation for the current locale.
    # 
    # %C The century number (year/100) as a 2-digit integer.
    # 
    # %d The day of the month as a decimal number (range 01 to 31).
    # 
    # %D Equivalent to %m/%d/%y.  (Americans should note that in many other
    # countries %d/%m/%y is rather common. This means that in international
    # context this format is ambiguous and should not be used.)
    # 
    # %e Like %d, the day of the month as a decimal number, but a leading
    # zero is replaced by a space.
    # 
    # %f Microsecond as a decimal number, zero-padded on the left (range
    # 000000-999999). This is an extension to the set of directives available
    # to `timezone`_.
    # 
    # %F Equivalent to %Y-%m-%d (the ISO 8601 date format).
    # 
    # %G The ISO 8601 week-based year with century as a decimal number.  The
    # 4-digit year corresponding to the ISO week number (see %V).  This has
    # the same format and value as %Y, except that if the ISO week number
    # belongs to the previous or next year, that year is used instead.
    # 
    # %g Like %G, but without century, that is, with a 2-digit year (00-99).
    # 
    # %h Equivalent to %b.
    # 
    # %H The hour as a decimal number using a 24-hour clock (range 00 to 23).
    # 
    # %I The hour as a decimal number using a 12-hour clock (range 01 to 12).
    # 
    # %j The day of the year as a decimal number (range 001 to 366).
    # 
    # %k The hour (24-hour clock) as a decimal number (range 0 to 23).
    # Single digits are preceded by a blank.  (See also %H.)
    # 
    # %l The hour (12-hour clock) as a decimal number (range 1 to 12).
    # Single digits are preceded by a blank.  (See also %I.)  (TZ)
    # 
    # %m The month as a decimal number (range 01 to 12).
    # 
    # %M The minute as a decimal number (range 00 to 59).
    # 
    # %n A newline character. Bokeh text does not currently support newline
    # characters.
    # 
    # %N Nanosecond as a decimal number, zero-padded on the left (range
    # 000000000-999999999). Supports a padding width specifier, i.e.  %3N
    # displays 3 leftmost digits. However, this is only accurate to the
    # millisecond level of precision due to limitations of `timezone`_.
    # 
    # %p Either "AM" or "PM" according to the given time value, or the
    # corresponding strings for the current locale.  Noon is treated as "PM"
    # and midnight as "AM".
    # 
    # %P Like %p but in lowercase: "am" or "pm" or a corresponding string for
    # the current locale.
    # 
    # %r The time in a.m. or p.m. notation.  In the POSIX locale this is
    # equivalent to %I:%M:%S %p.
    # 
    # %R The time in 24-hour notation (%H:%M). For a version including the
    # seconds, see %T below.
    # 
    # %s The number of seconds since the Epoch, 1970-01-01 00:00:00 +0000
    # (UTC).
    # 
    # %S The second as a decimal number (range 00 to 60).  (The range is up
    # to 60 to allow for occasional leap seconds.)
    # 
    # %t A tab character. Bokeh text does not currently support tab
    # characters.
    # 
    # %T The time in 24-hour notation (%H:%M:%S).
    # 
    # %u The day of the week as a decimal, range 1 to 7, Monday being 1.  See
    # also %w.
    # 
    # %U The week number of the current year as a decimal number, range 00 to
    # 53, starting with the first Sunday as the first day of week 01.  See
    # also %V and %W.
    # 
    # %V The ISO 8601 week number (see NOTES) of the current year as a
    # decimal number, range 01 to 53, where week 1 is the first week that has
    # at least 4 days in the new year.  See also %U and %W.
    # 
    # %w The day of the week as a decimal, range 0 to 6, Sunday being 0.  See
    # also %u.
    # 
    # %W The week number of the current year as a decimal number, range 00 to
    # 53, starting with the first Monday as the first day of week 01.
    # 
    # %x The preferred date representation for the current locale without the
    # time.
    # 
    # %X The preferred time representation for the current locale without the
    # date.
    # 
    # %y The year as a decimal number without a century (range 00 to 99).
    # 
    # %Y The year as a decimal number including the century.
    # 
    # %z The +hhmm or -hhmm numeric timezone (that is, the hour and minute
    # offset from UTC).
    # 
    # %Z The timezone name or abbreviation.
    # 
    # %% A literal '%' character.
    # 
    # .. warning:: The client library BokehJS uses the `timezone`_ library to
    # format datetimes. The inclusion of the list below is based on the claim
    # that `timezone`_ makes to support "the full compliment of GNU date
    # format specifiers." However, this claim has not been tested
    # exhaustively against this list. If you find formats that do not
    # function as expected, please submit a `github issue`_, so that the
    # documentation can be updated appropriately.
    # 
    # .. _strftime: http://man7.org/linux/man-pages/man3/strftime.3.html ..
    # _timezone: http://bigeasy.github.io/timezone/ .. _github issue:
    # https://github.com/bokeh/bokeh/issues
    # > Either(Enum('ATOM', 'W3C', 'RFC-3339', 'ISO-8601', 'COOKIE', 'RFC-822', 'RFC-850', 'RFC-1036', 'RFC-1123', 'RFC-2822', 'RSS', 'TIMESTAMP'), String)
    format = NULL
  )
)

# Basic string cell formatter.
StringFormatter <- R6::R6Class("StringFormatter",
  inherit = CellFormatter,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      text_color = NULL, font_style = "normal", tags = list(),
      subscribed_events = list(), text_align = "left", name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["StringFormatter"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # An optional text color. See :class:`bokeh.core.properties.Color` for
    # details.
    # > Color
    text_color = NULL,
    # An optional text font style, e.g. bold, italic.
    # > Enum('normal', 'italic', 'bold')
    font_style = NULL,
    # An optional text align, i.e. left, center or right.
    # > Enum('left', 'right', 'center')
    text_align = NULL
  )
)

# Render a filled area band along a dimension.
Band <- R6::R6Class("Band",
  inherit = Annotation,
  public = list(
    specified_args = NULL,
    initialize = function(
      lower_units = "data", base = NULL, line_cap = "butt",
      x_range_name = "default", fill_alpha = 0.4, y_range_name = "default",
      subscribed_events = list(), line_alpha = 0.3, dimension = "height",
      line_dash_offset = 0L, js_event_callbacks = structure(list(), .Names
      = character(0)), js_property_callbacks = structure(list(), .Names =
      character(0)), name = NULL, visible = TRUE, line_join = "miter",
      lower = NULL, upper = NULL, line_width = 1L, source = NULL, tags = list(),
      line_dash = list(), upper_units = "data", base_units = "data",
      fill_color = "#fff9ba", line_color = "#cccccc", plot = NULL,
      level = "annotation", id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        visible = visible, name = name, plot = plot, tags = tags,
        subscribed_events = subscribed_events, level = level,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["Band"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # 
    # > Enum('screen', 'data')
    lower_units = NULL,
    # The orthogonal coordinates of the upper and lower values.
    # > DistanceSpec(units_default='data')
    base = NULL,
    # The line cap values for the band.
    # > Enum('butt', 'round', 'square')
    line_cap = NULL,
    # A particular (named) x-range to use for computing screen locations when
    # rendering annotations on the plot. If unset, use the default x-range.
    # > String
    x_range_name = NULL,
    # The fill alpha values for the band.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    fill_alpha = NULL,
    # A particular (named) y-range to use for computing screen locations when
    # rendering annotations on the plot. If unset, use the default y-range.
    # > String
    y_range_name = NULL,
    # The line alpha values for the band.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_alpha = NULL,
    # The direction of the band.
    # > Enum('width', 'height')
    dimension = NULL,
    # The line dash offset values for the band.
    # > Int
    line_dash_offset = NULL,
    # The line join values for the band.
    # > Enum('miter', 'round', 'bevel')
    line_join = NULL,
    # The coordinates of the lower portion of the filled area band.
    # > DistanceSpec(units_default='data')
    lower = NULL,
    # The coordinations of the upper portion of the filled area band.
    # > DistanceSpec(units_default='data')
    upper = NULL,
    # The line width values for the band.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_width = NULL,
    # Local data source to use when rendering annotations on the plot.
    # > Instance(DataSource)
    source = NULL,
    # The line dash values for the band.
    # > DashPattern
    line_dash = NULL,
    # 
    # > Enum('screen', 'data')
    upper_units = NULL,
    # 
    # > Enum('screen', 'data')
    base_units = NULL,
    # The fill color values for the band.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    fill_color = NULL,
    # The line color values for the band.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    line_color = NULL
  )
)

# Apply either a uniform or normally sampled random jitter to data.
Jitter <- R6::R6Class("Jitter",
  inherit = Transform,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      width = 1L, mean = 0L, subscribed_events = list(), tags = list(),
      range = NULL, distribution = "uniform", name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Jitter"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The width (absolute for uniform distribution and sigma for the normal
    # distribution) of the random sample.
    # > Float
    width = NULL,
    # The central value for the random sample
    # > Float
    mean = NULL,
    # When applying Jitter to Categorical data values, the corresponding
    # ``FactorRange`` must be supplied as the ``range`` property.
    # > Instance(Range)
    range = NULL,
    # The random distribution upon which to pull the random scatter
    # > Enum('uniform', 'normal')
    distribution = NULL
  )
)

# A layoutable toolbar that can accept the tools of multiple plots, and
# can merge the tools into a single button for convenience.
ToolbarBox <- R6::R6Class("ToolbarBox",
  inherit = Box,
  public = list(
    specified_args = NULL,
    initialize = function(
      children = list(), js_event_callbacks = structure(list(), .Names =
      character(0)), toolbar_location = "right", merge_tools = TRUE,
      width = NULL, height = NULL, disabled = FALSE, sizing_mode = "fixed",
      tools = list(), subscribed_events = list(), tags = list(),
      logo = "normal", css_classes = NULL, name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(children = children,
        js_event_callbacks = js_event_callbacks, width = width,
        height = height, disabled = disabled, sizing_mode = sizing_mode,
        tags = tags, subscribed_events = subscribed_events,
        css_classes = css_classes, name = name,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["ToolbarBox"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Should the toolbar be presented as if it was stuck to the `above`,
    # `right`, `left`, `below` edge of a plot. Default is `right`.
    # > Enum('above', 'below', 'left', 'right')
    toolbar_location = NULL,
    # Merge all the tools together so there is one tool to control all the
    # plots.
    # > Bool
    merge_tools = NULL,
    # A list of tools to add to the plot.
    # > List(Instance(Tool))
    tools = NULL,
    # What version of the Bokeh logo to display on the toolbar. If set to
    # None, no logo will be displayed.
    # > Enum('normal', 'grey')
    logo = NULL
  )
)

# Display tick values from continuous ranges as powers of some base.
# 
# Most often useful in conjunction with a ``LogTicker``.
LogTickFormatter <- R6::R6Class("LogTickFormatter",
  inherit = TickFormatter,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      ticker = NULL, tags = list(), subscribed_events = list(), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["LogTickFormatter"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The corresponding ``LogTicker``, used to determine the correct base to
    # use. If unset, the formatter will use base 10 as a default.
    # > Instance(Ticker)
    ticker = NULL
  )
)

# 
CategoricalScale <- R6::R6Class("CategoricalScale",
  inherit = LinearScale,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["CategoricalScale"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Single-line input widget.
TextInput <- R6::R6Class("TextInput",
  inherit = InputWidget,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      width = NULL, height = NULL, disabled = FALSE, callback = NULL,
      tags = list(), sizing_mode = "fixed", title = "", placeholder = "",
      subscribed_events = list(), css_classes = NULL, name = NULL, value = "",
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        width = width, height = height, disabled = disabled,
        sizing_mode = sizing_mode, title = title, tags = tags,
        subscribed_events = subscribed_events, css_classes = css_classes,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["TextInput"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # A callback to run in the browser whenever the user unfocuses the
    # TextInput widget by hitting Enter or clicking outside of the text box
    # area.
    # > Instance(Callback)
    callback = NULL,
    # Placeholder for empty input field
    # > String
    placeholder = NULL,
    # Initial or entered text value.
    # > String
    value = NULL
  )
)

# A base class that defines common properties for all axis types.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
Axis <- R6::R6Class("Axis",
  inherit = GuideRenderer,
  public = list(
    specified_args = NULL,
    initialize = function(
      major_tick_line_color = "black", major_tick_line_dash_offset = 0L,
      major_tick_line_dash = list(), major_tick_line_cap = "butt",
      axis_line_join = "miter", axis_label_text_font_style = "italic",
      minor_tick_line_dash = list(), formatter = NULL,
      major_tick_line_width = 1L, axis_line_dash = list(), axis_label = "",
      minor_tick_line_color = "black", axis_label_text_font = "helvetica",
      y_range_name = "default", ticker = NULL, subscribed_events = list(),
      axis_line_alpha = 1, js_event_callbacks = structure(list(), .Names =
      character(0)), major_label_text_font_style = "normal",
      minor_tick_line_join = "miter", major_label_text_font = "helvetica",
      minor_tick_out = 4L, major_label_text_line_height = 1.2,
      major_tick_in = 2L, major_label_text_font_size = list(value = "8pt"),
      axis_label_text_alpha = 1, minor_tick_line_dash_offset = 0L,
      plot = NULL, tags = list(), axis_label_text_font_size = list(value =
      "10pt"), major_tick_out = 6L, axis_label_text_color = "#444444",
      minor_tick_line_alpha = 1, major_label_text_baseline = "alphabetic",
      major_label_text_alpha = 1, major_label_overrides = structure(list(),
      .Names = character(0)), axis_line_dash_offset = 0L, bounds = "auto",
      x_range_name = "default", major_tick_line_alpha = 1,
      major_label_text_color = "#444444", axis_line_color = "black",
      axis_line_cap = "butt", major_label_text_align = "center",
      major_label_orientation = "horizontal", axis_label_standoff = 5L,
      js_property_callbacks = structure(list(), .Names = character(0)),
      name = NULL, axis_label_text_align = "left", visible = TRUE,
      minor_tick_line_width = 1L, major_tick_line_join = "miter",
      minor_tick_line_cap = "butt", minor_tick_in = 0L,
      axis_label_text_line_height = 1.2,
      axis_label_text_baseline = "bottom", major_label_standoff = 5L,
      axis_line_width = 1L, level = "overlay", id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        visible = visible, name = name, plot = plot, tags = tags,
        subscribed_events = subscribed_events, level = level,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["Axis"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The line color of the major ticks.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    major_tick_line_color = NULL,
    # The line dash offset of the major ticks.
    # > Int
    major_tick_line_dash_offset = NULL,
    # The line dash of the major ticks.
    # > DashPattern
    major_tick_line_dash = NULL,
    # The line cap of the major ticks.
    # > Enum('butt', 'round', 'square')
    major_tick_line_cap = NULL,
    # The line join of the axis line.
    # > Enum('miter', 'round', 'bevel')
    axis_line_join = NULL,
    # The text font style of the axis label.
    # > Enum('normal', 'italic', 'bold')
    axis_label_text_font_style = NULL,
    # The line dash of the minor ticks.
    # > DashPattern
    minor_tick_line_dash = NULL,
    # A TickFormatter to use for formatting the visual appearance of ticks.
    # > Instance(TickFormatter)
    formatter = NULL,
    # The line width of the major ticks.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    major_tick_line_width = NULL,
    # The line dash of the axis line.
    # > DashPattern
    axis_line_dash = NULL,
    # A text label for the axis, displayed parallel to the axis rule.
    # 
    # .. note:: LaTeX notation is not currently supported; please see
    # :bokeh-issue:`647` to track progress or contribute.
    # > String
    axis_label = NULL,
    # The line color of the minor ticks.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    minor_tick_line_color = NULL,
    # The text font of the axis label.
    # > String
    axis_label_text_font = NULL,
    # A particular (named) y-range to use for computing screen locations when
    # rendering an axis on the plot. If unset, use the default y-range.
    # > String
    y_range_name = NULL,
    # A Ticker to use for computing locations of axis components.
    # 
    # The property may also be passed a sequence of floating point numbers as
    # a shorthand for creating and configuring a ``FixedTicker``, e.g. the
    # following code
    # 
    # .. code-block:: python
    # 
    # from bokeh.plotting import figure
    # 
    # p = figure() p.xaxis.ticker = [10, 20, 37.4]
    # 
    # is equivalent to:
    # 
    # .. code-block:: python
    # 
    # from bokeh.plotting import figure from bokeh.models.tickers import
    # FixedTicker
    # 
    # p = figure() p.xaxis.ticker = FixedTicker(ticks=[10, 20, 37.4])
    # > Instance(Ticker)
    ticker = NULL,
    # The line alpha of the axis line.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    axis_line_alpha = NULL,
    # The text font style of the major tick labels.
    # > Enum('normal', 'italic', 'bold')
    major_label_text_font_style = NULL,
    # The line join of the minor ticks.
    # > Enum('miter', 'round', 'bevel')
    minor_tick_line_join = NULL,
    # The text font of the major tick labels.
    # > String
    major_label_text_font = NULL,
    # The distance in pixels that major ticks should extend out of the main
    # plot area.
    # > Int
    minor_tick_out = NULL,
    # The text line height of the major tick labels.
    # > Float
    major_label_text_line_height = NULL,
    # The distance in pixels that major ticks should extend into the main
    # plot area.
    # > Int
    major_tick_in = NULL,
    # The text font size of the major tick labels.
    # > FontSizeSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), List(String))), List(String))
    major_label_text_font_size = NULL,
    # The text alpha of the axis label.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    axis_label_text_alpha = NULL,
    # The line dash offset of the minor ticks.
    # > Int
    minor_tick_line_dash_offset = NULL,
    # The text font size of the axis label.
    # > FontSizeSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), List(String))), List(String))
    axis_label_text_font_size = NULL,
    # The distance in pixels that major ticks should extend out of the main
    # plot area.
    # > Int
    major_tick_out = NULL,
    # The text color of the axis label.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    axis_label_text_color = NULL,
    # The line alpha of the minor ticks.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    minor_tick_line_alpha = NULL,
    # The text baseline of the major tick labels.
    # > Enum('top', 'middle', 'bottom', 'alphabetic', 'hanging', 'ideographic')
    major_label_text_baseline = NULL,
    # The text alpha of the major tick labels.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    major_label_text_alpha = NULL,
    # Provide explicit tick label values for specific tick locations that
    # override normal formatting.
    # > Dict(Either(Float, String), String)
    major_label_overrides = NULL,
    # The line dash offset of the axis line.
    # > Int
    axis_line_dash_offset = NULL,
    # Bounds for the rendered axis. If unset, the axis will span the entire
    # plot in the given dimension.
    # > Either(Auto, Tuple(Float, Float), Tuple(Datetime, Datetime))
    bounds = NULL,
    # A particular (named) x-range to use for computing screen locations when
    # rendering an axis on the plot. If unset, use the default x-range.
    # > String
    x_range_name = NULL,
    # The line alpha of the major ticks.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    major_tick_line_alpha = NULL,
    # The text color of the major tick labels.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    major_label_text_color = NULL,
    # The line color of the axis line.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    axis_line_color = NULL,
    # The line cap of the axis line.
    # > Enum('butt', 'round', 'square')
    axis_line_cap = NULL,
    # The text align of the major tick labels.
    # > Enum('left', 'right', 'center')
    major_label_text_align = NULL,
    # What direction the major label text should be oriented. If a number is
    # supplied, the angle of the text is measured from horizontal.
    # > Either(Enum('horizontal', 'vertical'), Float)
    major_label_orientation = NULL,
    # The distance in pixels that the axis labels should be offset from the
    # tick labels.
    # > Int
    axis_label_standoff = NULL,
    # The text align of the axis label.
    # > Enum('left', 'right', 'center')
    axis_label_text_align = NULL,
    # The line width of the minor ticks.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    minor_tick_line_width = NULL,
    # The line join of the major ticks.
    # > Enum('miter', 'round', 'bevel')
    major_tick_line_join = NULL,
    # The line cap of the minor ticks.
    # > Enum('butt', 'round', 'square')
    minor_tick_line_cap = NULL,
    # The distance in pixels that minor ticks should extend into the main
    # plot area.
    # > Int
    minor_tick_in = NULL,
    # The text line height of the axis label.
    # > Float
    axis_label_text_line_height = NULL,
    # The text baseline of the axis label.
    # > Enum('top', 'middle', 'bottom', 'alphabetic', 'hanging', 'ideographic')
    axis_label_text_baseline = NULL,
    # The distance in pixels that the major tick labels should be offset from
    # the associated ticks.
    # > Int
    major_label_standoff = NULL,
    # The line width of the axis line.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    axis_line_width = NULL
  )
)

# 
LogScale <- R6::R6Class("LogScale",
  inherit = Scale,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["LogScale"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Base class for interpolator transforms.
# 
# Interpolators return the value of a function which has been evaluated
# between specified (x, y) pairs of data.  As an example, if two control
# point pairs were provided to the interpolator, a linear interpolaction
# at a specific value of 'x' would result in the value of 'y' which
# existed on the line conneting the two control points.
# 
# The control point pairs for the interpolators can be specified through
# either
# 
# * A literal sequence of values:
# 
# .. code-block: python
# 
# interp = Interpolator(x=[1, 2, 3, 4, 5], y=[2, 5, 10, 12, 16])
# 
# * or a pair of columns defined in a `ColumnDataSource` object:
# 
# .. code-block: python
# 
# interp = Interpolator(x="year", y="earnings", data=jewlery_prices))
# 
# This is the base class and is not intended to end use.  Please see the
# documentation for the final derived classes (Jitter,
# LineraInterpolator, StepInterpolator) for mor information on their
# specific methods of interpolation.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
Interpolator <- R6::R6Class("Interpolator",
  inherit = Transform,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      x = NULL, y = NULL, data = NULL, tags = list(), subscribed_events = list(),
      clip = TRUE, name = NULL, js_property_callbacks = structure(list(),
      .Names = character(0)), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Interpolator"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Independant coordiante denoting the location of a point.
    # > Either(String, Seq(Float))
    x = NULL,
    # Dependant coordinate denoting the value of a point at a location.
    # > Either(String, Seq(Float))
    y = NULL,
    # Data which defines the source for the named columns if a string is
    # passed to either the ``x`` or ``y`` parameters.
    # > Instance(ColumnarDataSource)
    data = NULL,
    # Determine if the interpolation should clip the result to include only
    # values inside its predefined range.  If this is set to False, it will
    # return the most value of the closest point.
    # > Bool
    clip = NULL
  )
)

# Combine different tickers at different scales.
# 
# Uses the ``min_interval`` and ``max_interval`` interval attributes of
# the tickers to select the appropriate ticker at different scales.
CompositeTicker <- R6::R6Class("CompositeTicker",
  inherit = ContinuousTicker,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      num_minor_ticks = 5L, tickers = list(), desired_num_ticks = 6L,
      tags = list(), subscribed_events = list(), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        name = name, num_minor_ticks = num_minor_ticks, tags = tags,
        subscribed_events = subscribed_events,
        desired_num_ticks = desired_num_ticks,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["CompositeTicker"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # A list of Ticker objects to combine at different scales in order to
    # generate tick values. The supplied tickers should be in order.
    # Specifically, if S comes before T, then it should be the case that::
    # 
    # S.get_max_interval() < T.get_min_interval()
    # > Seq(Instance(Ticker))
    tickers = NULL
  )
)

# Calendar-based date picker widget.
DatePicker <- R6::R6Class("DatePicker",
  inherit = InputWidget,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      max_date = NULL, js_property_callbacks = structure(list(), .Names =
      character(0)), width = NULL, min_date = NULL, height = NULL,
      disabled = FALSE, callback = NULL, sizing_mode = "fixed", title = "",
      tags = list(), subscribed_events = list(), css_classes = NULL,
      name = NULL, value = NULL, id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        width = width, height = height, disabled = disabled,
        sizing_mode = sizing_mode, title = title, tags = tags,
        subscribed_events = subscribed_events, css_classes = css_classes,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["DatePicker"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Optional latest allowable date.
    # > Date
    max_date = NULL,
    # Optional earliest allowable date.
    # > Date
    min_date = NULL,
    # A callback to run in the browser whenever the current date value
    # changes.
    # > Instance(Callback)
    callback = NULL,
    # The initial or picked date.
    # > Date
    value = NULL
  )
)

# Render multiple text labels as annotations.
# 
# ``LabelSet`` will render multiple text labels at given ``x`` and ``y``
# coordinates, which can be in either screen (pixel) space, or data (axis
# range) space. In this case (as opposed to the single ``Label`` model),
# ``x`` and ``y`` can also be the name of a column from a
# :class:`~bokeh.models.sources.ColumnDataSource`, in which case the
# labels will be "vectorized" using coordinate values from the specified
# columns.
# 
# The label can also be configured with a screen space offset from ``x``
# and ``y``, by using the ``x_offset`` and ``y_offset`` properties. These
# offsets may be vectorized by giving the name of a data source column.
# 
# Additionally, the label can be rotated with the ``angle`` property
# (which may also be a column name.)
# 
# There are also standard text, fill, and line properties to control the
# appearance of the text, its background, as well as the rectangular
# bounding box border.
# 
# The data source is provided by setting the ``source`` property.
LabelSet <- R6::R6Class("LabelSet",
  inherit = TextAnnotation,
  public = list(
    specified_args = NULL,
    initialize = function(
      text = "text", text_font_style = "normal", border_line_dash = list(),
      border_line_width = 1L, text_font = "helvetica", y_units = "data",
      level = "annotation", x_range_name = "default", angle = 0L,
      background_fill_alpha = 1, background_fill_color = NULL,
      y_range_name = "default", text_line_height = 1.2, y_offset = 0L,
      subscribed_events = list(), text_font_size = list(value = "12pt"),
      text_align = "left", angle_units = "rad",
      js_property_callbacks = structure(list(), .Names = character(0)),
      js_event_callbacks = structure(list(), .Names = character(0)),
      name = NULL, visible = TRUE, border_line_join = "miter", x_offset = 0L,
      x = NULL, border_line_color = NULL, text_color = "#444444", y = NULL,
      source = NULL, border_line_alpha = 1, tags = list(), x_units = "data",
      border_line_cap = "butt", plot = NULL, text_baseline = "bottom",
      render_mode = "canvas", text_alpha = 1, border_line_dash_offset = 0L,
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        visible = visible, name = name, plot = plot, tags = tags,
        subscribed_events = subscribed_events, level = level,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["LabelSet"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The text values to render.
    # > StringSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), List(String))), List(String))
    text = NULL,
    # The text font style values for the text.
    # > Enum('normal', 'italic', 'bold')
    text_font_style = NULL,
    # The line dash values for the text bounding box.
    # > DashPattern
    border_line_dash = NULL,
    # The line width values for the text bounding box.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    border_line_width = NULL,
    # The text font values for the text.
    # > String
    text_font = NULL,
    # The unit type for the ys attribute. Interpreted as "data space" units
    # by default.
    # > Enum('screen', 'data')
    y_units = NULL,
    # A particular (named) x-range to use for computing screen locations when
    # rendering annotations on the plot. If unset, use the default x-range.
    # > String
    x_range_name = NULL,
    # The angles to rotate the text, as measured from the horizontal.
    # 
    # .. warning:: The center of rotation for canvas and css render_modes is
    # different.  For `render_mode="canvas"` the label is rotated from the
    # top-left corner of the annotation, while for `render_mode="css"` the
    # annotation is rotated around it's center.
    # > AngleSpec(units_default='rad')
    angle = NULL,
    # The fill alpha values for the text bounding box.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    background_fill_alpha = NULL,
    # The fill color values for the text bounding box.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    background_fill_color = NULL,
    # A particular (named) y-range to use for computing screen locations when
    # rendering annotations on the plot. If unset, use the default y-range.
    # > String
    y_range_name = NULL,
    # The text line height values for the text.
    # > Float
    text_line_height = NULL,
    # Offset values to apply to the y-coordinates.
    # 
    # This is useful, for instance, if it is desired to "float" text a fixed
    # distance in screen units from a given data position.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    y_offset = NULL,
    # The text font size values for the text.
    # > FontSizeSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), List(String))), List(String))
    text_font_size = NULL,
    # The text align values for the text.
    # > Enum('left', 'right', 'center')
    text_align = NULL,
    # 
    # > Enum('deg', 'rad')
    angle_units = NULL,
    # The line join values for the text bounding box.
    # > Enum('miter', 'round', 'bevel')
    border_line_join = NULL,
    # Offset values to apply to the x-coordinates.
    # 
    # This is useful, for instance, if it is desired to "float" text a fixed
    # distance in screen units from a given data position.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    x_offset = NULL,
    # The x-coordinates to locate the text anchors.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    x = NULL,
    # The line color values for the text bounding box.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    border_line_color = NULL,
    # The text color values for the text.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    text_color = NULL,
    # The y-coordinates to locate the text anchors.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    y = NULL,
    # Local data source to use when rendering annotations on the plot.
    # > Instance(DataSource)
    source = NULL,
    # The line alpha values for the text bounding box.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    border_line_alpha = NULL,
    # The unit type for the xs attribute. Interpreted as "data space" units
    # by default.
    # > Enum('screen', 'data')
    x_units = NULL,
    # The line cap values for the text bounding box.
    # > Enum('butt', 'round', 'square')
    border_line_cap = NULL,
    # The text baseline values for the text.
    # > Enum('top', 'middle', 'bottom', 'alphabetic', 'hanging', 'ideographic')
    text_baseline = NULL,
    # Specifies whether the text is rendered as a canvas element or as an css
    # element overlaid on the canvas. The default mode is "canvas".
    # 
    # .. note:: The CSS labels won't be present in the output using the
    # "save" tool.
    # 
    # .. warning:: Not all visual styling properties are supported if the
    # render_mode is set to "css". The border_line_dash property isn't fully
    # supported and border_line_dash_offset isn't supported at all. Setting
    # text_alpha will modify the opacity of the entire background box and
    # border in addition to the text. Finally, clipping Label annotations
    # inside of the plot area isn't supported in "css" mode.
    # > Enum('canvas', 'css')
    render_mode = NULL,
    # The text alpha values for the text.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    text_alpha = NULL,
    # The line dash offset values for the text bounding box.
    # > Int
    border_line_dash_offset = NULL
  )
)

# Display tick values from categorical ranges as string values.
CategoricalTickFormatter <- R6::R6Class("CategoricalTickFormatter",
  inherit = TickFormatter,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["CategoricalTickFormatter"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Base class for ``Expression`` models that represent a computation to be
# carried out on the client-side.
# 
# JavaScript implementations should implement the following methods:
# 
# .. code-block: coffeescript
# 
# v_compute: (source) -> # compute an array of values
# 
# Note that the result of this call will be automatically saved and
# re-used for each ``source`` that is passed in. If a ``source`` is
# changed, then the saved value for that source is discarded, and the
# next call will re-compute (and save) a new value. If you wish to
# prevent this caching, you may implement ``_v_compute: (source)``
# instead.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
Expression <- R6::R6Class("Expression",
  inherit = Model,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Expression"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Table column widget.
TableColumn <- R6::R6Class("TableColumn",
  inherit = Model,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      field = NULL, sortable = TRUE, width = 300L, formatter = NULL,
      subscribed_events = list(), title = NULL, tags = list(), editor = NULL,
      default_sort = "ascending", name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["TableColumn"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The name of the field mapping to a column in the data source.
    # > String
    field = NULL,
    # Whether this column is sortable or not. Note that data table has to
    # have sorting enabled to allow sorting in general.
    # > Bool
    sortable = NULL,
    # The width or maximum width (depending on data table's configuration) in
    # pixels of this column.
    # > Int
    width = NULL,
    # The cell formatter for this column. By default, a simple string
    # formatter is used.
    # > Instance(CellFormatter)
    formatter = NULL,
    # The title of this column. If not set, column's data field is used
    # instead.
    # > String
    title = NULL,
    # The cell editor for this column. By default, a simple string editor is
    # used.
    # > Instance(CellEditor)
    editor = NULL,
    # The default sorting order. By default ``ascending`` order is used.
    # > Enum('ascending', 'descending')
    default_sort = NULL
  )
)

# Generate nice lat/lon ticks form underlying WebMercator coordinates.
MercatorTicker <- R6::R6Class("MercatorTicker",
  inherit = BasicTicker,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      mantissas = list(1L, 2L, 5L), base = 10, num_minor_ticks = 5L,
      min_interval = 0, desired_num_ticks = 6L, max_interval = NULL,
      tags = list(), subscribed_events = list(), dimension = NULL, name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        mantissas = mantissas, base = base,
        num_minor_ticks = num_minor_ticks, min_interval = min_interval,
        desired_num_ticks = desired_num_ticks, max_interval = max_interval,
        tags = tags, subscribed_events = subscribed_events, name = name,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["MercatorTicker"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Specify whether to generate ticks for Latitude or Longitude.
    # 
    # Projected coordinates are not separable, computing Latitude and
    # Longitude tick locations from Web Mercator requires considering
    # coordinates from both dimensions together. Use this property to specify
    # which result should be returned.
    # 
    # Typically, if the ticker is for an x-axis, then dimension should be
    # ``"lon"`` and if the ticker is for a y-axis, then the dimension should
    # be `"lat"``.
    # 
    # In order to prevent hard to debug errors, there is no default value for
    # dimension. Using an un-configured MercatorTicker will result in a
    # validation error and a JavaScript console error.
    # > Enum('lat', 'lon')
    dimension = NULL
  )
)

# An abstract base class for renderer types.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
Renderer <- R6::R6Class("Renderer",
  inherit = Model,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      name = NULL, visible = TRUE, tags = list(), subscribed_events = list(),
      level = "image", js_property_callbacks = structure(list(), .Names =
      character(0)), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Renderer"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Is the renderer visible.
    # > Bool
    visible = NULL,
    # Specifies the level in which to paint this renderer.
    # > Enum('image', 'underlay', 'glyph', 'annotation', 'overlay')
    level = NULL
  )
)

# A base class for tools that respond to tap/click events.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
Tap <- R6::R6Class("Tap",
  inherit = Tool,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Tap"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# *toolbar icon*: |lasso_select_icon|
# 
# The lasso selection tool allows users to make selections on a Plot by
# indicating a free-drawn "lasso" region by dragging the mouse or a
# finger over the plot region. The end of the drag event indicates the
# selection region is ready.
# 
# See :ref:`userguide_styling_selected_unselected_glyphs` for information
# on styling selected and unselected glyphs.
# 
# .. note:: Selections can be comprised of multiple regions, even those
# made by different selection tools. Hold down the <<shift>> key while
# making a selection to append the new selection to any previous
# selection that might exist.
# 
# .. |lasso_select_icon| image:: /_images/icons/LassoSelect.png :height:
# 18pt
LassoSelectTool <- R6::R6Class("LassoSelectTool",
  inherit = Drag,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      renderers = list(), callback = NULL, select_every_mousemove = TRUE,
      subscribed_events = list(), tags = list(), overlay = NULL,
      names = list(), name = NULL, js_property_callbacks = structure(list(),
      .Names = character(0)), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["LassoSelectTool"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # An explicit list of renderers to hit test again. If unset, defaults to
    # all renderers on a plot.
    # > List(Instance(Renderer))
    renderers = NULL,
    # A callback to run in the browser on every selection of a lasso area.
    # The cb_data parameter that is available to the Callback code will
    # contain one LassoSelectTool-specific field:
    # 
    # :geometry: object containing the coordinates of the lasso area
    # > Instance(Callback)
    callback = NULL,
    # Whether a selection computation should happen on every mouse event, or
    # only once, when the selection region is completed. Default: True
    # > Bool
    select_every_mousemove = NULL,
    # A shaded annotation drawn to indicate the selection region.
    # > Instance(PolyAnnotation)
    overlay = NULL,
    # A list of names to query for. If set, only renderers that have a
    # matching value for their ``name`` attribute will be used.
    # > List(String)
    names = NULL
  )
)

# Render horizontal bars, given a center coordinate, ``height`` and
# (``left``, ``right``) coordinates.
# 
# Example -------
# 
# .. bokeh-plot:: ../examples/reference/models/HBar.py :source-position:
# below
HBar <- R6::R6Class("HBar",
  inherit = Glyph,
  public = list(
    specified_args = NULL,
    initialize = function(
      right = NULL, js_event_callbacks = structure(list(), .Names =
      character(0)), js_property_callbacks = structure(list(), .Names =
      character(0)), line_join = "miter", line_width = 1L, height = NULL,
      y = NULL, line_cap = "butt", fill_alpha = 1, tags = list(),
      line_dash = list(), left = 0L, line_color = "black", fill_color = "gray",
      subscribed_events = list(), line_alpha = 1, name = NULL,
      line_dash_offset = 0L, id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["HBar"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The x-coordinates of the right edges.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    right = NULL,
    # The line join values for the horizontal bars.
    # > Enum('miter', 'round', 'bevel')
    line_join = NULL,
    # The line width values for the horizontal bars.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_width = NULL,
    # The heights of the vertical bars.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    height = NULL,
    # The y-coordinates of the centers of the horizontal bars.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    y = NULL,
    # The line cap values for the horizontal bars.
    # > Enum('butt', 'round', 'square')
    line_cap = NULL,
    # The fill alpha values for the horizontal bars.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    fill_alpha = NULL,
    # The line dash values for the horizontal bars.
    # > DashPattern
    line_dash = NULL,
    # The x-coordinates of the left edges.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    left = NULL,
    # The line color values for the horizontal bars.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    line_color = NULL,
    # The fill color values for the horizontal bars.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    fill_color = NULL,
    # The line alpha values for the horizontal bars.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_alpha = NULL,
    # The line dash offset values for the horizontal bars.
    # > Int
    line_dash_offset = NULL
  )
)

# A view into a ColumnDataSource that represents a row-wise subset.
CDSView <- R6::R6Class("CDSView",
  inherit = Model,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      name = NULL, tags = list(), subscribed_events = list(), filters = list(),
      source = NULL, js_property_callbacks = structure(list(), .Names =
      character(0)), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["CDSView"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # List of filters that the view comprises.
    # > List(Instance(Filter))
    filters = NULL,
    # The ColumnDataSource associated with this view. Used to determine the
    # length of the columns.
    # > Instance(ColumnarDataSource)
    source = NULL
  )
)

# A base class for all image source types.
ImageSource <- R6::R6Class("ImageSource",
  inherit = Model,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      tags = list(), extra_url_vars = structure(list(), .Names =
      character(0)), url = "", subscribed_events = list(), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["ImageSource"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # A dictionary that maps url variable template keys to values.  These
    # variables are useful for parts of tile urls which do not change from
    # tile to tile (e.g. server host name, or layer name).
    # > Dict(String, Any)
    extra_url_vars = NULL,
    # tile service url (example:
    # http://c.tile.openstreetmap.org/{Z}/{X}/{Y}.png)
    # > String
    url = NULL
  )
)

# Render a whisker along a dimension.
Whisker <- R6::R6Class("Whisker",
  inherit = Annotation,
  public = list(
    specified_args = NULL,
    initialize = function(
      lower_units = "data", base = NULL, upper_head = NULL, line_cap = "butt",
      x_range_name = "default", y_range_name = "default",
      subscribed_events = list(), line_alpha = 1, dimension = "height",
      line_dash_offset = 0L, js_event_callbacks = structure(list(), .Names
      = character(0)), js_property_callbacks = structure(list(), .Names =
      character(0)), name = NULL, visible = TRUE, line_join = "miter",
      lower = NULL, upper = NULL, line_width = 1L, source = NULL, tags = list(),
      lower_head = NULL, line_dash = list(), base_units = "data",
      upper_units = "data", line_color = "black", plot = NULL,
      level = "underlay", id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        visible = visible, name = name, plot = plot, tags = tags,
        subscribed_events = subscribed_events, level = level,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["Whisker"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # 
    # > Enum('screen', 'data')
    lower_units = NULL,
    # The orthogonal coordinates of the upper and lower values.
    # > DistanceSpec(units_default='data')
    base = NULL,
    # Instance of ArrowHead.
    # > Instance(ArrowHead)
    upper_head = NULL,
    # The line cap values for the whisker body.
    # > Enum('butt', 'round', 'square')
    line_cap = NULL,
    # A particular (named) x-range to use for computing screen locations when
    # rendering annotations on the plot. If unset, use the default x-range.
    # > String
    x_range_name = NULL,
    # A particular (named) y-range to use for computing screen locations when
    # rendering annotations on the plot. If unset, use the default y-range.
    # > String
    y_range_name = NULL,
    # The line alpha values for the whisker body.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_alpha = NULL,
    # The direction of the band.
    # > Enum('width', 'height')
    dimension = NULL,
    # The line dash offset values for the whisker body.
    # > Int
    line_dash_offset = NULL,
    # The line join values for the whisker body.
    # > Enum('miter', 'round', 'bevel')
    line_join = NULL,
    # The coordinates of the lower end of the whiskers.
    # > DistanceSpec(units_default='data')
    lower = NULL,
    # The coordinations of the upper end of the whiskers.
    # > DistanceSpec(units_default='data')
    upper = NULL,
    # The line width values for the whisker body.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_width = NULL,
    # Local data source to use when rendering annotations on the plot.
    # > Instance(DataSource)
    source = NULL,
    # Instance of ArrowHead.
    # > Instance(ArrowHead)
    lower_head = NULL,
    # The line dash values for the whisker body.
    # > DashPattern
    line_dash = NULL,
    # 
    # > Enum('screen', 'data')
    base_units = NULL,
    # 
    # > Enum('screen', 'data')
    upper_units = NULL,
    # The line color values for the whisker body.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    line_color = NULL
  )
)

# A single-widget container with title bar and controls.
Panel <- R6::R6Class("Panel",
  inherit = Widget,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      width = NULL, height = NULL, disabled = FALSE, sizing_mode = "fixed",
      closable = FALSE, title = "", tags = list(), subscribed_events = list(),
      css_classes = NULL, name = NULL, child = NULL, id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        width = width, height = height, disabled = disabled,
        sizing_mode = sizing_mode, tags = tags,
        subscribed_events = subscribed_events, css_classes = css_classes,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["Panel"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Whether this panel is closeable or not. If True, an "x" button will
    # appear.
    # > Bool
    closable = NULL,
    # An optional text title of the panel.
    # > String
    title = NULL,
    # The child widget. If you need more children, use a layout widget, e.g.
    # ``Row`` or ``Column``.
    # > Instance(LayoutDOM)
    child = NULL
  )
)

# Base class for glyphs that are simple markers with line and fill
# properties, located at an (x, y) location with a specified size.
# 
# .. note:: For simplicity, all markers have both line and fill
# properties declared, however some markers (`Asterisk`, `Cross`, `X`)
# only draw lines. For these markers, the fill values are simply ignored.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
Marker <- R6::R6Class("Marker",
  inherit = XYGlyph,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      line_join = "miter", x = NULL, size = 4L, line_width = 1L,
      line_cap = "butt", fill_alpha = 1, y = NULL, angle = 0, line_dash = list(),
      fill_color = "gray", line_color = "black", tags = list(),
      subscribed_events = list(), line_alpha = 1, angle_units = "rad",
      name = NULL, line_dash_offset = 0L, id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Marker"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The line join values for the markers.
    # > Enum('miter', 'round', 'bevel')
    line_join = NULL,
    # The x-axis coordinates for the center of the markers.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    x = NULL,
    # The size (diameter) values for the markers in screen space units.
    # > ScreenDistanceSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    size = NULL,
    # The line width values for the markers.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_width = NULL,
    # The line cap values for the markers.
    # > Enum('butt', 'round', 'square')
    line_cap = NULL,
    # The fill alpha values for the markers.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    fill_alpha = NULL,
    # The y-axis coordinates for the center of the markers.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    y = NULL,
    # The angles to rotate the markers.
    # > AngleSpec(units_default='rad')
    angle = NULL,
    # The line dash values for the markers.
    # > DashPattern
    line_dash = NULL,
    # The fill color values for the markers.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    fill_color = NULL,
    # The line color values for the markers.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    line_color = NULL,
    # The line alpha values for the markers.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_alpha = NULL,
    # 
    # > Enum('deg', 'rad')
    angle_units = NULL,
    # The line dash offset values for the markers.
    # > Int
    line_dash_offset = NULL
  )
)

# A container for widgets that are part of a layout.
WidgetBox <- R6::R6Class("WidgetBox",
  inherit = LayoutDOM,
  public = list(
    specified_args = NULL,
    initialize = function(
      children = list(), js_event_callbacks = structure(list(), .Names =
      character(0)), width = NULL, height = NULL, disabled = FALSE,
      sizing_mode = "fixed", tags = list(), subscribed_events = list(),
      css_classes = NULL, name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        width = width, height = height, disabled = disabled,
        sizing_mode = sizing_mode, tags = tags,
        subscribed_events = subscribed_events, css_classes = css_classes,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["WidgetBox"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The list of widgets to put in the layout box.
    # > List(Instance(Widget))
    children = NULL
  )
)

# Execute a JavaScript function.
CustomJS <- R6::R6Class("CustomJS",
  inherit = Callback,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      tags = list(), code = "", args = structure(list(), .Names =
      character(0)), subscribed_events = list(), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["CustomJS"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # A snippet of JavaScript code to execute in the browser. The code is
    # made into the body of a function, and all of of the named objects in
    # ``args`` are available as parameters that the code can use.
    # Additionally, a ``cb_obj`` parameter contains the object that triggered
    # the callback and an optional ``cb_data`` parameter that contains any
    # tool-specific data (i.e. mouse coordinates and hovered glyph indices
    # for the HoverTool).
    # 
    # .. note:: Use ``CustomJS.from_coffeescript()`` for CoffeeScript source
    # code.
    # > String
    code = NULL,
    # A mapping of names to Bokeh plot objects. These objects are made
    # available to the callback code snippet as the values of named
    # parameters to the callback.
    # > Dict(String, Instance(Model))
    args = NULL
  )
)

# Lay out child components in a single vertical row.
# 
# Children can be specified as positional arguments, as a single argument
# that is a sequence, or using the ``children`` keyword argument.
Column <- R6::R6Class("Column",
  inherit = Box,
  public = list(
    specified_args = NULL,
    initialize = function(
      children = list(), js_event_callbacks = structure(list(), .Names =
      character(0)), width = NULL, height = NULL, disabled = FALSE,
      sizing_mode = "fixed", tags = list(), subscribed_events = list(),
      css_classes = NULL, name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(children = children,
        js_event_callbacks = js_event_callbacks, width = width,
        height = height, disabled = disabled, sizing_mode = sizing_mode,
        tags = tags, subscribed_events = subscribed_events,
        css_classes = css_classes, name = name,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["Column"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Render Bézier curves.
# 
# For more information consult the `Wikipedia article for Bézier curve`_.
# 
# .. _Wikipedia article for Bézier curve:
# http://en.wikipedia.org/wiki/Bézier_curve
# 
# Example -------
# 
# .. bokeh-plot:: ../examples/reference/models/Bezier.py
# :source-position: below
Bezier <- R6::R6Class("Bezier",
  inherit = Glyph,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      line_join = "miter", y0 = NULL, line_width = 1L, line_cap = "butt",
      cy0 = NULL, x1 = NULL, line_dash = list(), x0 = NULL, cx0 = NULL,
      subscribed_events = list(), line_color = "black", tags = list(),
      cx1 = NULL, line_alpha = 1, cy1 = NULL, y1 = NULL, name = NULL,
      line_dash_offset = 0L, id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Bezier"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The line join values for the Bézier curves.
    # > Enum('miter', 'round', 'bevel')
    line_join = NULL,
    # The y-coordinates of the starting points.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    y0 = NULL,
    # The line width values for the Bézier curves.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_width = NULL,
    # The line cap values for the Bézier curves.
    # > Enum('butt', 'round', 'square')
    line_cap = NULL,
    # The y-coordinates of first control points.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    cy0 = NULL,
    # The x-coordinates of the ending points.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    x1 = NULL,
    # The line dash values for the Bézier curves.
    # > DashPattern
    line_dash = NULL,
    # The x-coordinates of the starting points.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    x0 = NULL,
    # The x-coordinates of first control points.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    cx0 = NULL,
    # The line color values for the Bézier curves.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    line_color = NULL,
    # The x-coordinates of second control points.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    cx1 = NULL,
    # The line alpha values for the Bézier curves.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_alpha = NULL,
    # The y-coordinates of second control points.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    cy1 = NULL,
    # The y-coordinates of the ending points.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    y1 = NULL,
    # The line dash offset values for the Bézier curves.
    # > Int
    line_dash_offset = NULL
  )
)

# Render a single title box as an annotation.
Title <- R6::R6Class("Title",
  inherit = TextAnnotation,
  public = list(
    specified_args = NULL,
    initialize = function(
      text = NULL, align = "left", text_font_style = "bold",
      border_line_dash = list(), border_line_width = 1L,
      text_font = "helvetica", level = "annotation",
      background_fill_alpha = 1, background_fill_color = NULL,
      subscribed_events = list(), text_font_size = list(value = "10pt"),
      js_property_callbacks = structure(list(), .Names = character(0)),
      js_event_callbacks = structure(list(), .Names = character(0)),
      name = NULL, visible = TRUE, border_line_join = "miter",
      text_color = "#444444", border_line_color = NULL,
      border_line_alpha = 1, offset = 0L, plot = NULL,
      border_line_cap = "butt", tags = list(), render_mode = "canvas",
      text_alpha = 1, border_line_dash_offset = 0L, id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        visible = visible, name = name, plot = plot, tags = tags,
        subscribed_events = subscribed_events, level = level,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["Title"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The text value to render.
    # > String
    text = NULL,
    # Location to align the title text.
    # > Enum('left', 'right', 'center')
    align = NULL,
    # A style to use for rendering text.
    # 
    # Acceptable values are:
    # 
    # - ``'normal'`` normal text - ``'italic'`` *italic text* - ``'bold'``
    # **bold text**
    # > Enum('normal', 'italic', 'bold')
    text_font_style = NULL,
    # The line dash values for the text bounding box.
    # > DashPattern
    border_line_dash = NULL,
    # The line width values for the text bounding box.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    border_line_width = NULL,
    # Name of a font to use for rendering text, e.g., ``'times'``,
    # ``'helvetica'``.
    # > String
    text_font = NULL,
    # The fill alpha values for the text bounding box.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    background_fill_alpha = NULL,
    # The fill color values for the text bounding box.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    background_fill_color = NULL,
    # 
    # > FontSizeSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), List(String))), List(String))
    text_font_size = NULL,
    # The line join values for the text bounding box.
    # > Enum('miter', 'round', 'bevel')
    border_line_join = NULL,
    # A color to use to fill text with.
    # 
    # Acceptable values are:
    # 
    # - any of the 147 named `CSS colors`_, e.g ``'green'``, ``'indigo'`` -
    # an RGB(A) hex value, e.g., ``'#FF0000'``, ``'#44444444'`` - a 3-tuple
    # of integers (r,g,b) between 0 and 255 - a 4-tuple of (r,g,b,a) where
    # r,g,b are integers between 0..255 and a is between 0..1
    # 
    # .. _CSS colors: http://www.w3schools.com/cssref/css_colornames.asp
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    text_color = NULL,
    # The line color values for the text bounding box.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    border_line_color = NULL,
    # The line alpha values for the text bounding box.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    border_line_alpha = NULL,
    # Offset the text by a number of pixels (can be positive or negative).
    # Shifts the text in different directions based on the location of the
    # title:
    # 
    # * above: shifts title right * right: shifts title down * below: shifts
    # title right * left: shifts title up
    # > Float
    offset = NULL,
    # The line cap values for the text bounding box.
    # > Enum('butt', 'round', 'square')
    border_line_cap = NULL,
    # Specifies whether the text is rendered as a canvas element or as an css
    # element overlaid on the canvas. The default mode is "canvas".
    # 
    # .. note:: The CSS labels won't be present in the output using the
    # "save" tool.
    # 
    # .. warning:: Not all visual styling properties are supported if the
    # render_mode is set to "css". The border_line_dash property isn't fully
    # supported and border_line_dash_offset isn't supported at all. Setting
    # text_alpha will modify the opacity of the entire background box and
    # border in addition to the text. Finally, clipping Label annotations
    # inside of the plot area isn't supported in "css" mode.
    # > Enum('canvas', 'css')
    render_mode = NULL,
    # An alpha value to use to fill text with.
    # 
    # Acceptable values are floating point numbers between 0 (transparent)
    # and 1 (opaque).
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    text_alpha = NULL,
    # The line dash offset values for the text bounding box.
    # > Int
    border_line_dash_offset = NULL
  )
)

# Render upside-down triangle markers.
# 
# Example -------
# 
# .. bokeh-plot:: ../examples/reference/models/InvertedTriangle.py
# :source-position: below
InvertedTriangle <- R6::R6Class("InvertedTriangle",
  inherit = Marker,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      line_join = "miter", x = NULL, size = 4L, line_width = 1L,
      line_cap = "butt", fill_alpha = 1, y = NULL, angle = 0, line_dash = list(),
      fill_color = "gray", line_color = "black", tags = list(),
      subscribed_events = list(), line_alpha = 1, angle_units = "rad",
      name = NULL, line_dash_offset = 0L, id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks,
        line_join = line_join, x = x, size = size, line_width = line_width,
        line_cap = line_cap, fill_alpha = fill_alpha, y = y, angle = angle,
        line_dash = line_dash, fill_color = fill_color,
        line_color = line_color, tags = tags,
        subscribed_events = subscribed_events, line_alpha = line_alpha,
        angle_units = angle_units, name = name,
        line_dash_offset = line_dash_offset, id = id)
      types <- bk_prop_types[["InvertedTriangle"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Boolean value cell editor.
CheckboxEditor <- R6::R6Class("CheckboxEditor",
  inherit = CellEditor,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["CheckboxEditor"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Render ellipses.
# 
# Example -------
# 
# .. bokeh-plot:: ../examples/reference/models/Ellipse.py
# :source-position: below
Ellipse <- R6::R6Class("Ellipse",
  inherit = XYGlyph,
  public = list(
    specified_args = NULL,
    initialize = function(
      line_cap = "butt", fill_alpha = 1, angle = 0, width_units = "data",
      subscribed_events = list(), line_alpha = 1, angle_units = "rad",
      line_dash_offset = 0L, js_event_callbacks = structure(list(), .Names
      = character(0)), js_property_callbacks = structure(list(), .Names =
      character(0)), name = NULL, line_join = "miter", width = NULL, x = NULL,
      height_units = "data", line_width = 1L, height = NULL, y = NULL,
      line_dash = list(), fill_color = "gray", line_color = "black",
      tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Ellipse"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The line cap values for the ovals.
    # > Enum('butt', 'round', 'square')
    line_cap = NULL,
    # The fill alpha values for the ovals.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    fill_alpha = NULL,
    # The angle the ellipses are rotated from horizontal. [rad]
    # > AngleSpec(units_default='rad')
    angle = NULL,
    # 
    # > Enum('screen', 'data')
    width_units = NULL,
    # The line alpha values for the ovals.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_alpha = NULL,
    # 
    # > Enum('deg', 'rad')
    angle_units = NULL,
    # The line dash offset values for the ovals.
    # > Int
    line_dash_offset = NULL,
    # The line join values for the ovals.
    # > Enum('miter', 'round', 'bevel')
    line_join = NULL,
    # The widths of each ellipse.
    # > DistanceSpec(units_default='data')
    width = NULL,
    # The x-coordinates of the centers of the ellipses.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    x = NULL,
    # 
    # > Enum('screen', 'data')
    height_units = NULL,
    # The line width values for the ovals.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_width = NULL,
    # The heights of each ellipse.
    # > DistanceSpec(units_default='data')
    height = NULL,
    # The y-coordinates of the centers of the ellipses.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    y = NULL,
    # The line dash values for the ovals.
    # > DashPattern
    line_dash = NULL,
    # The fill color values for the ovals.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    fill_color = NULL,
    # The line color values for the ovals.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    line_color = NULL
  )
)

# Base class for text annotation models such as labels and titles.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
TextAnnotation <- R6::R6Class("TextAnnotation",
  inherit = Annotation,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      visible = TRUE, name = NULL, plot = NULL, tags = list(),
      subscribed_events = list(), level = "annotation",
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        visible = visible, name = name, plot = plot, tags = tags,
        subscribed_events = subscribed_events, level = level,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["TextAnnotation"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# A group of radio boxes rendered as toggle buttons.
RadioButtonGroup <- R6::R6Class("RadioButtonGroup",
  inherit = ButtonGroup,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      width = NULL, height = NULL, disabled = FALSE, callback = NULL,
      tags = list(), sizing_mode = "fixed", subscribed_events = list(),
      active = NULL, labels = list(), css_classes = NULL,
      button_type = "default", name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        width = width, height = height, disabled = disabled,
        callback = callback, sizing_mode = sizing_mode,
        subscribed_events = subscribed_events, tags = tags, labels = labels,
        css_classes = css_classes, button_type = button_type, name = name,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["RadioButtonGroup"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The index of the selected radio box, or ``None`` if nothing is
    # selected.
    # > Int
    active = NULL
  )
)

# The QUADKEYTileSource has the same tile origin as the WMTSTileSource
# but requests tiles using a `quadkey` argument instead of X, Y, Z e.g.
# ``http://your.quadkey.tile.host/{Q}.png``
QUADKEYTileSource <- R6::R6Class("QUADKEYTileSource",
  inherit = MercatorTileSource,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      extra_url_vars = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      max_zoom = 30L, min_zoom = 0L, tile_size = 256L,
      x_origin_offset = 20037508.34, y_origin_offset = 20037508.34,
      tags = list(), attribution = "", url = "", subscribed_events = list(),
      initial_resolution = 156543.033928041, name = NULL, wrap_around = TRUE,
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        extra_url_vars = extra_url_vars,
        js_property_callbacks = js_property_callbacks, max_zoom = max_zoom,
        min_zoom = min_zoom, tile_size = tile_size,
        x_origin_offset = x_origin_offset,
        y_origin_offset = y_origin_offset, tags = tags,
        attribution = attribution, url = url,
        subscribed_events = subscribed_events,
        initial_resolution = initial_resolution, name = name,
        wrap_around = wrap_around, id = id)
      types <- bk_prop_types[["QUADKEYTileSource"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# An axis that picks nice numbers for tick locations on a linear scale.
# Configured with a ``BasicTickFormatter`` by default.
LinearAxis <- R6::R6Class("LinearAxis",
  inherit = ContinuousAxis,
  public = list(
    specified_args = NULL,
    initialize = function(
      major_tick_line_color = "black", major_tick_line_dash_offset = 0L,
      major_tick_line_dash = list(), major_tick_line_cap = "butt",
      axis_line_join = "miter", axis_label_text_font_style = "italic",
      minor_tick_line_dash = list(), formatter = NULL,
      major_tick_line_width = 1L, axis_line_dash = list(), axis_label = "",
      minor_tick_line_color = "black", axis_label_text_font = "helvetica",
      y_range_name = "default", ticker = NULL, subscribed_events = list(),
      axis_line_alpha = 1, js_event_callbacks = structure(list(), .Names =
      character(0)), major_label_text_font_style = "normal",
      minor_tick_line_join = "miter", major_label_text_font = "helvetica",
      minor_tick_out = 4L, major_label_text_line_height = 1.2,
      major_tick_in = 2L, major_label_text_font_size = list(value = "8pt"),
      axis_label_text_alpha = 1, minor_tick_line_dash_offset = 0L,
      plot = NULL, tags = list(), axis_label_text_font_size = list(value =
      "10pt"), major_tick_out = 6L, axis_label_text_color = "#444444",
      minor_tick_line_alpha = 1, major_label_text_baseline = "alphabetic",
      major_label_text_alpha = 1, major_label_overrides = structure(list(),
      .Names = character(0)), axis_line_dash_offset = 0L, bounds = "auto",
      x_range_name = "default", major_tick_line_alpha = 1,
      major_label_text_color = "#444444", axis_line_color = "black",
      axis_line_cap = "butt", major_label_text_align = "center",
      major_label_orientation = "horizontal", axis_label_standoff = 5L,
      js_property_callbacks = structure(list(), .Names = character(0)),
      name = NULL, axis_label_text_align = "left", visible = TRUE,
      minor_tick_line_width = 1L, major_tick_line_join = "miter",
      minor_tick_line_cap = "butt", minor_tick_in = 0L,
      axis_label_text_line_height = 1.2,
      axis_label_text_baseline = "bottom", major_label_standoff = 5L,
      axis_line_width = 1L, level = "overlay", id = NULL
    ) {
      super$initialize(major_tick_line_color = major_tick_line_color,
        major_tick_line_dash_offset = major_tick_line_dash_offset,
        major_tick_line_dash = major_tick_line_dash,
        major_tick_line_cap = major_tick_line_cap,
        axis_line_join = axis_line_join,
        axis_label_text_font_style = axis_label_text_font_style,
        minor_tick_line_dash = minor_tick_line_dash, formatter = formatter,
        major_tick_line_width = major_tick_line_width,
        axis_line_dash = axis_line_dash, axis_label = axis_label,
        minor_tick_line_color = minor_tick_line_color,
        axis_label_text_font = axis_label_text_font,
        y_range_name = y_range_name, ticker = ticker,
        subscribed_events = subscribed_events,
        axis_line_alpha = axis_line_alpha,
        js_event_callbacks = js_event_callbacks,
        major_label_text_font_style = major_label_text_font_style,
        minor_tick_line_join = minor_tick_line_join,
        major_label_text_font = major_label_text_font,
        minor_tick_out = minor_tick_out,
        major_label_text_line_height = major_label_text_line_height,
        major_tick_in = major_tick_in,
        major_label_text_font_size = major_label_text_font_size,
        axis_label_text_alpha = axis_label_text_alpha,
        minor_tick_line_dash_offset = minor_tick_line_dash_offset,
        plot = plot, tags = tags,
        axis_label_text_font_size = axis_label_text_font_size,
        major_tick_out = major_tick_out,
        axis_label_text_color = axis_label_text_color,
        minor_tick_line_alpha = minor_tick_line_alpha,
        major_label_text_baseline = major_label_text_baseline,
        major_label_text_alpha = major_label_text_alpha,
        major_label_overrides = major_label_overrides,
        axis_line_dash_offset = axis_line_dash_offset, bounds = bounds,
        x_range_name = x_range_name,
        major_tick_line_alpha = major_tick_line_alpha,
        major_label_text_color = major_label_text_color,
        axis_line_color = axis_line_color, axis_line_cap = axis_line_cap,
        major_label_text_align = major_label_text_align,
        major_label_orientation = major_label_orientation,
        axis_label_standoff = axis_label_standoff,
        js_property_callbacks = js_property_callbacks, name = name,
        axis_label_text_align = axis_label_text_align, visible = visible,
        minor_tick_line_width = minor_tick_line_width,
        major_tick_line_join = major_tick_line_join,
        minor_tick_line_cap = minor_tick_line_cap,
        minor_tick_in = minor_tick_in,
        axis_label_text_line_height = axis_label_text_line_height,
        axis_label_text_baseline = axis_label_text_baseline,
        major_label_standoff = major_label_standoff,
        axis_line_width = axis_line_width, level = level, id = id)
      types <- bk_prop_types[["LinearAxis"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# An abstract base class for data renderer types (e.g. ``GlyphRenderer``,
# ``TileRenderer``, ``GraphRenderer``).
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
DataRenderer <- R6::R6Class("DataRenderer",
  inherit = Renderer,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      name = NULL, visible = TRUE, tags = list(), subscribed_events = list(),
      level = "image", js_property_callbacks = structure(list(), .Names =
      character(0)), id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        name = name, visible = visible, tags = tags,
        subscribed_events = subscribed_events, level = level,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["DataRenderer"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Render triangle markers.
# 
# Example -------
# 
# .. bokeh-plot:: ../examples/reference/models/Triangle.py
# :source-position: below
Triangle <- R6::R6Class("Triangle",
  inherit = Marker,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      line_join = "miter", x = NULL, size = 4L, line_width = 1L,
      line_cap = "butt", fill_alpha = 1, y = NULL, angle = 0, line_dash = list(),
      fill_color = "gray", line_color = "black", tags = list(),
      subscribed_events = list(), line_alpha = 1, angle_units = "rad",
      name = NULL, line_dash_offset = 0L, id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks,
        line_join = line_join, x = x, size = size, line_width = line_width,
        line_cap = line_cap, fill_alpha = fill_alpha, y = y, angle = angle,
        line_dash = line_dash, fill_color = fill_color,
        line_color = line_color, tags = tags,
        subscribed_events = subscribed_events, line_alpha = line_alpha,
        angle_units = angle_units, name = name,
        line_dash_offset = line_dash_offset, id = id)
      types <- bk_prop_types[["Triangle"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Open a URL in a new tab or window (browser dependent).
OpenURL <- R6::R6Class("OpenURL",
  inherit = Callback,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      tags = list(), url = "http://", subscribed_events = list(), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["OpenURL"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The URL to direct the web browser to. This can be a template string,
    # which will be formatted with data from the data source.
    # > String
    url = NULL
  )
)

# Abstract base class for groups with items rendered as buttons.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
ButtonGroup <- R6::R6Class("ButtonGroup",
  inherit = AbstractGroup,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      width = NULL, height = NULL, disabled = FALSE, callback = NULL,
      sizing_mode = "fixed", subscribed_events = list(), tags = list(),
      labels = list(), css_classes = NULL, button_type = "default",
      name = NULL, js_property_callbacks = structure(list(), .Names =
      character(0)), id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        width = width, height = height, disabled = disabled,
        callback = callback, sizing_mode = sizing_mode,
        subscribed_events = subscribed_events, tags = tags, labels = labels,
        css_classes = css_classes, name = name,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["ButtonGroup"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # A style for the button, signifying it's role.
    # > Enum('default', 'primary', 'success', 'warning', 'danger', 'link')
    button_type = NULL
  )
)

# *toolbar icon*: |poly_select_icon|
# 
# The polygon selection tool allows users to make selections on a Plot by
# indicating a polygonal region with mouse clicks. single clicks (or
# taps) add successive points to the definition of the polygon, and a
# double click (or tap) indicates the selection region is ready.
# 
# See :ref:`userguide_styling_selected_unselected_glyphs` for information
# on styling selected and unselected glyphs.
# 
# .. note:: Selections can be comprised of multiple regions, even those
# made by different selection tools. Hold down the <<shift>> key while
# making a selection to append the new selection to any previous
# selection that might exist.
# 
# .. |poly_select_icon| image:: /_images/icons/PolygonSelect.png :height:
# 18pt
PolySelectTool <- R6::R6Class("PolySelectTool",
  inherit = Tap,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      renderers = list(), callback = NULL, subscribed_events = list(),
      tags = list(), overlay = NULL, names = list(), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["PolySelectTool"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # An explicit list of renderers to hit test again. If unset, defaults to
    # all renderers on a plot.
    # > List(Instance(Renderer))
    renderers = NULL,
    # A callback to run in the browser on completion of drawing a polygon.
    # The cb_data parameter that is available to the Callback code will
    # contain one PolySelectTool-specific field:
    # 
    # :geometry: object containing the coordinates of the polygon
    # > Instance(Callback)
    callback = NULL,
    # A shaded annotation drawn to indicate the selection region.
    # > Instance(PolyAnnotation)
    overlay = NULL,
    # A list of names to query for. If set, only renderers that have a
    # matching value for their ``name`` attribute will be used.
    # > List(String)
    names = NULL
  )
)

# Abstract base class for map plot models.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
MapPlot <- R6::R6Class("MapPlot",
  inherit = Plot,
  public = list(
    specified_args = NULL,
    initialize = function(
      right = list(), toolbar_sticky = TRUE, title_location = "above",
      x_scale = NULL, min_border_right = NULL, outline_line_alpha = 1,
      inner_width = NULL, background_fill_color = "#ffffff", title = NULL,
      above = list(), subscribed_events = list(),
      outline_line_color = "#e5e5e5", outline_line_dash = list(),
      js_event_callbacks = structure(list(), .Names = character(0)),
      toolbar_location = "right", layout_width = NULL, y_scale = NULL,
      width = NULL, renderers = list(), toolbar = NULL, x_range = NULL,
      outline_line_cap = "butt", plot_height = 600L, y_range = NULL,
      tags = list(), min_border_top = NULL, layout_height = NULL,
      outline_line_width = 1L, aspect_scale = 1L,
      outline_line_dash_offset = 0L, min_border_bottom = NULL,
      output_backend = "canvas", lod_threshold = 2000L, v_symmetry = FALSE,
      lod_factor = 10L, lod_interval = 300L, border_fill_alpha = 1,
      background_fill_alpha = 1, hidpi = TRUE, outline_line_join = "miter",
      left = list(), sizing_mode = "fixed", border_fill_color = "#ffffff",
      css_classes = NULL, min_border_left = NULL, h_symmetry = TRUE,
      min_border = 5L, js_property_callbacks = structure(list(), .Names =
      character(0)), name = NULL, lod_timeout = 500L,
      extra_x_ranges = structure(list(), .Names = character(0)),
      plot_width = 600L, below = list(), height = NULL, disabled = FALSE,
      extra_y_ranges = structure(list(), .Names = character(0)),
      match_aspect = FALSE, inner_height = NULL, id = NULL
    ) {
      super$initialize(right = right, toolbar_sticky = toolbar_sticky,
        title_location = title_location, x_scale = x_scale,
        min_border_right = min_border_right,
        outline_line_alpha = outline_line_alpha, inner_width = inner_width,
        background_fill_color = background_fill_color, title = title,
        above = above, subscribed_events = subscribed_events,
        outline_line_color = outline_line_color,
        outline_line_dash = outline_line_dash,
        js_event_callbacks = js_event_callbacks,
        toolbar_location = toolbar_location, layout_width = layout_width,
        y_scale = y_scale, width = width, renderers = renderers,
        toolbar = toolbar, x_range = x_range,
        outline_line_cap = outline_line_cap, plot_height = plot_height,
        y_range = y_range, tags = tags, min_border_top = min_border_top,
        layout_height = layout_height,
        outline_line_width = outline_line_width,
        aspect_scale = aspect_scale,
        outline_line_dash_offset = outline_line_dash_offset,
        min_border_bottom = min_border_bottom,
        output_backend = output_backend, lod_threshold = lod_threshold,
        v_symmetry = v_symmetry, lod_factor = lod_factor,
        lod_interval = lod_interval, border_fill_alpha = border_fill_alpha,
        background_fill_alpha = background_fill_alpha, hidpi = hidpi,
        outline_line_join = outline_line_join, left = left,
        sizing_mode = sizing_mode, border_fill_color = border_fill_color,
        css_classes = css_classes, min_border_left = min_border_left,
        h_symmetry = h_symmetry, min_border = min_border,
        js_property_callbacks = js_property_callbacks, name = name,
        lod_timeout = lod_timeout, extra_x_ranges = extra_x_ranges,
        plot_width = plot_width, below = below, height = height,
        disabled = disabled, extra_y_ranges = extra_y_ranges,
        match_aspect = match_aspect, inner_height = inner_height, id = id)
      types <- bk_prop_types[["MapPlot"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Collect tools to display for a single plot.
Toolbar <- R6::R6Class("Toolbar",
  inherit = ToolbarBase,
  public = list(
    specified_args = NULL,
    initialize = function(
      active_inspect = "auto", js_event_callbacks = structure(list(),
      .Names = character(0)), active_drag = "auto", active_tap = "auto",
      width = NULL, height = NULL, disabled = FALSE, name = NULL,
      sizing_mode = NULL, tools = list(), subscribed_events = list(),
      tags = list(), logo = "normal", css_classes = NULL,
      active_scroll = "auto", js_property_callbacks = structure(list(),
      .Names = character(0)), id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        width = width, height = height, disabled = disabled,
        sizing_mode = sizing_mode, tools = tools,
        subscribed_events = subscribed_events, tags = tags, logo = logo,
        css_classes = css_classes, name = name,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["Toolbar"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Specify an inspection tool or sequence of inspection tools to be active
    # when the plot is displayed.
    # > Either(Auto, Instance(Inspection), Seq(Instance(Inspection)))
    active_inspect = NULL,
    # Specify a drag tool to be active when the plot is displayed.
    # > Either(Auto, Instance(Drag))
    active_drag = NULL,
    # Specify a tap/click tool to be active when the plot is displayed.
    # > Either(Auto, Instance(Tap))
    active_tap = NULL,
    # Specify a scroll/pinch tool to be active when the plot is displayed.
    # > Either(Auto, Instance(Scroll))
    active_scroll = NULL
  )
)

# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
AbstractSlider <- R6::R6Class("AbstractSlider",
  inherit = Widget,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      orientation = "horizontal", width = NULL, tooltips = TRUE,
      callback_throttle = 200L, height = NULL, disabled = FALSE,
      callback = NULL, css_classes = NULL, sizing_mode = "fixed", name = NULL,
      tags = list(), title = "", format = NULL, subscribed_events = list(),
      callback_policy = "throttle", show_value = TRUE, bar_color = "#e6e6e6",
      direction = "ltr", js_property_callbacks = structure(list(), .Names =
      character(0)), id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        width = width, height = height, disabled = disabled,
        sizing_mode = sizing_mode, tags = tags,
        subscribed_events = subscribed_events, css_classes = css_classes,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["AbstractSlider"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Orient the slider either horizontally (default) or vertically.
    # > Enum('horizontal', 'vertical')
    orientation = NULL,
    # 
    # > Bool
    tooltips = NULL,
    # Number of millseconds to pause between callback calls as the slider is
    # moved.
    # > Float
    callback_throttle = NULL,
    # A callback to run in the browser whenever the current Slider value
    # changes.
    # > Instance(Callback)
    callback = NULL,
    # Slider's label.
    # > String
    title = NULL,
    # 
    # > String
    format = NULL,
    # When the callback is initiated. This parameter can take on only one of
    # three options:
    # 
    # * "continuous": the callback will be executed immediately for each
    # movement of the slider * "throttle": the callback will be executed at
    # most every ``callback_throttle`` milliseconds.  * "mouseup": the
    # callback will be executed only once when the slider is released.
    # 
    # The "mouseup" policy is intended for scenarios in which the callback is
    # expensive in time.
    # > Enum('continuous', 'throttle', 'mouseup')
    callback_policy = NULL,
    # Whether or not show slider's value.
    # > Bool
    show_value = NULL,
    # 
    # > Color
    bar_color = NULL,
    # 
    # > Enum('ltr', 'rtl')
    direction = NULL
  )
)

# Render a closed-body arrow head.
NormalHead <- R6::R6Class("NormalHead",
  inherit = ArrowHead,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      visible = TRUE, line_join = "miter", size = 25L, line_width = 1L,
      line_cap = "butt", fill_alpha = 1, name = NULL, line_dash = list(),
      plot = NULL, fill_color = "black", line_color = "black", tags = list(),
      subscribed_events = list(), line_alpha = 1, level = "annotation",
      line_dash_offset = 0L, id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        visible = visible, name = name, plot = plot, tags = tags,
        subscribed_events = subscribed_events, level = level,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["NormalHead"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The line join values for the arrow head outline.
    # > Enum('miter', 'round', 'bevel')
    line_join = NULL,
    # The size, in pixels, of the arrow head.
    # > Float
    size = NULL,
    # The line width values for the arrow head outline.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_width = NULL,
    # The line cap values for the arrow head outline.
    # > Enum('butt', 'round', 'square')
    line_cap = NULL,
    # The fill alpha values for the arrow head interior.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    fill_alpha = NULL,
    # The line dash values for the arrow head outline.
    # > DashPattern
    line_dash = NULL,
    # The fill color values for the arrow head interior.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    fill_color = NULL,
    # The line color values for the arrow head outline.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    line_color = NULL,
    # The line alpha values for the arrow head outline.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_alpha = NULL,
    # The line dash offset values for the arrow head outline.
    # > Int
    line_dash_offset = NULL
  )
)

# 
AjaxDataSource <- R6::R6Class("AjaxDataSource",
  inherit = RemoteSource,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      mode = "replace", data_url = NULL, http_headers = structure(list(),
      .Names = character(0)), max_size = NULL, column_names = list(),
      content_type = "application/json", method = "POST", callback = NULL,
      tags = list(), data = structure(list(), .Names = character(0)),
      polling_interval = NULL, subscribed_events = list(),
      selected = structure(list(`0d` = structure(list(indices = list(),
      glyph = NULL), .Names = c("indices", "glyph")), `2d` =
      structure(list(indices = structure(list(), .Names =
      character(0))), .Names = "indices"), `1d` =
      structure(list(indices = list()), .Names = "indices")), .Names =
      c("0d", "2d", "1d")), if_modified = FALSE, name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        data_url = data_url, column_names = column_names,
        callback = callback, tags = tags, data = data,
        polling_interval = polling_interval,
        subscribed_events = subscribed_events, selected = selected,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["AjaxDataSource"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Whether to append new data to existing data (up to ``max_size``), or to
    # replace existing data entirely.
    # > Enum('replace', 'append')
    mode = NULL,
    # HTTP headers to set for the Ajax request.
    # > Dict(String, String)
    http_headers = NULL,
    # Maximum size of the data array being kept after each pull requests.
    # Larger than that size, the data will be right shifted.
    # > Int
    max_size = NULL,
    # Set the "contentType" parameter for the Ajax request.
    # > String
    content_type = NULL,
    # http method - GET or POST
    # > Enum('POST', 'GET')
    method = NULL,
    # Whether to include an ``If-Modified-Since`` header in AJAX requests to
    # the server. If this header is supported by the server, then only new
    # data since the last request will be returned.
    # > Bool
    if_modified = NULL
  )
)

# *toolbar icon*: |pan_icon|
# 
# The pan tool allows the user to pan a Plot by left-dragging a mouse, or
# on touch devices by dragging a finger or stylus, across the plot
# region.
# 
# The pan tool also activates the border regions of a Plot for "single
# axis" panning. For instance, dragging in the vertical border or axis
# will effect a pan in the vertical direction only, with the horizontal
# dimension kept fixed.
# 
# .. |pan_icon| image:: /_images/icons/Pan.png :height: 18pt
PanTool <- R6::R6Class("PanTool",
  inherit = Drag,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      tags = list(), dimensions = "both", subscribed_events = list(),
      name = NULL, js_property_callbacks = structure(list(), .Names =
      character(0)), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["PanTool"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Which dimensions the pan tool is constrained to act in. By default the
    # pan tool will pan in any dimension, but can be configured to only pan
    # horizontally across the width of the plot, or vertically across the
    # height of the plot.
    # > Enum('width', 'height', 'both')
    dimensions = NULL
  )
)

# A base class for tools that are buttons in the toolbar.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
Action <- R6::R6Class("Action",
  inherit = Tool,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Action"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Base class of glyphs with `x` and `y` coordinate attributes.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
XYGlyph <- R6::R6Class("XYGlyph",
  inherit = Glyph,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["XYGlyph"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Render wedges.
# 
# Example -------
# 
# .. bokeh-plot:: ../examples/reference/models/Wedge.py :source-position:
# below
Wedge <- R6::R6Class("Wedge",
  inherit = XYGlyph,
  public = list(
    specified_args = NULL,
    initialize = function(
      radius_units = "data", line_cap = "butt", fill_alpha = 1,
      start_angle_units = "rad", radius = NULL, start_angle = NULL,
      line_alpha = 1, subscribed_events = list(), line_dash_offset = 0L,
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      name = NULL, end_angle_units = "rad", line_join = "miter", x = NULL,
      line_width = 1L, y = NULL, end_angle = NULL, line_dash = list(),
      fill_color = "gray", line_color = "black", tags = list(),
      direction = "anticlock", id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Wedge"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # 
    # > Enum('screen', 'data')
    radius_units = NULL,
    # The line cap values for the wedges.
    # > Enum('butt', 'round', 'square')
    line_cap = NULL,
    # The fill alpha values for the wedges.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    fill_alpha = NULL,
    # 
    # > Enum('deg', 'rad')
    start_angle_units = NULL,
    # Radii of the wedges.
    # > DistanceSpec(units_default='data')
    radius = NULL,
    # The angles to start the wedges, as measured from the horizontal.
    # > AngleSpec(units_default='rad')
    start_angle = NULL,
    # The line alpha values for the wedges.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_alpha = NULL,
    # The line dash offset values for the wedges.
    # > Int
    line_dash_offset = NULL,
    # 
    # > Enum('deg', 'rad')
    end_angle_units = NULL,
    # The line join values for the wedges.
    # > Enum('miter', 'round', 'bevel')
    line_join = NULL,
    # The x-coordinates of the points of the wedges.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    x = NULL,
    # The line width values for the wedges.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_width = NULL,
    # The y-coordinates of the points of the wedges.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    y = NULL,
    # The angles to end the wedges, as measured from the horizontal.
    # > AngleSpec(units_default='rad')
    end_angle = NULL,
    # The line dash values for the wedges.
    # > DashPattern
    line_dash = NULL,
    # The fill color values for the wedges.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    fill_color = NULL,
    # The line color values for the wedges.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    line_color = NULL,
    # Which direction to stroke between the start and end angles.
    # > Enum('clock', 'anticlock')
    direction = NULL
  )
)

# With the NodesAndLinkedEdges policy, inspection or selection of graph
# nodes will result in the inspection or selection of the node and of the
# linked graph edges. There is no direct selection or inspection of graph
# edges.
NodesAndLinkedEdges <- R6::R6Class("NodesAndLinkedEdges",
  inherit = GraphHitTestPolicy,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["NodesAndLinkedEdges"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# 
GraphRenderer <- R6::R6Class("GraphRenderer",
  inherit = DataRenderer,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      visible = TRUE, x_range_name = "default", node_renderer = NULL,
      selection_policy = NULL, layout_provider = NULL, name = NULL,
      y_range_name = "default", tags = list(), subscribed_events = list(),
      edge_renderer = NULL, inspection_policy = NULL, level = "glyph",
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        name = name, visible = visible, tags = tags,
        subscribed_events = subscribed_events, level = level,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["GraphRenderer"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # A particular (named) x-range to use for computing screen locations when
    # rendering graphs on the plot. If unset, use the default x-range.
    # > String
    x_range_name = NULL,
    # Instance of a GlyphRenderer containing an XYGlyph that will be rendered
    # as the graph nodes.
    # > Instance(GlyphRenderer)
    node_renderer = NULL,
    # An instance of a GraphHitTestPolicy that provides the logic for
    # selection of graph components.
    # > Instance(GraphHitTestPolicy)
    selection_policy = NULL,
    # An instance of a LayoutProvider that supplies the layout of the network
    # graph in cartesian space.
    # > Instance(LayoutProvider)
    layout_provider = NULL,
    # A particular (named) y-range to use for computing screen locations when
    # rendering graphs on the plot. If unset, use the default -range.
    # > String
    y_range_name = NULL,
    # Instance of a GlyphRenderer containing an MultiLine Glyph that will be
    # rendered as the graph edges.
    # > Instance(GlyphRenderer)
    edge_renderer = NULL,
    # An instance of a GraphHitTestPolicy that provides the logic for
    # inspection of graph components.
    # > Instance(GraphHitTestPolicy)
    inspection_policy = NULL
  )
)

# Render ovals.
# 
# This glyph renders ovals using Bézier curves, which are similar, but
# not identical to ellipses. In particular, widths equal to heights will
# not render circles. Use the ``Ellipse`` glyph for that.
# 
# Example -------
# 
# .. bokeh-plot:: ../examples/reference/models/Oval.py :source-position:
# below
Oval <- R6::R6Class("Oval",
  inherit = XYGlyph,
  public = list(
    specified_args = NULL,
    initialize = function(
      line_cap = "butt", fill_alpha = 1, angle = 0, width_units = "data",
      subscribed_events = list(), line_alpha = 1, angle_units = "rad",
      line_dash_offset = 0L, js_event_callbacks = structure(list(), .Names
      = character(0)), js_property_callbacks = structure(list(), .Names =
      character(0)), name = NULL, line_join = "miter", width = NULL, x = NULL,
      height_units = "data", line_width = 1L, height = NULL, y = NULL,
      line_dash = list(), fill_color = "gray", line_color = "black",
      tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Oval"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The line cap values for the ovals.
    # > Enum('butt', 'round', 'square')
    line_cap = NULL,
    # The fill alpha values for the ovals.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    fill_alpha = NULL,
    # The angle the ovals are rotated from horizontal. [rad]
    # > AngleSpec(units_default='rad')
    angle = NULL,
    # 
    # > Enum('screen', 'data')
    width_units = NULL,
    # The line alpha values for the ovals.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_alpha = NULL,
    # 
    # > Enum('deg', 'rad')
    angle_units = NULL,
    # The line dash offset values for the ovals.
    # > Int
    line_dash_offset = NULL,
    # The line join values for the ovals.
    # > Enum('miter', 'round', 'bevel')
    line_join = NULL,
    # The overall widths of each oval.
    # > DistanceSpec(units_default='data')
    width = NULL,
    # The x-coordinates of the centers of the ovals.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    x = NULL,
    # 
    # > Enum('screen', 'data')
    height_units = NULL,
    # The line width values for the ovals.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_width = NULL,
    # The overall height of each oval.
    # > DistanceSpec(units_default='data')
    height = NULL,
    # The y-coordinates of the centers of the ovals.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    y = NULL,
    # The line dash values for the ovals.
    # > DashPattern
    line_dash = NULL,
    # The fill color values for the ovals.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    fill_color = NULL,
    # The line color values for the ovals.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    line_color = NULL
  )
)

# Render '+' cross markers.
# 
# Example -------
# 
# .. bokeh-plot:: ../examples/reference/models/Cross.py :source-position:
# below
Cross <- R6::R6Class("Cross",
  inherit = Marker,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      line_join = "miter", x = NULL, size = 4L, line_width = 1L,
      line_cap = "butt", fill_alpha = 1, y = NULL, angle = 0, line_dash = list(),
      fill_color = "gray", line_color = "black", tags = list(),
      subscribed_events = list(), line_alpha = 1, angle_units = "rad",
      name = NULL, line_dash_offset = 0L, id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks,
        line_join = line_join, x = x, size = size, line_width = line_width,
        line_cap = line_cap, fill_alpha = fill_alpha, y = y, angle = angle,
        line_dash = line_dash, fill_color = fill_color,
        line_color = line_color, tags = tags,
        subscribed_events = subscribed_events, line_alpha = line_alpha,
        angle_units = angle_units, name = name,
        line_dash_offset = line_dash_offset, id = id)
      types <- bk_prop_types[["Cross"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# HTML formatter using a template.  This uses Underscore's `template`
# method and syntax.  http://underscorejs.org/#template The formatter has
# access other items in the row via the `dataContext` object passed to
# the formatter.  So, for example, if another column in the datasource
# was named `url`, the template could access it as:
# 
# <a href="<%= url %>"><%= value %></a>
# 
# To use a different set of template delimiters, pass the appropriate
# values for `evaluate`, `interpolate', or `escape`.  See the Underscore
# `template` documentation for more information.
# http://underscorejs.org/#template
# 
# Example: Simple HTML template to format the column value as code.
# 
# HTMLTemplateFormatter(template='<code><%= value %></code>')
# 
# Example: Use values from other columns (`manufacturer` and `model`) to
# build a hyperlink.
# 
# HTMLTemplateFormatter(template='<a
# href="https:/www.google.com/search?q=<%= manufacturer %>+<%= model %>"
# target="_blank"><%= value %></a>')
HTMLTemplateFormatter <- R6::R6Class("HTMLTemplateFormatter",
  inherit = CellFormatter,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      template = "<%= value %>", tags = list(), subscribed_events = list(),
      name = NULL, js_property_callbacks = structure(list(), .Names =
      character(0)), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["HTMLTemplateFormatter"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Template string to be used by Underscore's template method.
    # > String
    template = NULL
  )
)

# Display tick values that are formatted by a user-defined function.
FuncTickFormatter <- R6::R6Class("FuncTickFormatter",
  inherit = TickFormatter,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      tags = list(), code = "", args = structure(list(), .Names =
      character(0)), subscribed_events = list(), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["FuncTickFormatter"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # A snippet of JavaScript code that reformats a single tick to the
    # desired format. The variable ``tick`` will contain the unformatted tick
    # value and can be expected to be present in the code snippet namespace
    # at render time.
    # 
    # Example:
    # 
    # .. code-block:: javascript
    # 
    # code = ''' return Math.floor(tick) + " + " + (tick % 1).toFixed(2) '''
    # > String
    code = NULL,
    # A mapping of names to Bokeh plot objects. These objects are made
    # available to the formatter code snippet as the values of named
    # parameters to the callback.
    # > Dict(String, Instance(Model))
    args = NULL
  )
)

# With the NodesOnly policy, only graph nodes are able to be selected and
# inspected. There is no selection or inspection of graph edges.
NodesOnly <- R6::R6Class("NodesOnly",
  inherit = GraphHitTestPolicy,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["NodesOnly"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Two dimensional grid for visualisation and editing large amounts of
# data.
DataTable <- R6::R6Class("DataTable",
  inherit = TableWidget,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      sortable = TRUE, view = NULL, width = NULL, fit_columns = TRUE,
      columns = list(), source = NULL, height = 400L, disabled = FALSE,
      name = NULL, sizing_mode = "fixed", scroll_to_selection = TRUE,
      reorderable = TRUE, subscribed_events = list(), tags = list(),
      editable = FALSE, css_classes = NULL, selectable = TRUE,
      row_headers = TRUE, js_property_callbacks = structure(list(), .Names
      = character(0)), id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        view = view, width = width, height = height, source = source,
        disabled = disabled, sizing_mode = sizing_mode, tags = tags,
        subscribed_events = subscribed_events, css_classes = css_classes,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["DataTable"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Allows to sort table's contents. By default natural order is preserved.
    # To sort a column, click on it's header. Clicking one more time changes
    # sort direction. Use Ctrl + click to return to natural order. Use Shift
    # + click to sort multiple columns simultaneously.
    # > Bool
    sortable = NULL,
    # Whether columns should be fit to the available width. This results in
    # no horizontal scrollbar showing up, but data can get unreadable if
    # there is no enough space available. If set to ``True``, columns' width
    # is understood as maximum width.
    # > Bool
    fit_columns = NULL,
    # The list of child column widgets.
    # > List(Instance(TableColumn))
    columns = NULL,
    # Whenever a selection is made on the data source, scroll the selected
    # rows into the table's viewport if none of the selected rows are already
    # in the viewport.
    # > Bool
    scroll_to_selection = NULL,
    # Allows the reordering of a tables's columns. To reorder a column, click
    # and drag a table's header to the desired location in the table.  The
    # columns on either side will remain in their previous order.
    # > Bool
    reorderable = NULL,
    # Allows to edit table's contents. Needs cell editors to be configured on
    # columns that are required to be editable.
    # > Bool
    editable = NULL,
    # Whether a table's rows can be selected or not. Using ``checkbox`` is
    # equivalent to ``True``, but makes selection visible through a checkbox
    # for each row, instead of highlighting rows. Multiple selection is
    # allowed and can be achieved by either clicking multiple checkboxes (if
    # enabled) or using Shift + click on rows.
    # > Either(Bool, Enum('checkbox'))
    selectable = NULL,
    # Enable or disable row headers, i.e. the index column.
    # > Bool
    row_headers = NULL
  )
)

# A two-state toggle button.
Toggle <- R6::R6Class("Toggle",
  inherit = AbstractButton,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      icon = NULL, width = NULL, height = NULL, disabled = FALSE,
      callback = NULL, tags = list(), sizing_mode = "fixed", active = FALSE,
      subscribed_events = list(), css_classes = NULL, label = "Toggle",
      button_type = "default", name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        icon = icon, width = width, height = height, disabled = disabled,
        callback = callback, sizing_mode = sizing_mode, tags = tags,
        subscribed_events = subscribed_events, css_classes = css_classes,
        label = label, button_type = button_type, name = name,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["Toggle"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The initial state of a button. Also used to trigger ``on_click`` event
    # handler.
    # > Bool
    active = NULL
  )
)

# Render a shaded rectangular region as an annotation.
BoxAnnotation <- R6::R6Class("BoxAnnotation",
  inherit = Annotation,
  public = list(
    specified_args = NULL,
    initialize = function(
      right = NULL, line_cap = "butt", x_range_name = "default", bottom = NULL,
      top_units = "data", fill_alpha = 0.4, y_range_name = "default",
      top = NULL, left = NULL, subscribed_events = list(), line_alpha = 0.3,
      line_dash_offset = 0L, js_event_callbacks = structure(list(), .Names
      = character(0)), js_property_callbacks = structure(list(), .Names =
      character(0)), right_units = "data", visible = TRUE,
      line_join = "miter", name = NULL, bottom_units = "data", line_width = 1L,
      left_units = "data", line_dash = list(), plot = NULL,
      fill_color = "#fff9ba", line_color = "#cccccc", tags = list(),
      render_mode = "canvas", level = "annotation", id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        visible = visible, name = name, plot = plot, tags = tags,
        subscribed_events = subscribed_events, level = level,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["BoxAnnotation"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The x-coordinates of the right edge of the box annotation.
    # 
    # Datetime values are also accepted, but note that they are immediately
    # converted to milliseconds-since-epoch.
    # > Either(Auto, NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float))
    right = NULL,
    # The line cap values for the box.
    # > Enum('butt', 'round', 'square')
    line_cap = NULL,
    # A particular (named) x-range to use for computing screen locations when
    # rendering box annotations on the plot. If unset, use the default
    # x-range.
    # > String
    x_range_name = NULL,
    # The y-coordinates of the bottom edge of the box annotation.
    # 
    # Datetime values are also accepted, but note that they are immediately
    # converted to milliseconds-since-epoch.
    # > Either(Auto, NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float))
    bottom = NULL,
    # The unit type for the top attribute. Interpreted as "data space" units
    # by default.
    # > Enum('screen', 'data')
    top_units = NULL,
    # The fill alpha values for the box.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    fill_alpha = NULL,
    # A particular (named) y-range to use for computing screen locations when
    # rendering box annotations on the plot. If unset, use the default
    # y-range.
    # > String
    y_range_name = NULL,
    # The y-coordinates of the top edge of the box annotation.
    # 
    # Datetime values are also accepted, but note that they are immediately
    # converted to milliseconds-since-epoch.
    # > Either(Auto, NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float))
    top = NULL,
    # The x-coordinates of the left edge of the box annotation.
    # 
    # Datetime values are also accepted, but note that they are immediately
    # converted to milliseconds-since-epoch.
    # > Either(Auto, NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float))
    left = NULL,
    # The line alpha values for the box.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_alpha = NULL,
    # The line dash offset values for the box.
    # > Int
    line_dash_offset = NULL,
    # The unit type for the right attribute. Interpreted as "data space"
    # units by default.
    # > Enum('screen', 'data')
    right_units = NULL,
    # The line join values for the box.
    # > Enum('miter', 'round', 'bevel')
    line_join = NULL,
    # The unit type for the bottom attribute. Interpreted as "data space"
    # units by default.
    # > Enum('screen', 'data')
    bottom_units = NULL,
    # The line width values for the box.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_width = NULL,
    # The unit type for the left attribute. Interpreted as "data space" units
    # by default.
    # > Enum('screen', 'data')
    left_units = NULL,
    # The line dash values for the box.
    # > DashPattern
    line_dash = NULL,
    # The fill color values for the box.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    fill_color = NULL,
    # The line color values for the box.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    line_color = NULL,
    # Specifies whether the box is rendered as a canvas element or as an css
    # element overlaid on the canvas. The default mode is "canvas".
    # 
    # .. warning:: The line_dash and line_dash_offset attributes aren't
    # supported if the render_mode is set to "css"
    # > Enum('canvas', 'css')
    render_mode = NULL
  )
)

# 
StaticLayoutProvider <- R6::R6Class("StaticLayoutProvider",
  inherit = LayoutProvider,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      graph_layout = structure(list(), .Names = character(0)),
      tags = list(), subscribed_events = list(), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["StaticLayoutProvider"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The coordinates of the graph nodes in cartesian space. The dictionary
    # keys correspond to a node index and the values are a two element
    # sequence containing the x and y coordinates of the node.
    # 
    # .. code-block:: python
    # 
    # { 0 : [0.5, 0.5], 1 : [1.0, 0.86], 2 : [0.86, 1], }
    # > Dict(Either(String, Int), Seq(Any))
    graph_layout = NULL
  )
)

# Render square markers with a '+' cross through the center.
# 
# Example -------
# 
# .. bokeh-plot:: ../examples/reference/models/SquareCross.py
# :source-position: below
SquareCross <- R6::R6Class("SquareCross",
  inherit = Marker,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      line_join = "miter", x = NULL, size = 4L, line_width = 1L,
      line_cap = "butt", fill_alpha = 1, y = NULL, angle = 0, line_dash = list(),
      fill_color = "gray", line_color = "black", tags = list(),
      subscribed_events = list(), line_alpha = 1, angle_units = "rad",
      name = NULL, line_dash_offset = 0L, id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks,
        line_join = line_join, x = x, size = size, line_width = line_width,
        line_cap = line_cap, fill_alpha = fill_alpha, y = y, angle = angle,
        line_dash = line_dash, fill_color = fill_color,
        line_color = line_color, tags = tags,
        subscribed_events = subscribed_events, line_alpha = line_alpha,
        angle_units = angle_units, name = name,
        line_dash_offset = line_dash_offset, id = id)
      types <- bk_prop_types[["SquareCross"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Render a color bar based on a color mapper.
ColorBar <- R6::R6Class("ColorBar",
  inherit = Annotation,
  public = list(
    specified_args = NULL,
    initialize = function(
      major_tick_line_color = "#ffffff", minor_tick_line_dash = list(),
      formatter = NULL, background_fill_color = "#ffffff",
      js_event_callbacks = structure(list(), .Names = character(0)),
      minor_tick_line_join = "miter", major_label_text_font = "helvetica",
      major_label_text_line_height = 1.2, width = "auto",
      major_label_text_font_size = list(value = "8pt"), bar_line_alpha = 1,
      location = "top_right", plot = NULL, border_line_dash = list(),
      bar_line_cap = "butt", major_label_text_baseline = "middle",
      background_fill_alpha = 0.95, major_label_text_color = "#444444",
      title_text_align = "left", title_text_color = "#444444",
      title_text_font_style = "italic", bar_line_color = NULL,
      orientation = "vertical", name = NULL, visible = TRUE,
      border_line_join = "miter", bar_line_dash = list(),
      major_tick_line_join = "miter", border_line_dash_offset = 0L,
      major_tick_line_dash_offset = 0L, major_tick_line_dash = list(),
      major_tick_line_cap = "butt", title_text_baseline = "bottom",
      title_text_line_height = 1.2, scale_alpha = 1,
      title_text_font_size = list(value = "10pt"),
      major_tick_line_width = 1L, ticker = NULL, title = NULL,
      minor_tick_line_color = NULL, subscribed_events = list(),
      major_label_text_font_style = "normal", minor_tick_out = 0L,
      border_line_color = NULL, major_tick_in = 5L,
      minor_tick_line_dash_offset = 0L, border_line_cap = "butt",
      title_text_font = "helvetica", tags = list(), padding = 10L,
      level = "annotation", bar_line_width = 1L, major_tick_out = 0L,
      minor_tick_line_alpha = 1, major_label_text_alpha = 1,
      major_label_overrides = structure(list(), .Names = character(0)),
      color_mapper = NULL, title_text_alpha = 1, major_tick_line_alpha = 1,
      bar_line_dash_offset = 0L, major_label_text_align = "center",
      js_property_callbacks = structure(list(), .Names = character(0)),
      bar_line_join = "miter", title_standoff = 2L, height = "auto",
      minor_tick_line_width = 1L, border_line_alpha = 1,
      minor_tick_line_cap = "butt", minor_tick_in = 0L, label_standoff = 5L,
      border_line_width = 1L, margin = 30L, id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        visible = visible, name = name, plot = plot, tags = tags,
        subscribed_events = subscribed_events, level = level,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["ColorBar"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The line color of the major ticks.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    major_tick_line_color = NULL,
    # The line dash of the minor ticks.
    # > DashPattern
    minor_tick_line_dash = NULL,
    # A TickFormatter to use for formatting the visual appearance of ticks.
    # > Instance(TickFormatter)
    formatter = NULL,
    # The fill color for the color bar background style.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    background_fill_color = NULL,
    # The line join of the minor ticks.
    # > Enum('miter', 'round', 'bevel')
    minor_tick_line_join = NULL,
    # The text font of the major tick labels.
    # > String
    major_label_text_font = NULL,
    # The text line height of the major tick labels.
    # > Float
    major_label_text_line_height = NULL,
    # The width (in pixels) that the color scale should occupy.
    # > Either(Auto, Int)
    width = NULL,
    # The text font size of the major tick labels.
    # > FontSizeSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), List(String))), List(String))
    major_label_text_font_size = NULL,
    # The line alpha for the color scale bar outline.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    bar_line_alpha = NULL,
    # The location where the color bar should draw itself. It's either one of
    # ``bokeh.core.enums.LegendLocation``'s enumerated values, or a ``(x,
    # y)`` tuple indicating an absolute location absolute location in screen
    # coordinates (pixels from the bottom-left corner).
    # 
    # .. warning:: If the color bar is placed in a side panel, the location
    # will likely have to be set to `(0,0)`.
    # > Either(Enum('top_left', 'top_center', 'top_right', 'center_left', 'center', 'center_right', 'bottom_left', 'bottom_center', 'bottom_right'), Tuple(Float, Float))
    location = NULL,
    # The line dash for the color bar border outline.
    # > DashPattern
    border_line_dash = NULL,
    # The line cap for the color scale bar outline.
    # > Enum('butt', 'round', 'square')
    bar_line_cap = NULL,
    # The text baseline of the major tick labels.
    # > Enum('top', 'middle', 'bottom', 'alphabetic', 'hanging', 'ideographic')
    major_label_text_baseline = NULL,
    # The fill alpha for the color bar background style.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    background_fill_alpha = NULL,
    # The text color of the major tick labels.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    major_label_text_color = NULL,
    # The text align values for the title text.
    # > Enum('left', 'right', 'center')
    title_text_align = NULL,
    # The text color values for the title text.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    title_text_color = NULL,
    # The text font style values for the title text.
    # > Enum('normal', 'italic', 'bold')
    title_text_font_style = NULL,
    # The line color for the color scale bar outline.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    bar_line_color = NULL,
    # Whether the color bar should be oriented vertically or horizontally.
    # > Enum('horizontal', 'vertical')
    orientation = NULL,
    # The line join for the color bar border outline.
    # > Enum('miter', 'round', 'bevel')
    border_line_join = NULL,
    # The line dash for the color scale bar outline.
    # > DashPattern
    bar_line_dash = NULL,
    # The line join of the major ticks.
    # > Enum('miter', 'round', 'bevel')
    major_tick_line_join = NULL,
    # The line dash offset for the color bar border outline.
    # > Int
    border_line_dash_offset = NULL,
    # The line dash offset of the major ticks.
    # > Int
    major_tick_line_dash_offset = NULL,
    # The line dash of the major ticks.
    # > DashPattern
    major_tick_line_dash = NULL,
    # The line cap of the major ticks.
    # > Enum('butt', 'round', 'square')
    major_tick_line_cap = NULL,
    # The text baseline values for the title text.
    # > Enum('top', 'middle', 'bottom', 'alphabetic', 'hanging', 'ideographic')
    title_text_baseline = NULL,
    # The text line height values for the title text.
    # > Float
    title_text_line_height = NULL,
    # The alpha with which to render the color scale.
    # > Float
    scale_alpha = NULL,
    # The text font size values for the title text.
    # > FontSizeSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), List(String))), List(String))
    title_text_font_size = NULL,
    # The line width of the major ticks.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    major_tick_line_width = NULL,
    # A Ticker to use for computing locations of axis components.
    # > Instance(Ticker)
    ticker = NULL,
    # The title text to render.
    # > String
    title = NULL,
    # The line color of the minor ticks.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    minor_tick_line_color = NULL,
    # The text font style of the major tick labels.
    # > Enum('normal', 'italic', 'bold')
    major_label_text_font_style = NULL,
    # The distance (in pixels) that major ticks should extend out of the main
    # plot area.
    # > Int
    minor_tick_out = NULL,
    # The line color for the color bar border outline.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    border_line_color = NULL,
    # The distance (in pixels) that major ticks should extend into the main
    # plot area.
    # > Int
    major_tick_in = NULL,
    # The line dash offset of the minor ticks.
    # > Int
    minor_tick_line_dash_offset = NULL,
    # The line cap for the color bar border outline.
    # > Enum('butt', 'round', 'square')
    border_line_cap = NULL,
    # The text font values for the title text.
    # > String
    title_text_font = NULL,
    # Amount of padding (in pixels) between the color scale and color bar
    # border.
    # > Int
    padding = NULL,
    # The line width for the color scale bar outline.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    bar_line_width = NULL,
    # The distance (in pixels) that major ticks should extend out of the main
    # plot area.
    # > Int
    major_tick_out = NULL,
    # The line alpha of the minor ticks.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    minor_tick_line_alpha = NULL,
    # The text alpha of the major tick labels.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    major_label_text_alpha = NULL,
    # Provide explicit tick label values for specific tick locations that
    # override normal formatting.
    # > Dict(Either(Float, String), String)
    major_label_overrides = NULL,
    # A continuous color mapper containing a color palette to render.
    # 
    # .. warning:: If the `low` and `high` attributes of the ColorMapper
    # aren't set, ticks and tick labels won't be rendered. Additionally, if a
    # LogTicker is passed to the `ticker` argument and either or both of the
    # logarithms of `low` and `high` values of the color_mapper are
    # non-numeric (i.e. `low=0`), the tick and tick labels won't be rendered.
    # > Instance(ContinuousColorMapper)
    color_mapper = NULL,
    # The text alpha values for the title text.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    title_text_alpha = NULL,
    # The line alpha of the major ticks.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    major_tick_line_alpha = NULL,
    # The line dash offset for the color scale bar outline.
    # > Int
    bar_line_dash_offset = NULL,
    # The text align of the major tick labels.
    # > Enum('left', 'right', 'center')
    major_label_text_align = NULL,
    # The line join for the color scale bar outline.
    # > Enum('miter', 'round', 'bevel')
    bar_line_join = NULL,
    # The distance (in pixels) to separate the title from the color bar.
    # > Int
    title_standoff = NULL,
    # The height (in pixels) that the color scale should occupy.
    # > Either(Auto, Int)
    height = NULL,
    # The line width of the minor ticks.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    minor_tick_line_width = NULL,
    # The line alpha for the color bar border outline.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    border_line_alpha = NULL,
    # The line cap of the minor ticks.
    # > Enum('butt', 'round', 'square')
    minor_tick_line_cap = NULL,
    # The distance (in pixels) that minor ticks should extend into the main
    # plot area.
    # > Int
    minor_tick_in = NULL,
    # The distance (in pixels) to separate the tick labels from the color
    # bar.
    # > Int
    label_standoff = NULL,
    # The line width for the color bar border outline.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    border_line_width = NULL,
    # Amount of margin (in pixels) around the outside of the color bar.
    # > Int
    margin = NULL
  )
)

# An auto-fitting range in a continuous scalar dimension.  The upper and
# lower bounds are set to the min and max of the data.
DataRange1d <- R6::R6Class("DataRange1d",
  inherit = DataRange,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      follow = NULL, end = NULL, flipped = FALSE, min_interval = NULL,
      renderers = list(), follow_interval = NULL, range_padding = 0.1,
      bounds = NULL, max_interval = NULL, callback = NULL, tags = list(),
      range_padding_units = "percent", default_span = 2,
      subscribed_events = list(), start = NULL, names = list(), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        renderers = renderers, callback = callback, tags = tags,
        subscribed_events = subscribed_events, names = names, name = name,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["DataRange1d"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Configure the data to follow one or the other data extreme, with a
    # maximum range size of ``follow_interval``.
    # 
    # If set to ``"start"`` then the range will adjust so that ``start``
    # always corresponds to the minimum data value (or maximum, if
    # ``flipped`` is ``True``).
    # 
    # If set to ``"end"`` then the range will adjust so that ``end`` always
    # corresponds to the maximum data value (or minimum, if ``flipped`` is
    # ``True``).
    # 
    # If set to ``None`` (default), then auto-ranging does not follow, and
    # the range will encompass both the minimum and maximum data values.
    # 
    # ``follow`` cannot be used with bounds, and if set, bounds will be set
    # to ``None``.
    # > Enum('start', 'end')
    follow = NULL,
    # An explicitly supplied range end. If provided, will override
    # automatically computed end value.
    # > Float
    end = NULL,
    # Whether the range should be "flipped" from its normal direction when
    # auto-ranging.
    # > Bool
    flipped = NULL,
    # The level that the range is allowed to zoom in, expressed as the
    # minimum visible interval. If set to ``None`` (default), the minimum
    # interval is not bound.
    # > Float
    min_interval = NULL,
    # If ``follow`` is set to ``"start"`` or ``"end"`` then the range will
    # always be constrained to that::
    # 
    # abs(r.start - r.end) <= follow_interval
    # 
    # is maintained.
    # > Float
    follow_interval = NULL,
    # How much padding to add around the computed data bounds.
    # 
    # When ``range_padding_units`` is set to ``"percent"``, the span of the
    # range span is expanded to make the range ``range_padding`` percent
    # larger.
    # 
    # When ``range_padding_units`` is set to ``"absolute"``, the start and
    # end of the range span are extended by the amount ``range_padding``.
    # > Float
    range_padding = NULL,
    # The bounds that the range is allowed to go to. Typically used to
    # prevent the user from panning/zooming/etc away from the data.
    # 
    # By default, the bounds will be None, allowing your plot to pan/zoom as
    # far as you want. If bounds are 'auto' they will be computed to be the
    # same as the start and end of the DataRange1d.
    # 
    # Bounds are provided as a tuple of ``(min, max)`` so regardless of
    # whether your range is increasing or decreasing, the first item should
    # be the minimum value of the range and the second item should be the
    # maximum.  Setting ``min > max`` will result in a ``ValueError``.
    # 
    # If you only want to constrain one end of the plot, you can set ``min``
    # or ``max`` to ``None`` e.g. ``DataRange1d(bounds=(None, 12))``
    # > MinMaxBounds(Auto, Tuple(Float, Float))
    bounds = NULL,
    # The level that the range is allowed to zoom out, expressed as the
    # maximum visible interval. Note that ``bounds`` can impose an implicit
    # constraint on the maximum interval as well.
    # > Float
    max_interval = NULL,
    # Whether the ``range_padding`` should be interpreted as a percentage, or
    # as an absolute quantity. (default: ``"percent"``)
    # > Enum('percent', 'absolute')
    range_padding_units = NULL,
    # A default width for the interval, in case ``start`` is equal to ``end``
    # (if used with a log axis, default_span is in powers of 10).
    # > Float
    default_span = NULL,
    # An explicitly supplied range start. If provided, will override
    # automatically computed start value.
    # > Float
    start = NULL
  )
)

# A base class for all ticker types.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
Ticker <- R6::R6Class("Ticker",
  inherit = Model,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Ticker"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# *toolbar icon*: |box_select_icon|
# 
# The box selection tool allows users to make selections on a Plot by
# indicating a rectangular region by dragging the mouse or a finger over
# the plot region. The end of the drag event indicates the selection
# region is ready.
# 
# See :ref:`userguide_styling_selected_unselected_glyphs` for information
# on styling selected and unselected glyphs.
# 
# .. |box_select_icon| image:: /_images/icons/BoxSelect.png :height: 18pt
BoxSelectTool <- R6::R6Class("BoxSelectTool",
  inherit = Drag,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      dimensions = "both", renderers = list(), callback = NULL,
      select_every_mousemove = FALSE, subscribed_events = list(),
      tags = list(), overlay = NULL, names = list(), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["BoxSelectTool"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Which dimensions the box selection is to be free in. By default, users
    # may freely draw selections boxes with any dimensions. If only "width"
    # is supplied, the box will be constrained to span the entire vertical
    # space of the plot, only the horizontal dimension can be controlled. If
    # only "height" is supplied, the box will be constrained to span the
    # entire horizontal space of the plot, and the vertical dimension can be
    # controlled.
    # > Enum('width', 'height', 'both')
    dimensions = NULL,
    # An explicit list of renderers to hit test again. If unset, defaults to
    # all renderers on a plot.
    # > List(Instance(Renderer))
    renderers = NULL,
    # A callback to run in the browser on completion of drawing a selection
    # box.  The cb_data parameter that is available to the Callback code will
    # contain one BoxSelectTool-specific field:
    # 
    # :geometry: object containing the coordinates of the selection box
    # > Instance(Callback)
    callback = NULL,
    # Whether a selection computation should happen on every mouse event, or
    # only once, when the selection region is completed. Default: False
    # > Bool
    select_every_mousemove = NULL,
    # A shaded annotation drawn to indicate the selection region.
    # > Instance(BoxAnnotation)
    overlay = NULL,
    # A list of names to query for. If set, only renderers that have a
    # matching value for their ``name`` attribute will be used.
    # > List(String)
    names = NULL
  )
)

# Generate ticks on a log scale.
LogTicker <- R6::R6Class("LogTicker",
  inherit = AdaptiveTicker,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      mantissas = list(1L, 5L), base = 10, num_minor_ticks = 5L,
      min_interval = 0, desired_num_ticks = 6L, max_interval = NULL,
      tags = list(), subscribed_events = list(), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        mantissas = mantissas, base = base,
        num_minor_ticks = num_minor_ticks, min_interval = min_interval,
        desired_num_ticks = desired_num_ticks, max_interval = max_interval,
        tags = tags, subscribed_events = subscribed_events, name = name,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["LogTicker"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# A base class for all range types.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
Range <- R6::R6Class("Range",
  inherit = Model,
  public = list(
    specified_args = NULL,
    initialize = function(
      callback = NULL, js_event_callbacks = structure(list(), .Names =
      character(0)), tags = list(), subscribed_events = list(), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Range"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # A callback to run in the browser whenever the range is updated.
    # > Instance(Callback)
    callback = NULL
  )
)

# Render a single line.
# 
# The ``Line`` glyph is different from most other glyphs in that the
# vector of values only produces one glyph on the Plot.
# 
# Example -------
# 
# .. bokeh-plot:: ../examples/reference/models/Line.py :source-position:
# below
Line <- R6::R6Class("Line",
  inherit = XYGlyph,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      line_join = "miter", x = NULL, line_width = 1L, line_cap = "butt",
      y = NULL, line_dash = list(), line_color = "black", tags = list(),
      subscribed_events = list(), line_alpha = 1, name = NULL,
      line_dash_offset = 0L, id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Line"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The line join values for the line.
    # > Enum('miter', 'round', 'bevel')
    line_join = NULL,
    # The x-coordinates for the points of the line.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    x = NULL,
    # The line width values for the line.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_width = NULL,
    # The line cap values for the line.
    # > Enum('butt', 'round', 'square')
    line_cap = NULL,
    # The y-coordinates for the points of the line.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    y = NULL,
    # The line dash values for the line.
    # > DashPattern
    line_dash = NULL,
    # The line color values for the line.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    line_color = NULL,
    # The line alpha values for the line.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_alpha = NULL,
    # The line dash offset values for the line.
    # > Int
    line_dash_offset = NULL
  )
)

# A base class for non-categorical ticker types.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
ContinuousTicker <- R6::R6Class("ContinuousTicker",
  inherit = Ticker,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      name = NULL, num_minor_ticks = 5L, tags = list(),
      subscribed_events = list(), desired_num_ticks = 6L,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["ContinuousTicker"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The number of minor tick positions to generate between adjacent major
    # tick values.
    # > Int
    num_minor_ticks = NULL,
    # A desired target number of major tick positions to generate across the
    # plot range.
    # 
    # .. note: This value is a suggestion, and ticker subclasses may ignore
    # it entirely, or use it only as an ideal goal to approach as well as can
    # be, in the context of a specific ticking strategy.
    # > Int
    desired_num_ticks = NULL
  )
)

# Generate ticks spaced apart even numbers of years.
YearsTicker <- R6::R6Class("YearsTicker",
  inherit = SingleIntervalTicker,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      num_minor_ticks = 5L, desired_num_ticks = 6L, tags = list(),
      subscribed_events = list(), interval = NULL, name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        num_minor_ticks = num_minor_ticks,
        desired_num_ticks = desired_num_ticks, tags = tags,
        subscribed_events = subscribed_events, interval = interval,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["YearsTicker"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Render annular wedges.
# 
# Example -------
# 
# .. bokeh-plot:: ../examples/reference/models/AnnularWedge.py
# :source-position: below
AnnularWedge <- R6::R6Class("AnnularWedge",
  inherit = XYGlyph,
  public = list(
    specified_args = NULL,
    initialize = function(
      line_cap = "butt", fill_alpha = 1, start_angle_units = "rad",
      inner_radius = NULL, start_angle = NULL, line_alpha = 1,
      subscribed_events = list(), line_dash_offset = 0L,
      inner_radius_units = "data", js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, end_angle_units = "rad",
      line_join = "miter", x = NULL, outer_radius = NULL, line_width = 1L,
      y = NULL, end_angle = NULL, line_dash = list(), fill_color = "gray",
      line_color = "black", tags = list(), outer_radius_units = "data",
      direction = "anticlock", id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["AnnularWedge"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The line cap values for the annular wedges.
    # > Enum('butt', 'round', 'square')
    line_cap = NULL,
    # The fill alpha values for the annular wedges.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    fill_alpha = NULL,
    # 
    # > Enum('deg', 'rad')
    start_angle_units = NULL,
    # The inner radii of the annular wedges.
    # > DistanceSpec(units_default='data')
    inner_radius = NULL,
    # The angles to start the annular wedges, as measured from the
    # horizontal.
    # > AngleSpec(units_default='rad')
    start_angle = NULL,
    # The line alpha values for the annular wedges.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_alpha = NULL,
    # The line dash offset values for the annular wedges.
    # > Int
    line_dash_offset = NULL,
    # 
    # > Enum('screen', 'data')
    inner_radius_units = NULL,
    # 
    # > Enum('deg', 'rad')
    end_angle_units = NULL,
    # The line join values for the annular wedges.
    # > Enum('miter', 'round', 'bevel')
    line_join = NULL,
    # The x-coordinates of the center of the annular wedges.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    x = NULL,
    # The outer radii of the annular wedges.
    # > DistanceSpec(units_default='data')
    outer_radius = NULL,
    # The line width values for the annular wedges.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_width = NULL,
    # The y-coordinates of the center of the annular wedges.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    y = NULL,
    # The angles to end the annular wedges, as measured from the horizontal.
    # > AngleSpec(units_default='rad')
    end_angle = NULL,
    # The line dash values for the annular wedges.
    # > DashPattern
    line_dash = NULL,
    # The fill color values for the annular wedges.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    fill_color = NULL,
    # The line color values for the annular wedges.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    line_color = NULL,
    # 
    # > Enum('screen', 'data')
    outer_radius_units = NULL,
    # Which direction to stroke between the start and end angles.
    # > Enum('clock', 'anticlock')
    direction = NULL
  )
)

# Slider-based number selection widget.
Slider <- R6::R6Class("Slider",
  inherit = AbstractSlider,
  public = list(
    specified_args = NULL,
    initialize = function(
      end = NULL, sizing_mode = "fixed", title = "",
      subscribed_events = list(), callback_policy = "throttle",
      css_classes = NULL, start = NULL, show_value = TRUE,
      bar_color = "#e6e6e6", value = NULL,
      js_event_callbacks = structure(list(), .Names = character(0)),
      orientation = "horizontal", js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, width = NULL,
      callback_throttle = 200L, height = NULL, disabled = FALSE,
      callback = NULL, tags = list(), step = 1L, format = "0[.]00",
      tooltips = TRUE, direction = "ltr", id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        orientation = orientation, width = width, tooltips = tooltips,
        callback_throttle = callback_throttle, height = height,
        disabled = disabled, callback = callback, css_classes = css_classes,
        sizing_mode = sizing_mode, name = name, tags = tags, title = title,
        format = format, subscribed_events = subscribed_events,
        callback_policy = callback_policy, show_value = show_value,
        bar_color = bar_color, direction = direction,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["Slider"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The maximum allowable value.
    # > Float
    end = NULL,
    # The minimum allowable value.
    # > Float
    start = NULL,
    # Initial or selected value.
    # > Float
    value = NULL,
    # The step between consecutive values.
    # > Float
    step = NULL
  )
)

# *toolbar icon*: |undo_icon|
# 
# Undo tool allows to restore previous state of the plot.
# 
# .. |undo_icon| image:: /_images/icons/Undo.png :height: 18pt
UndoTool <- R6::R6Class("UndoTool",
  inherit = Action,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["UndoTool"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Lay out child components in a single horizontal row.
# 
# Children can be specified as positional arguments, as a single argument
# that is a sequence, or using the ``children`` keyword argument.
Row <- R6::R6Class("Row",
  inherit = Box,
  public = list(
    specified_args = NULL,
    initialize = function(
      children = list(), js_event_callbacks = structure(list(), .Names =
      character(0)), width = NULL, height = NULL, disabled = FALSE,
      sizing_mode = "fixed", tags = list(), subscribed_events = list(),
      css_classes = NULL, name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(children = children,
        js_event_callbacks = js_event_callbacks, width = width,
        height = height, disabled = disabled, sizing_mode = sizing_mode,
        tags = tags, subscribed_events = subscribed_events,
        css_classes = css_classes, name = name,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["Row"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# The BBoxTileSource has the same default tile origin as the
# WMTSTileSource but requested tiles use a ``{XMIN}``, ``{YMIN}``,
# ``{XMAX}``, ``{YMAX}`` e.g.
# ``http://your.custom.tile.service?bbox={XMIN},{YMIN},{XMAX},{YMAX}``.
BBoxTileSource <- R6::R6Class("BBoxTileSource",
  inherit = MercatorTileSource,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      extra_url_vars = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      use_latlon = FALSE, max_zoom = 30L, min_zoom = 0L, tile_size = 256L,
      x_origin_offset = 20037508.34, y_origin_offset = 20037508.34,
      tags = list(), attribution = "", url = "", subscribed_events = list(),
      initial_resolution = 156543.033928041, name = NULL, wrap_around = TRUE,
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        extra_url_vars = extra_url_vars,
        js_property_callbacks = js_property_callbacks, max_zoom = max_zoom,
        min_zoom = min_zoom, tile_size = tile_size,
        x_origin_offset = x_origin_offset,
        y_origin_offset = y_origin_offset, tags = tags,
        attribution = attribution, url = url,
        subscribed_events = subscribed_events,
        initial_resolution = initial_resolution, name = name,
        wrap_around = wrap_around, id = id)
      types <- bk_prop_types[["BBoxTileSource"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Flag which indicates option to output {XMIN},{YMIN},{XMAX},{YMAX} in
    # meters or latitude and longitude.
    # > Bool
    use_latlon = NULL
  )
)

# *toolbar icon*: |crosshair_icon|
# 
# The hover tool is a passive inspector tool. It is generally on at all
# times, but can be configured in the inspector's menu associated with
# the *toolbar icon* shown above.
# 
# By default, the hover tool displays informational tooltips whenever the
# cursor is directly over a glyph. The data to show comes from the
# glyph's data source, and what is to be displayed is configurable with
# the ``tooltips`` attribute that maps display names to columns in the
# data source, or to special known variables.
# 
# Here is an example of how to configure and use the hover tool::
# 
# # Add tooltip (name, field) pairs to the tool. See below for a #
# description of possible field values.  hover.tooltips = [ ("index",
# "$index"), ("(x,y)", "($x, $y)"), ("radius", "@radius"), ("fill color",
# "$color[hex, swatch]:fill_color"), ("foo", "@foo"), ("bar", "@bar"),
# ("baz", "@baz{safe}"), ("total", "@total{$0,0.00}" ]
# 
# You can also supply a ``Callback`` to the HoverTool, to build custom
# interactions on hover. In this case you may want to turn the tooltips
# off by setting ``tooltips=None``.
# 
# .. warning::
# 
# Hover tool does not currently work with the following glyphs:
# 
# .. hlist:: :columns: 3
# 
# * annulus * arc * bezier * image * image_rgba * image_url * oval *
# patch * quadratic * ray * text
# 
# .. |hover_icon| image:: /_images/icons/Hover.png :height: 18pt
HoverTool <- R6::R6Class("HoverTool",
  inherit = Inspection,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      formatters = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      mode = "mouse", renderers = list(), callback = NULL,
      line_policy = "nearest", attachment = "horizontal", name = NULL,
      toggleable = TRUE, tags = list(), subscribed_events = list(),
      tooltips = list(list("index", "$index"), list("data (x, y)", "($x,
      $y)"), list("canvas (x, y)", "($sx, $sy)")), names = list(),
      show_arrow = TRUE, point_policy = "snap_to_data", anchor = "center",
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        toggleable = toggleable, tags = tags,
        subscribed_events = subscribed_events, name = name,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["HoverTool"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Specify the formatting scheme for data source columns, e.g.
    # 
    # .. code-block:: python
    # 
    # tool.formatters = dict(date="datetime")
    # 
    # will cause format specifications for the "date" column to be
    # interpreted according to the "datetime" formatting scheme. The
    # following schemed are available:
    # 
    # :``"numeral"``: Provides a wide variety of formats for numbers,
    # currency, bytes, times, and percentages. The full set of formats can be
    # found in the |NumeralTickFormatter| reference documentation.
    # 
    # :``"datetime"``: Provides formats for date and time values. The full
    # set of formats is listed in the |DatetimeTickFormatter| reference
    # documentation.
    # 
    # :``"printf"``: Provides formats similar to C-style "printf" type
    # specifiers. See the |PrintfTickFormatter| reference documentation for
    # complete details.
    # 
    # If no formatter is specified for a column name, the default
    # ``"numeral"`` formatter is assumed.
    # 
    # .. |NumeralTickFormatter| replace::
    # :class:`~bokeh.models.formatters.NumeralTickFormatter` ..
    # |DatetimeTickFormatter| replace::
    # :class:`~bokeh.models.formatters.DatetimeTickFormatter` ..
    # |PrintfTickFormatter| replace::
    # :class:`~bokeh.models.formatters.PrintfTickFormatter`
    # > Dict(String, Enum('numeral', 'datetime', 'printf'))
    formatters = NULL,
    # Whether to consider hover pointer as a point (x/y values), or a span on
    # h or v directions.
    # > Enum('mouse', 'hline', 'vline')
    mode = NULL,
    # An explicit list of renderers to hit test again. If unset, defaults to
    # all renderers on a plot.
    # > List(Instance(Renderer))
    renderers = NULL,
    # A callback to run in the browser whenever the input's value changes.
    # The cb_data parameter that is available to the Callback code will
    # contain two HoverTool specific fields:
    # 
    # :index: object containing the indices of the hovered points in the data
    # source :geometry: object containing the coordinates of the hover cursor
    # > Instance(Callback)
    callback = NULL,
    # When showing tooltips for lines, designates whether the tooltip
    # position should be the "previous" or "next" points on the line, the
    # "nearest" point to the current mouse position, or "interpolate" along
    # the line to the current mouse position.
    # > Enum('prev', 'next', 'nearest', 'interp', 'none')
    line_policy = NULL,
    # Whether tooltip's arrow should appear in the horizontal or vertical
    # dimension.
    # > Enum('horizontal', 'vertical')
    attachment = NULL,
    # The (name, field) pairs describing what the hover tool should display
    # when there is a hit.
    # 
    # Field names starting with "@" are interpreted as columns on the data
    # source. For instance, "@temp" would look up values to display from the
    # "temp" column of the data source.
    # 
    # Field names starting with "$" are special, known fields:
    # 
    # :$index: index of selected point in the data source :$x: x-coordinate
    # under the cursor in data space :$y: y-coordinate under the cursor in
    # data space :$sx: x-coordinate under the cursor in screen (canvas) space
    # :$sy: y-coordinate under the cursor in screen (canvas) space :$color:
    # color data from data source, with the syntax:
    # ``$color[options]:field_name``. The available options are: 'hex' (to
    # display the color as a hex value), and 'swatch' to also display a small
    # color swatch.
    # 
    # Field names that begin with ``@`` are associated with columns in a
    # ``ColumnDataSource``. For instance the field name ``"@price"`` will
    # display values from the ``"price"`` column whenever a hover is
    # triggered.  If the hover is for the 17th glyph, then the hover tooltip
    # will correspondingly display the 17th price value.
    # 
    # Note that if a column name contains spaces, the it must be supplied by
    # surrounding it in curly braces, e.g. ``@{adjusted close}`` will display
    # values from a column named ``"adjusted close"``.
    # 
    # By default, values for fields (e.g. ``@foo``) are displayed in a basic
    # numeric format. However it is possible to control the formatting of
    # values more precisely. Fields can be modified by appending a format
    # specified to the end in curly braces. Some examples are below.
    # 
    # .. code-block:: python
    # 
    # "@foo{0,0.000}" # formats 10000.1234 as: 10,000.123
    # 
    # "@foo{(.00)}" # formats -10000.1234 as: (10000.123)
    # 
    # "@foo{($ 0.00 a)}" # formats 1230974 as: $ 1.23 m
    # 
    # Specifying a format ``{safe}`` after a field name will override
    # automatic escaping of the tooltip data source. Any HTML tags in the
    # data tags will be rendered as HTML in the resulting HoverTool output.
    # See :ref:`custom_hover_tooltip` for a more detailed example.
    # 
    # ``None`` is also a valid value for tooltips. This turns off the
    # rendering of tooltips. This is mostly useful when supplying other
    # actions on hover via the callback property.
    # 
    # .. note:: The tooltips attribute can also be configured with a mapping
    # type, e.g. ``dict`` or ``OrderedDict``. However, if a ``dict`` is used,
    # the visual presentation order is unspecified.
    # > Either(String, List(Tuple(String, String)))
    tooltips = NULL,
    # A list of names to query for. If set, only renderers that have a
    # matching value for their ``name`` attribute will be used.
    # > List(String)
    names = NULL,
    # Whether tooltip's arrow should be showed.
    # > Bool
    show_arrow = NULL,
    # Whether the tooltip position should snap to the "center" (or other
    # anchor) position of the associated glyph, or always follow the current
    # mouse cursor position.
    # > Enum('snap_to_data', 'follow_mouse', 'none')
    point_policy = NULL,
    # If point policy is set to `"snap_to_data"`, `anchor` defines the
    # attachment point of a tooltip. The default is to attach to the center
    # of a glyph.
    # > Enum('top_left', 'top_center', 'top_right', 'center_left', 'center', 'center_right', 'bottom_left', 'bottom_center', 'bottom_right')
    anchor = NULL
  )
)

# A base class for tools that respond to scroll events.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
Scroll <- R6::R6Class("Scroll",
  inherit = Tool,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Scroll"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Display horizontal or vertical grid lines at locations given by a
# supplied ``Ticker``.
Grid <- R6::R6Class("Grid",
  inherit = GuideRenderer,
  public = list(
    specified_args = NULL,
    initialize = function(
      grid_line_cap = "butt", minor_grid_line_cap = "butt",
      minor_grid_line_dash = list(), minor_grid_line_width = 1L,
      minor_grid_line_dash_offset = 0L, grid_line_alpha = 1,
      minor_grid_line_color = NULL, x_range_name = "default",
      band_fill_alpha = 0L, bounds = "auto", y_range_name = "default",
      ticker = NULL, grid_line_dash_offset = 0L, subscribed_events = list(),
      grid_line_color = "#e5e5e5", dimension = 0L, minor_grid_line_alpha = 1,
      js_property_callbacks = structure(list(), .Names = character(0)),
      js_event_callbacks = structure(list(), .Names = character(0)),
      grid_line_join = "miter", name = NULL, visible = TRUE,
      grid_line_width = 1L, grid_line_dash = list(),
      minor_grid_line_join = "miter", band_fill_color = NULL, plot = NULL,
      tags = list(), level = "underlay", id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        visible = visible, name = name, plot = plot, tags = tags,
        subscribed_events = subscribed_events, level = level,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["Grid"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The line cap of the Grid lines.
    # > Enum('butt', 'round', 'square')
    grid_line_cap = NULL,
    # The line cap of the minor Grid lines.
    # > Enum('butt', 'round', 'square')
    minor_grid_line_cap = NULL,
    # The line dash of the minor Grid lines.
    # > DashPattern
    minor_grid_line_dash = NULL,
    # The line width of the minor Grid lines.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    minor_grid_line_width = NULL,
    # The line dash offset of the minor Grid lines.
    # > Int
    minor_grid_line_dash_offset = NULL,
    # The line alpha of the Grid lines.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    grid_line_alpha = NULL,
    # The line color of the minor Grid lines.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    minor_grid_line_color = NULL,
    # A particular (named) x-range to use for computing screen locations when
    # rendering a grid on the plot. If unset, use the default x-range.
    # > String
    x_range_name = NULL,
    # The fill alpha of alternating bands between Grid lines.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    band_fill_alpha = NULL,
    # Bounds for the rendered grid lines. If unset, the grid lines will span
    # the entire plot in the given dimension.
    # > Either(Auto, Tuple(Float, Float))
    bounds = NULL,
    # A particular (named) y-range to use for computing screen locations when
    # rendering a grid on the plot. If unset, use the default y-range.
    # > String
    y_range_name = NULL,
    # The Ticker to use for computing locations for the Grid lines.
    # > Instance(Ticker)
    ticker = NULL,
    # The line dash offset of the Grid lines.
    # > Int
    grid_line_dash_offset = NULL,
    # The line color of the Grid lines.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    grid_line_color = NULL,
    # Which dimension the Axis Grid lines will intersect. The x-axis is
    # dimension 0 (vertical Grid lines) and the y-axis is dimension 1
    # (horizontal Grid lines).
    # > Int
    dimension = NULL,
    # The line alpha of the minor Grid lines.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    minor_grid_line_alpha = NULL,
    # The line join of the Grid lines.
    # > Enum('miter', 'round', 'bevel')
    grid_line_join = NULL,
    # The line width of the Grid lines.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    grid_line_width = NULL,
    # The line dash of the Grid lines.
    # > DashPattern
    grid_line_dash = NULL,
    # The line join of the minor Grid lines.
    # > Enum('miter', 'round', 'bevel')
    minor_grid_line_join = NULL,
    # The fill color of alternating bands between Grid lines.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    band_fill_color = NULL
  )
)

# An IndexFilter filters data by returning the subset of data at a given
# set of indices.
IndexFilter <- R6::R6Class("IndexFilter",
  inherit = Filter,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      tags = list(), filter = NULL, indices = NULL, subscribed_events = list(),
      name = NULL, js_property_callbacks = structure(list(), .Names =
      character(0)), id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        filter = filter, tags = tags, subscribed_events = subscribed_events,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["IndexFilter"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # A list of integer indices representing the subset of data to select.
    # > Seq(Int)
    indices = NULL
  )
)

# A base class for all tile source types.
# 
# In general, tile sources are used as a required input for
# ``TileRenderer``.
TileSource <- R6::R6Class("TileSource",
  inherit = Model,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      extra_url_vars = structure(list(), .Names = character(0)),
      max_zoom = 30L, min_zoom = 0L, tile_size = 256L, x_origin_offset = NULL,
      y_origin_offset = NULL, tags = list(), attribution = "", url = "",
      subscribed_events = list(), initial_resolution = NULL, name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["TileSource"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # A dictionary that maps url variable template keys to values.
    # 
    # These variables are useful for parts of tile urls which do not change
    # from tile to tile (e.g. server host name, or layer name).
    # > Dict(String, Any)
    extra_url_vars = NULL,
    # A maximum zoom level for the tile layer. This is the most zoomed-in
    # level.
    # > Int
    max_zoom = NULL,
    # A minimum zoom level for the tile layer. This is the most zoomed-out
    # level.
    # > Int
    min_zoom = NULL,
    # Tile size in pixels (e.g. 256)
    # > Int
    tile_size = NULL,
    # An x-offset in plot coordinates
    # > Float
    x_origin_offset = NULL,
    # A y-offset in plot coordinates
    # > Float
    y_origin_offset = NULL,
    # Data provider attribution content. This can include HTML content.
    # > String
    attribution = NULL,
    # Tile service url e.g., http://c.tile.openstreetmap.org/{Z}/{X}/{Y}.png
    # > String
    url = NULL,
    # Resolution (plot_units / pixels) of minimum zoom level of tileset
    # projection. None to auto-compute.
    # > Float
    initial_resolution = NULL
  )
)

# A base class for all data range types.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
DataRange <- R6::R6Class("DataRange",
  inherit = Range,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      renderers = list(), callback = NULL, tags = list(),
      subscribed_events = list(), names = list(), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(callback = callback,
        js_event_callbacks = js_event_callbacks, tags = tags,
        subscribed_events = subscribed_events, name = name,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["DataRange"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # An explicit list of renderers to autorange against. If unset, defaults
    # to all renderers on a plot.
    # > List(Instance(Renderer))
    renderers = NULL,
    # A list of names to query for. If set, only renderers that have a
    # matching value for their ``name`` attribute will be used for
    # autoranging.
    # > List(String)
    names = NULL
  )
)

# Generate ticks at fixed, explicitly supplied locations.
# 
# .. note:: The ``desired_num_ticks`` property is ignored by this Ticker.
FixedTicker <- R6::R6Class("FixedTicker",
  inherit = ContinuousTicker,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      num_minor_ticks = 5L, desired_num_ticks = 6L, ticks = list(),
      tags = list(), subscribed_events = list(), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        name = name, num_minor_ticks = num_minor_ticks, tags = tags,
        subscribed_events = subscribed_events,
        desired_num_ticks = desired_num_ticks,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["FixedTicker"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # List of tick locations.
    # > Seq(Float)
    ticks = NULL
  )
)

# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
GraphHitTestPolicy <- R6::R6Class("GraphHitTestPolicy",
  inherit = Model,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["GraphHitTestPolicy"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Range-slider based number range selection widget.
RangeSlider <- R6::R6Class("RangeSlider",
  inherit = AbstractSlider,
  public = list(
    specified_args = NULL,
    initialize = function(
      end = NULL, sizing_mode = "fixed", title = "",
      subscribed_events = list(), callback_policy = "throttle",
      css_classes = NULL, start = NULL, show_value = TRUE,
      bar_color = "#e6e6e6", value = NULL,
      js_event_callbacks = structure(list(), .Names = character(0)),
      orientation = "horizontal", js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, width = NULL,
      callback_throttle = 200L, height = NULL, disabled = FALSE,
      callback = NULL, tags = list(), step = 1L, format = "0[.]00",
      tooltips = TRUE, direction = "ltr", id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        orientation = orientation, width = width, tooltips = tooltips,
        callback_throttle = callback_throttle, height = height,
        disabled = disabled, callback = callback, css_classes = css_classes,
        sizing_mode = sizing_mode, name = name, tags = tags, title = title,
        format = format, subscribed_events = subscribed_events,
        callback_policy = callback_policy, show_value = show_value,
        bar_color = bar_color, direction = direction,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["RangeSlider"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The maximum allowable value.
    # > Float
    end = NULL,
    # The minimum allowable value.
    # > Float
    start = NULL,
    # Initial or selected range.
    # > Tuple(Float, Float)
    value = NULL,
    # The step between consecutive values.
    # > Float
    step = NULL
  )
)

# Tick formatter based on a printf-style format string.
PrintfTickFormatter <- R6::R6Class("PrintfTickFormatter",
  inherit = TickFormatter,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      tags = list(), format = "%s", subscribed_events = list(), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["PrintfTickFormatter"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The number format, as defined as follows: the placeholder in the format
    # string is marked by % and is followed by one or more of these elements,
    # in this order:
    # 
    # * An optional ``+`` sign Causes the result to be preceded with a plus
    # or minus sign on numeric values. By default, only the ``-`` sign is
    # used on negative numbers.
    # 
    # * An optional padding specifier Specifies what (if any) character to
    # use for padding. Possible values are 0 or any other character preceded
    # by a ``'`` (single quote). The default is to pad with spaces.
    # 
    # * An optional ``-`` sign Causes sprintf to left-align the result of
    # this placeholder. The default is to right-align the result.
    # 
    # * An optional number Specifies how many characters the result should
    # have. If the value to be returned is shorter than this number, the
    # result will be padded.
    # 
    # * An optional precision modifier Consists of a ``.`` (dot) followed by
    # a number, specifies how many digits should be displayed for floating
    # point numbers. When used on a string, it causes the result to be
    # truncated.
    # 
    # * A type specifier Can be any of:
    # 
    # - ``%`` --- yields a literal ``%`` character - ``b`` --- yields an
    # integer as a binary number - ``c`` --- yields an integer as the
    # character with that ASCII value - ``d`` or ``i`` --- yields an integer
    # as a signed decimal number - ``e`` --- yields a float using scientific
    # notation - ``u`` --- yields an integer as an unsigned decimal number -
    # ``f`` --- yields a float as is - ``o`` --- yields an integer as an
    # octal number - ``s`` --- yields a string as is - ``x`` --- yields an
    # integer as a hexadecimal number (lower-case) - ``X`` --- yields an
    # integer as a hexadecimal number (upper-case)
    # > String
    format = NULL
  )
)

# *toolbar icon*: |tap_icon|
# 
# The tap selection tool allows the user to select at single points by
# left-clicking a mouse, or tapping with a finger.
# 
# See :ref:`userguide_styling_selected_unselected_glyphs` for information
# on styling selected and unselected glyphs.
# 
# .. |tap_icon| image:: /_images/icons/Tap.png :height: 18pt
# 
# .. note:: Selections can be comprised of multiple regions, even those
# made by different selection tools. Hold down the <<shift>> key while
# making a selection to append the new selection to any previous
# selection that might exist.
TapTool <- R6::R6Class("TapTool",
  inherit = Tap,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      renderers = list(), callback = NULL, tags = list(), behavior = "select",
      subscribed_events = list(), names = list(), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["TapTool"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # An explicit list of renderers to hit test again. If unset, defaults to
    # all renderers on a plot.
    # > List(Instance(Renderer))
    renderers = NULL,
    # A callback to execute *whenever a glyph is "hit"* by a mouse click or
    # tap.
    # 
    # This is often useful with the :class:`~bokeh.models.callbacks.OpenURL`
    # model to open URLs based on a user clicking or tapping a specific
    # glyph.
    # 
    # However, it may also be a :class:`~bokeh.models.callbacks.CustomJS`
    # which can execute arbitrary JavaScript code in response to clicking or
    # tapping glyphs. The callback will be executed for each individual glyph
    # that is it hit by a click or tap, and will receive the ``TapTool``
    # model as ``cb_obj``. The optional ``cb_data`` will have the data source
    # as its ``.source`` attribute and the selection geometry as its
    # ``.geometries`` attribute.
    # 
    # .. note:: This callback does *not* execute on every tap, only when a
    # glyphs is "hit". If you would like to execute a callback on every mouse
    # tap, please see
    # :ref:`userguide_interaction_jscallbacks_customjs_interactions`.
    # > Instance(Callback)
    callback = NULL,
    # This tool can be configured to either make selections or inspections on
    # associated data sources. The difference is that selection changes
    # propagate across bokeh and other components (e.g. selection glyph) will
    # be notified. Inspecions don't act like this, so it's useful to
    # configure `callback` when setting `behavior='inspect'`.
    # > Enum('select', 'inspect')
    behavior = NULL,
    # A list of names to query for. If set, only renderers that have a
    # matching value for their ``name`` attribute will be used.
    # > List(String)
    names = NULL
  )
)

# Options for GMapPlot objects.
GMapOptions <- R6::R6Class("GMapOptions",
  inherit = MapOptions,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      lat = NULL, zoom = 12L, scale_control = FALSE, map_type = "roadmap",
      styles = NULL, tags = list(), subscribed_events = list(), lng = NULL,
      name = NULL, js_property_callbacks = structure(list(), .Names =
      character(0)), id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks, lat = lat,
        zoom = zoom, tags = tags, subscribed_events = subscribed_events,
        lng = lng, name = name,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["GMapOptions"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Whether the Google map should display its distance scale control.
    # > Bool
    scale_control = NULL,
    # The `map type`_ to use for the GMapPlot.
    # 
    # .. _map type:
    # https://developers.google.com/maps/documentation/javascript/reference#MapTypeId
    # > Enum('satellite', 'roadmap', 'terrain', 'hybrid')
    map_type = NULL,
    # A JSON array of `map styles`_ to use for the GMapPlot. Many example
    # styles can `be found here`_.
    # 
    # .. _map styles:
    # https://developers.google.com/maps/documentation/javascript/reference#MapTypeStyle
    # .. _be found here: https://snazzymaps.com
    # > JSON
    styles = NULL
  )
)

# An abstract base class for layout components.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
LayoutDOM <- R6::R6Class("LayoutDOM",
  inherit = Model,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      width = NULL, height = NULL, disabled = FALSE, sizing_mode = "fixed",
      tags = list(), subscribed_events = list(), css_classes = NULL,
      name = NULL, js_property_callbacks = structure(list(), .Names =
      character(0)), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["LayoutDOM"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # An optional width for the component (in pixels).
    # > Int
    width = NULL,
    # An optional height for the component (in pixels).
    # > Int
    height = NULL,
    # Whether the widget will be disabled when rendered. If ``True``, the
    # widget will be greyed-out, and not respond to UI events.
    # > Bool
    disabled = NULL,
    # How the item being displayed should size itself. Possible values are
    # ``"fixed"``, ``"scale_width"``, ``"scale_height"``, ``"scale_both"``,
    # and ``"stretch_both"``.
    # 
    # ``"stretch_both"`` elements are completely responsive (independently in
    # width and height) and will resize to occupy all available space, even
    # if this changes the aspect ratio of the element.  This is sometimes
    # called outside-in, and is a typical behavior for desktop applications.
    # 
    # ``"fixed"`` elements are not responsive. They will retain their
    # original width and height regardless of any subsequent browser window
    # resize events.
    # 
    # ``"scale_width"`` elements will responsively resize to fit to the width
    # available, *while maintaining the original aspect ratio*. This is a
    # typical behavior for modern websites. For a ``Plot``, the aspect ratio
    # ``plot_width/plot_height`` is maintained.
    # 
    # ``"scale_height"`` elements will responsively resize to fit to the
    # height available, *while maintaining the original aspect ratio*. For a
    # ``Plot``, the aspect ratio ``plot_width/plot_height`` is maintained. A
    # plot with ``"scale_height"`` mode needs to be wrapped in a ``Row`` or
    # ``Column`` to be responsive.
    # 
    # ``"scale_both"`` elements will responsively resize to for both the
    # width and height available, *while maintaining the original aspect
    # ratio*.
    # > Enum('stretch_both', 'scale_width', 'scale_height', 'scale_both', 'fixed')
    sizing_mode = NULL,
    # A list of css class names to add to this DOM element. Note: the class
    # names are simply added as-is, no other guarantees are provided.
    # > Seq(String)
    css_classes = NULL
  )
)

# ``IntEditor`` optimized for editing percentages.
PercentEditor <- R6::R6Class("PercentEditor",
  inherit = CellEditor,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["PercentEditor"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Render a tee-style arrow head.
TeeHead <- R6::R6Class("TeeHead",
  inherit = ArrowHead,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      visible = TRUE, line_join = "miter", size = 25L, line_width = 1L,
      line_cap = "butt", name = NULL, line_dash = list(), plot = NULL,
      line_color = "black", tags = list(), subscribed_events = list(),
      line_alpha = 1, level = "annotation", line_dash_offset = 0L, id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        visible = visible, name = name, plot = plot, tags = tags,
        subscribed_events = subscribed_events, level = level,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["TeeHead"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The line join values for the arrow head outline.
    # > Enum('miter', 'round', 'bevel')
    line_join = NULL,
    # The size, in pixels, of the arrow head.
    # > Float
    size = NULL,
    # The line width values for the arrow head outline.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_width = NULL,
    # The line cap values for the arrow head outline.
    # > Enum('butt', 'round', 'square')
    line_cap = NULL,
    # The line dash values for the arrow head outline.
    # > DashPattern
    line_dash = NULL,
    # The line color values for the arrow head outline.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    line_color = NULL,
    # The line alpha values for the arrow head outline.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_alpha = NULL,
    # The line dash offset values for the arrow head outline.
    # > Int
    line_dash_offset = NULL
  )
)

# Render axis-aligned quads.
# 
# Example -------
# 
# .. bokeh-plot:: ../examples/reference/models/Quad.py :source-position:
# below
Quad <- R6::R6Class("Quad",
  inherit = Glyph,
  public = list(
    specified_args = NULL,
    initialize = function(
      right = NULL, js_event_callbacks = structure(list(), .Names =
      character(0)), js_property_callbacks = structure(list(), .Names =
      character(0)), line_join = "miter", line_width = 1L, line_cap = "butt",
      fill_alpha = 1, bottom = NULL, tags = list(), line_dash = list(),
      top = NULL, left = NULL, fill_color = "gray", line_color = "black",
      subscribed_events = list(), line_alpha = 1, name = NULL,
      line_dash_offset = 0L, id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Quad"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The x-coordinates of the right edges.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    right = NULL,
    # The line join values for the quads.
    # > Enum('miter', 'round', 'bevel')
    line_join = NULL,
    # The line width values for the quads.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_width = NULL,
    # The line cap values for the quads.
    # > Enum('butt', 'round', 'square')
    line_cap = NULL,
    # The fill alpha values for the quads.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    fill_alpha = NULL,
    # The y-coordinates of the bottom edges.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    bottom = NULL,
    # The line dash values for the quads.
    # > DashPattern
    line_dash = NULL,
    # The y-coordinates of the top edges.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    top = NULL,
    # The x-coordinates of the left edges.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    left = NULL,
    # The fill color values for the quads.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    fill_color = NULL,
    # The line color values for the quads.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    line_color = NULL,
    # The line alpha values for the quads.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_alpha = NULL,
    # The line dash offset values for the quads.
    # > Int
    line_dash_offset = NULL
  )
)

# Abstract base class for Row and Column. Do not use directly.
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
Box <- R6::R6Class("Box",
  inherit = LayoutDOM,
  public = list(
    specified_args = NULL,
    initialize = function(
      children = list(), js_event_callbacks = structure(list(), .Names =
      character(0)), width = NULL, height = NULL, disabled = FALSE,
      sizing_mode = "fixed", tags = list(), subscribed_events = list(),
      css_classes = NULL, name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        width = width, height = height, disabled = disabled,
        sizing_mode = sizing_mode, tags = tags,
        subscribed_events = subscribed_events, css_classes = css_classes,
        name = name, js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["Box"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The list of children, which can be other components including plots,
    # rows, columns, and widgets.
    # > List(Instance(LayoutDOM))
    children = NULL
  )
)

# 
TileRenderer <- R6::R6Class("TileRenderer",
  inherit = DataRenderer,
  public = list(
    specified_args = NULL,
    initialize = function(
      render_parents = TRUE, js_event_callbacks = structure(list(), .Names
      = character(0)), alpha = 1, visible = TRUE, x_range_name = "default",
      tile_source = NULL, name = NULL, y_range_name = "default", tags = list(),
      subscribed_events = list(), level = "underlay",
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        name = name, visible = visible, tags = tags,
        subscribed_events = subscribed_events, level = level,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["TileRenderer"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # Flag enable/disable drawing of parent tiles while waiting for new tiles
    # to arrive. Default value is True.
    # > Bool
    render_parents = NULL,
    # tile opacity 0.0 - 1.0
    # > Float
    alpha = NULL,
    # A particular (named) x-range to use for computing screen locations when
    # rendering glyphs on the plot. If unset, use the default x-range.
    # > String
    x_range_name = NULL,
    # Local data source to use when rendering glyphs on the plot.
    # > Instance(TileSource)
    tile_source = NULL,
    # A particular (named) y-range to use for computing screen locations when
    # rendering glyphs on the plot. If unset, use the default y-range.
    # > String
    y_range_name = NULL
  )
)

# Render segments.
# 
# Example -------
# 
# .. bokeh-plot:: ../examples/reference/models/Segment.py
# :source-position: below
Segment <- R6::R6Class("Segment",
  inherit = Glyph,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      line_join = "miter", y0 = NULL, line_width = 1L, line_cap = "butt",
      line_dash = list(), x0 = NULL, line_color = "black", tags = list(),
      subscribed_events = list(), line_alpha = 1, line_dash_offset = 0L,
      y1 = NULL, name = NULL, x1 = NULL, id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Segment"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The line join values for the segments.
    # > Enum('miter', 'round', 'bevel')
    line_join = NULL,
    # The y-coordinates of the starting points.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    y0 = NULL,
    # The line width values for the segments.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_width = NULL,
    # The line cap values for the segments.
    # > Enum('butt', 'round', 'square')
    line_cap = NULL,
    # The line dash values for the segments.
    # > DashPattern
    line_dash = NULL,
    # The x-coordinates of the starting points.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    x0 = NULL,
    # The line color values for the segments.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    line_color = NULL,
    # The line alpha values for the segments.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_alpha = NULL,
    # The line dash offset values for the segments.
    # > Int
    line_dash_offset = NULL,
    # The y-coordinates of the ending points.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    y1 = NULL,
    # The x-coordinates of the ending points.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    x1 = NULL
  )
)

# A Filter model represents a filtering operation that returns a row-wise
# subset of data when applied to a ColumnDataSource.
Filter <- R6::R6Class("Filter",
  inherit = Model,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      filter = NULL, tags = list(), subscribed_events = list(), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Filter"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # A list that can be either integer indices or booleans representing a
    # row-wise subset of data.
    # > Either(Seq(Int), Seq(Bool))
    filter = NULL
  )
)

# Render annuli.
# 
# Example -------
# 
# .. bokeh-plot:: ../examples/reference/models/Annulus.py
# :source-position: below
Annulus <- R6::R6Class("Annulus",
  inherit = XYGlyph,
  public = list(
    specified_args = NULL,
    initialize = function(
      inner_radius_units = "data", js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), line_join = "miter", x = NULL,
      outer_radius = NULL, line_width = 1L, line_cap = "butt", y = NULL,
      fill_alpha = 1, tags = list(), line_dash = list(), inner_radius = NULL,
      line_color = "black", fill_color = "gray", outer_radius_units = "data",
      line_alpha = 1, subscribed_events = list(), name = NULL,
      line_dash_offset = 0L, id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Annulus"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # 
    # > Enum('screen', 'data')
    inner_radius_units = NULL,
    # The line join values for the annuli.
    # > Enum('miter', 'round', 'bevel')
    line_join = NULL,
    # The x-coordinates of the center of the annuli.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    x = NULL,
    # The outer radii of the annuli.
    # > DistanceSpec(units_default='data')
    outer_radius = NULL,
    # The line width values for the annuli.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_width = NULL,
    # The line cap values for the annuli.
    # > Enum('butt', 'round', 'square')
    line_cap = NULL,
    # The y-coordinates of the center of the annuli.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    y = NULL,
    # The fill alpha values for the annuli.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    fill_alpha = NULL,
    # The line dash values for the annuli.
    # > DashPattern
    line_dash = NULL,
    # The inner radii of the annuli.
    # > DistanceSpec(units_default='data')
    inner_radius = NULL,
    # The line color values for the annuli.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    line_color = NULL,
    # The fill color values for the annuli.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    fill_color = NULL,
    # 
    # > Enum('screen', 'data')
    outer_radius_units = NULL,
    # The line alpha values for the annuli.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_alpha = NULL,
    # The line dash offset values for the annuli.
    # > Int
    line_dash_offset = NULL
  )
)

# Generate ticks on a linear scale.
# 
# .. note:: This class may be renamed to ``LinearTicker`` in the future.
BasicTicker <- R6::R6Class("BasicTicker",
  inherit = AdaptiveTicker,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      mantissas = list(1L, 2L, 5L), base = 10, num_minor_ticks = 5L,
      min_interval = 0, desired_num_ticks = 6L, max_interval = NULL,
      tags = list(), subscribed_events = list(), name = NULL,
      js_property_callbacks = structure(list(), .Names = character(0)),
      id = NULL
    ) {
      super$initialize(js_event_callbacks = js_event_callbacks,
        mantissas = mantissas, base = base,
        num_minor_ticks = num_minor_ticks, min_interval = min_interval,
        desired_num_ticks = desired_num_ticks, max_interval = max_interval,
        tags = tags, subscribed_events = subscribed_events, name = name,
        js_property_callbacks = js_property_callbacks, id = id)
      types <- bk_prop_types[["BasicTicker"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Render parabolas.
# 
# Example -------
# 
# .. bokeh-plot:: ../examples/reference/models/Quadratic.py
# :source-position: below
Quadratic <- R6::R6Class("Quadratic",
  inherit = Glyph,
  public = list(
    specified_args = NULL,
    initialize = function(
      js_event_callbacks = structure(list(), .Names = character(0)),
      js_property_callbacks = structure(list(), .Names = character(0)),
      line_join = "miter", cx = NULL, y0 = NULL, cy = NULL, line_width = 1L,
      line_cap = "butt", line_dash = list(), x0 = NULL, line_color = "black",
      tags = list(), subscribed_events = list(), line_alpha = 1,
      line_dash_offset = 0L, y1 = NULL, name = NULL, x1 = NULL, id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Quadratic"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # The line join values for the parabolas.
    # > Enum('miter', 'round', 'bevel')
    line_join = NULL,
    # The x-coordinates of the control points.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    cx = NULL,
    # The y-coordinates of the starting points.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    y0 = NULL,
    # The y-coordinates of the control points.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    cy = NULL,
    # The line width values for the parabolas.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_width = NULL,
    # The line cap values for the parabolas.
    # > Enum('butt', 'round', 'square')
    line_cap = NULL,
    # The line dash values for the parabolas.
    # > DashPattern
    line_dash = NULL,
    # The x-coordinates of the starting points.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    x0 = NULL,
    # The line color values for the parabolas.
    # > ColorSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Color)), Color)
    line_color = NULL,
    # The line alpha values for the parabolas.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    line_alpha = NULL,
    # The line dash offset values for the parabolas.
    # > Int
    line_dash_offset = NULL,
    # The y-coordinates of the ending points.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    y1 = NULL,
    # The x-coordinates of the ending points.
    # > NumberSpec(String, Dict(Enum('expr', 'field', 'value', 'transform'), Either(String, Instance(Transform), Instance(Expression), Float)), Float)
    x1 = NULL
  )
)

# Base class for ``Transform`` models that represent a computation to be
# carried out on the client-side.
# 
# JavaScript implementations should implement the following methods:
# 
# .. code-block: coffeescript
# 
# compute: (x) -> # compute the transform of a single value
# 
# v_compute: (xs) -> # compute the transform of an array of values
# 
# .. note:: This is an abstract base class used to help organize the
# hierarchy of Bokeh model types. **It is not useful to instantiate on
# its own.**
Transform <- R6::R6Class("Transform",
  inherit = Model,
  public = list(
    specified_args = NULL,
    initialize = function(
      subscribed_events = list(), js_event_callbacks = structure(list(),
      .Names = character(0)), js_property_callbacks = structure(list(),
      .Names = character(0)), name = NULL, tags = list(), id = NULL
    ) {
      super$initialize(subscribed_events = subscribed_events,
        js_event_callbacks = js_event_callbacks,
        js_property_callbacks = js_property_callbacks, name = name,
        tags = tags, id = id)
      types <- bk_prop_types[["Transform"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(

  )
)

# Base class
Base <- R6::R6Class("Base",
  public = list(
    specified_args = NULL,
    initialize = function(
      id = NULL
    ) {
      super$initialize()
      types <- bk_prop_types[["Base"]]
      for (nm in names(types)) {
        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)
      }
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(
    # id
    # > String
    id = NULL
  )
)

