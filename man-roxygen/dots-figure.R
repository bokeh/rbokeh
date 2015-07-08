#' @param \ldots additional parameters for fine control figure properties (see "Additional parameters" below)
#' @section Additional parameters:
#' \tabular{ll}{
#' \code{background_fill} \tab (color) background color of plot \cr
#' \code{border_fill} \tab (color) fill color of border area of plot \cr
#' \code{min_border} \tab (integer) A convenience property to set all all the min_X_border properties to the same value. If an individual border property is explicitly set, it will override min_border. \cr
#' \code{min_border_bottom} \tab (integer) Minimum size in pixels of the padding region below the bottom of the central plot region. This is a minimum. The padding region may expand as needed to accommodate titles or axes, etc. \cr
#' \code{min_border_left} \tab (integer) Minimum size in pixels of the padding region to the left of the central plot region. This is a minimum. The padding region may expand as needed to accommodate titles or axes, etc. \cr
#' \code{min_border_right} \tab (integer) Minimum size in pixels of the padding region to the right of the central plot region. This is a minimum. The padding region may expand as needed to accommodate titles or axes, etc. \cr
#' \code{min_border_top} \tab (integer) Minimum size in pixels of the padding region above the top of the central plot region. This is a minimum. The padding region may expand as needed to accommodate titles or axes, etc. \cr
#' \code{outline_line_alpha} \tab (numeric) The line alpha for the plot border outline. \cr
#' \code{outline_line_cap} \tab ('butt', 'round', 'square') The line cap for the plot border outline. \cr
#' \code{outline_line_color} \tab (color) The line color for the plot border outline. \cr
#' \code{outline_line_dash} \tab The line dash for the plot border outline. \cr
#' \code{outline_line_dash_offset} \tab (integer) The line dash offset for the plot border outline. \cr
#' \code{outline_line_join} \tab ('miter', 'round', 'bevel') The line join for the plot border outline. \cr
#' \code{outline_line_width} \tab (integer) The line width for the plot border outline. \cr
#' \code{plot_height} \tab (integer) Total height of the entire plot (including any axes, titles, border padding, etc.) This corresponds directly to the height of the HTML canvas that will be used. \cr
#' \code{plot_width} \tab (integer) Total width of the entire plot (including any axes, titles, border padding, etc.) This corresponds directly to the width of the HTML canvas that will be used. \cr
#' \code{title_text_align} \tab ('left', 'right', 'center') The text align for the plot title. \cr
#' \code{title_text_alpha} \tab The text alpha for the plot title. \cr
#' \code{title_text_baseline} \tab ('top', 'middle', 'bottom', 'alphabetic', 'hanging') The text baseline for the plot title. \cr
#' \code{title_text_color} \tab (color) The text color for the plot title. \cr
#' \code{title_text_font} \tab (string) The text font for the plot title. \cr
#' \code{title_text_font_size} \tab (string - e.g. '12pt') The text font size for the plot title. \cr
#' \code{title_text_font_style} \tab ('normal', 'italic', 'bold') The text font style for the plot title. \cr
#' \code{toolbar_location} \tab ('above', 'below', 'left', 'right') Where the toolbar will be located. If set to None, no toolbar will be attached to the plot. \cr
#' \code{h_symmetry} \tab (logical) Whether the total horizontal padding on both sides of the plot will be made equal (the left or right padding amount, whichever is larger). \cr
#' \code{v_symmetry} \tab (logical) Whether the total vertical padding on both sides of the plot will be made equal (the top or bottom padding amount, whichever is larger). \cr
#' \code{logo} \tab ('normal', 'grey') What version of the Bokeh logo to display on the toolbar. If set to None, no logo will be displayed. \cr
#' \code{lod_factor} \tab (integer) Decimation factor to use when applying level-of-detail decimation (see "Controlling level of detail"). \cr
#' \code{lod_interval} \tab (integer) Interval (in ms) during which an interactive tool event will enable level-of-detail downsampling (see "Controlling level of detail"). \cr
#' \code{lod_threshold} \tab (integer) A number of data points, above which level-of-detail downsampling may be performed by glyph renderers. Set to \code{NULL} to disable any level-of-detail downsampling (see "Controlling level of detail"). \cr
#' \code{lod_timeout} \tab (integer) Timeout (in ms) for checking whether interactive tool events are still occurring. Once level-of-detail mode is enabled, a check is made every lod_timeout ms. If no interactive tool events have happened, level-of-detail mode is disabled (see "Controlling level of detail").
#'  }
#' @section Controlling level of detail:
#' Although the HTML canvas can comfortably display tens or even hundreds of thousands of glyphs, doing so can have adverse affects on interactive performance. In order to accommodate large-ish (but not enormous) data sizes, Bokeh plots offer "Level of Detail" (LOD) capability in the client.
#'
#' The basic idea is that during interactive operations (e.g., panning or zooming), the plot only draws some small fraction data points. This hopefully allows the general sense of the interaction to be preserved mid-flight, while maintaining interactive performance. See the \code{lod_} parameters for information on how to control this.
