#' @param \ldots additional parameters for fine control figure properties (see "Additional parameters" below)
#' @section Additional parameters:
#' \tabular{ll}{
#' \code{background_fill} \tab (color) background color of plot \cr
#' \code{border_fill} \tab (color) fill color of border area of plot \cr
#' \code{min_border} \tab (int) A convenience property to set all all the min_X_border properties to the same value. If an individual border property is explicitly set, it will override min_border. \cr
#' \code{min_border_bottom} \tab (int) Minimum size in pixels of the padding region below the bottom of the central plot region. This is a minimum. The padding region may expand as needed to accommodate titles or axes, etc. \cr
#' \code{min_border_left} \tab (int) Minimum size in pixels of the padding region to the left of the central plot region. This is a minimum. The padding region may expand as needed to accommodate titles or axes, etc. \cr
#' \code{min_border_right} \tab (int) Minimum size in pixels of the padding region to the right of the central plot region. This is a minimum. The padding region may expand as needed to accommodate titles or axes, etc. \cr
#' \code{min_border_top} \tab (int) Minimum size in pixels of the padding region above the top of the central plot region. This is a minimum. The padding region may expand as needed to accommodate titles or axes, etc. \cr
#' \code{outline_line_alpha} \tab (dataspec) The line alpha for the plot border outline. \cr
#' \code{outline_line_cap} \tab ('butt', 'round', 'square') The line cap for the plot border outline. \cr
#' \code{outline_line_color} \tab (color) The line color for the plot border outline. \cr
#' \code{outline_line_dash} \tab The line dash for the plot border outline. \cr
#' \code{outline_line_dash_offset} \tab (int) The line dash offset for the plot border outline. \cr
#' \code{outline_line_join} \tab ('miter', 'round', 'bevel') The line join for the plot border outline. \cr
#' \code{outline_line_width} \tab (dataspec) The line width for the plot border outline. \cr
#' \code{plot_height} \tab (int) Total height of the entire plot (including any axes, titles, border padding, etc.) This corresponds directly to the height of the HTML canvas that will be used. \cr
#' \code{plot_width} \tab (int) Total width of the entire plot (including any axes, titles, border padding, etc.) This corresponds directly to the width of the HTML canvas that will be used. \cr
#' \code{title_text_align} \tab ('left', 'right', 'center') The text align for the plot title. \cr
#' \code{title_text_alpha} \tab The text alpha for the plot title. \cr
#' \code{title_text_baseline} \tab ('top', 'middle', 'bottom', 'alphabetic', 'hanging') The text baseline for the plot title. \cr
#' \code{title_text_color} \tab (color) The text color for the plot title. \cr
#' \code{title_text_font} \tab (string) The text font for the plot title. \cr
#' \code{title_text_font_size} \tab (string - '12pt') The text font size for the plot title. \cr
#' \code{title_text_font_style} \tab ('normal', 'italic', 'bold') The text font style for the plot title. \cr
#' \code{toolbar_location} \tab ('above', 'below', 'left', 'right') Where the toolbar will be located. If set to None, no toolbar will be attached to the plot. \cr
#' \code{h_symmetry} \tab (logical) Whether the total horizontal padding on both sides of the plot will be made equal (the left or right padding amount, whichever is larger). \cr
#' \code{v_symmetry} \tab (logical) Whether the total vertical padding on both sides of the plot will be made equal (the top or bottom padding amount, whichever is larger). \cr
#' \code{logo} \tab ('normal', 'grey') What version of the Bokeh logo to display on the toolbar. If set to None, no logo will be displayed. \cr
#'  }
