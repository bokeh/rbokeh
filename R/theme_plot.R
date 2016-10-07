#' Override theme parameters for general plot attributes
#'
#' @param fig figure to modify
#' @param background_fill_color (color) background color of plot
#' @param background_fill_alpha (numeric) background color alpha of plot
#' @param border_fill_color (color) fill color of border area of plot
#' @param border_fill_alpha (numeric) fill color alpha of border area of plot
#' @param outline_line_alpha (numeric) The line alpha for the plot border outline.
#' @param outline_line_cap ('butt', 'round', 'square') The line cap for the plot border outline.
#' @param outline_line_color (color) The line color for the plot border outline.
#' @param outline_line_dash The line dash for the plot border outline.
#' @param outline_line_dash_offset (integer) The line dash offset for the plot border outline.
#' @param outline_line_join ('miter', 'round', 'bevel') The line join for the plot border outline.
#' @param outline_line_width (integer) The line width for the plot border outline.
#' @param min_border (integer) A convenience property to set all all the min_X_border properties to the same value. If an individual border property is explicitly set, it will override min_border.
#' @param min_border_bottom (integer) Minimum size in pixels of the padding region below the bottom of the central plot region. This is a minimum. The padding region may expand as needed to accommodate titles or axes, etc.
#' @param min_border_left (integer) Minimum size in pixels of the padding region to the left of the central plot region. This is a minimum. The padding region may expand as needed to accommodate titles or axes, etc.
#' @param min_border_right (integer) Minimum size in pixels of the padding region to the right of the central plot region. This is a minimum. The padding region may expand as needed to accommodate titles or axes, etc.
#' @param min_border_top (integer) Minimum size in pixels of the padding region above the top of the central plot region. This is a minimum. The padding region may expand as needed to accommodate titles or axes, etc.
#' @param pars optionally specify a named list of all parameters - useful when dealing with theme lists
#' @example man-roxygen/ex-theme.R
#' @export
theme_plot <- function(fig,
  pars = NULL,
  background_fill_color = "white",
  background_fill_alpha = 1,
  border_fill_color = "white",
  border_fill_alpha = 1,
  outline_line_alpha = 1,
  outline_line_cap = "butt",
  outline_line_color = "black",
  outline_line_dash = NULL,
  outline_line_dash_offset = 0,
  outline_line_join = "miter",
  outline_line_width = 1,
  min_border = 50,
  min_border_bottom = 50,
  min_border_left = 50,
  min_border_right = 50,
  min_border_top = 50
) {
  # this will provide a list of all user-specified arguments
  # (can ignore the defaults for the ones they don't specify
  # because they are defaults if not specified in bokeh)
  if (is.null(pars)) {
    specified <- names(as.list(match.call())[-1])
    pars <- as.list(environment())[specified]
  }
  pars <- pars[names(pars) %in% names(figure_par_validator_map)]

  pars <- handle_extra_pars(pars, figure_par_validator_map)
  parnames <- names(pars)

  if (!is.null(fig$x$modeltype) && fig$x$modeltype == "GridPlot") {
    for (ii in seq_along(fig$x$spec$figs)) {
      for (nm in parnames)
        fig$x$spec$figs[[ii]]$x$spec$model$plot$attributes[[nm]] <- pars[[nm]]
    }
  } else {
    for (nm in parnames)
      fig$x$spec$model$plot$attributes[[nm]] <- pars[[nm]]
  }

  fig
}
