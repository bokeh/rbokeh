#' Override theme parameters for grid attributes
#'
#' @param fig figure to modify
#' @param which which grids to apply attributes to ("x" and/or "y")
#' @param band_fill_alpha The fill alpha of alternating bands between Grid lines.
#' @param band_fill_color The fill color of alternating bands between Grid lines.
#' @param grid_line_alpha The line alpha of the Grid lines.
#' @param grid_line_cap ('butt', 'round', 'square') The line cap of the Grid lines.
#' @param grid_line_color The line color of the Grid lines.
#' @param grid_line_dash The line dash of the Grid lines.
#' @param grid_line_dash_offset The line dash offset of the Grid lines.
#' @param grid_line_join ('miter', 'round', 'bevel') The line join of the Grid lines.
#' @param grid_line_width The line width of the Grid lines.
#' @param minor_grid_line_alpha The line alpha of the minor Grid lines.
#' @param minor_grid_line_cap ('butt', 'round', 'square') The line cap of the minor Grid lines.
#' @param minor_grid_line_color The line color of the minor Grid lines.
#' @param minor_grid_line_dash The line dash of the minor Grid lines.
#' @param minor_grid_line_dash_offset The line dash offset of the minor Grid lines.
#' @param minor_grid_line_join ('miter', 'round', 'bevel') The line join of the minor Grid lines.
#' @param minor_grid_line_width The line width of the minor Grid lines.
#' @param pars optionally specify a named list of all parameters - useful when dealing with theme lists
#' @example man-roxygen/ex-theme.R
#' @export
theme_grid <- function(fig,
  which = c("x", "y"),
  band_fill_alpha = 1,
  band_fill_color = "gray",
  grid_line_alpha = 1,
  grid_line_cap = "butt",
  grid_line_color = "black",
  grid_line_dash = NULL,
  grid_line_dash_offset = 0,
  grid_line_join = "miter",
  grid_line_width = 1,
  minor_grid_line_alpha = 1,
  minor_grid_line_cap = "butt",
  minor_grid_line_color = "black",
  minor_grid_line_dash = NULL,
  minor_grid_line_dash_offset = 0,
  minor_grid_line_join = "miter",
  minor_grid_line_width = 1,
  pars = NULL
) {
  # this will provide a list of all user-specified arguments
  # (can ignore the defaults for the ones they don't specify
  # because they are defaults if not specified in bokeh)
  if(is.null(pars)) {
    specified <- names(as.list(match.call())[-1])
    pars <- as.list(environment())[specified]
  }
  pars <- pars[names(pars) %in% names(grid_par_validator_map)]

  pars <- handle_extra_pars(pars, grid_par_validator_map)
  parnames <- names(pars)

  ## if an axis hasn't been created yet (usually done in prepare_figure)
  ## then create it here and apply attributes
  ## could alternatively save attributes and apply in prepare_figure
  if("x" %in% which && fig$x$spec$xaxes != FALSE) {
    if(is.null(fig$x$spec$model[["x_grid"]]))
      fig <- fig %>% x_axis()
    for(nm in parnames)
      fig$x$spec$model[["x_grid"]]$attributes[[nm]] <- pars[[nm]]
  }
  if("y" %in% which && fig$x$spec$yaxes != FALSE) {
    if(is.null(fig$x$spec$model[["y_grid"]]))
      fig <- fig %>% y_axis()
    for(nm in parnames)
      fig$x$spec$model[["y_grid"]]$attributes[[nm]] <- pars[[nm]]
  }

  fig
}


