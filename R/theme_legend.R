#' Override theme parameters for legend attributes
#'
#' @param fig figure to modify
#' @param border_line_alpha The line alpha for the legend border outline.
#' @param border_line_cap ('butt', 'round', 'square') The line cap for the legend border outline.
#' @param border_line_color The line color for the legend border outline.
#' @param border_line_dash The line dash for the legend border outline.
#' @param border_line_dash_offset The line dash offset for the legend border outline.
#' @param border_line_join ('miter', 'round', 'bevel') The line join for the legend border outline.
#' @param border_line_width The line width for the legend border outline.
#' @param glyph_height The height (in pixels) that the rendered legend glyph should occupy.
#' @param glyph_width The width (in pixels) that the rendered legend glyph should occupy.
#' @param label_height The height (in pixels) of the area that legend labels should occupy.
#' @param label_standoff The distance (in pixels) to separate the label from its associated glyph.
#' @param label_text_align ('left', 'right', 'center') The text align for the legend labels.
#' @param label_text_alpha The text alpha for the legend labels.
#' @param label_text_baseline ('top', 'middle', 'bottom', 'alphabetic', 'hanging') The text baseline for the legend labels.
#' @param label_text_color The text color for the legend labels.
#' @param label_text_font The text font for the legend labels.
#' @param label_text_font_size The text font size for the legend labels.
#' @param label_text_font_style ('normal', 'italic', 'bold') The text font style for the legend labels.
#' @param label_width The width (in pixels) of the area that legend labels should occupy.
#' @param legend_padding Amount of padding around the legend.
#' @param legend_spacing Amount of spacing between legend entries.
#' @param pars optionally specify a named list of all parameters - useful when dealing with theme lists
#' @examples
#' figure(legend_location = "top_left") %>%
#'   ly_points(1:10, legend = "a") %>%
#'   theme_legend(border_line_width = 2)
#' @export
theme_legend <- function(fig,
  border_line_alpha = 1,
  border_line_cap = "butt",
  border_line_color = "black",
  border_line_dash = NULL,
  border_line_dash_offset = 0,
  border_line_join = "miter",
  border_line_width = 1,
  glyph_height = 20,
  glyph_width = 20,
  label_height = 20,
  label_standoff = 15,
  label_text_align = "left",
  label_text_alpha = 1,
  label_text_baseline = "bottom",
  label_text_color ="#444444",
  label_text_font = "Helvetica",
  label_text_font_size = "12pt",
  label_text_font_style = "normal",
  label_width = 50,
  legend_padding = 10,
  legend_spacing = 3,
  pars = NULL
) {
  # this will provide a list of all user-specified arguments
  # (can ignore the defaults for the ones they don't specify
  # because they are defaults if not specified in bokeh)
  if(is.null(pars)) {
    specified <- names(as.list(match.call())[-1])
    pars <- as.list(environment())[specified]
  }
  pars <- pars[names(pars) %in% names(legend_par_validator_map)]

  pars <- handle_extra_pars(pars, legend_par_validator_map)
  parnames <- names(pars)

  for(nm in parnames)
    fig$x$spec$legend_attrs[[nm]] <- pars[[nm]]

  fig
}


