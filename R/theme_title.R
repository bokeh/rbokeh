#' Override theme parameters for general plot attributes
#'
#' @param fig figure to modify
#' @param background_fill_color (color) background color of plot
#' @param background_fill_alpha (numeric) background color alpha of plot
#' @param border_fill_color (color) fill color of border area of plot
#' @param border_fill_alpha (numeric) fill color alpha of border area of plot
#' @param align ('left', 'right', 'center') The text align for the plot title.
#' @param text_alpha The text alpha for the plot title.
#' @param text_baseline ('top', 'middle', 'bottom', 'alphabetic', 'hanging') The text baseline for the plot title.
#' @param text_color (color) The text color for the plot title.
#' @param text_font (string) The text font for the plot title.
#' @param text_font_size (string - e.g. '12pt') The text font size for the plot title.
#' @param text_font_style ('normal', 'italic', 'bold') The text font style for the plot title.
#' @param pars optionally specify a named list of all parameters - useful when dealing with theme lists
#' @examples
#' figure(title = "asdf") %>%
#'   ly_points(1:10) %>%
#'   theme_title(text_color = "red")
#' @export
theme_title <- function(fig,
  pars = NULL,
  background_fill_color = "white",
  background_fill_alpha = 1,
  border_fill_color = "white",
  border_fill_alpha = 1,
  align = "left",
  text_alpha = 1,
  text_baseline = "bottom",
  text_color = "#444444",
  text_font = "Helvetica",
  text_font_size = "12pt",
  text_font_style = "normal"
) {

  # this will provide a list of all user-specified arguments
  # (can ignore the defaults for the ones they don't specify
  # because they are defaults if not specified in bokeh)
  if (is.null(pars)) {
    specified <- names(as.list(match.call())[-1])
    pars <- as.list(environment())[specified]
  }
  pars <- pars[names(pars) %in% names(title_par_validator_map)]

  pars <- handle_extra_pars(pars, title_par_validator_map)
  parnames <- names(pars)

  if (!is.null(fig$x$modeltype) && fig$x$modeltype == "GridPlot") {
    for (ii in seq_along(fig$x$spec$figs)) {
      if (!is.null(fig$x$spec$figs[[ii]]$x$spec$model$title))
        for (nm in parnames)
          fig$x$spec$figs[[ii]]$x$spec$model$title$attributes[[nm]] <- pars[[nm]]
    }
  } else {
    if (!is.null(fig$x$spec$model$title)) {
      for (nm in parnames)
        fig$x$spec$model$title$attributes[[nm]] <- pars[[nm]]
    } else {
      message("Note: there isn't a title to appy a theme to...")
    }
  }

  fig
}

title_par_validator_map <- list(
  "background_fill_color" = "color",
  "background_fill_alpha" = "num_data_spec",
  "border_fill_color" = "color",
  "border_fill_alpha" = "alpha",
  "text_color" = "color",
  "text_alpha" = "num_data_spec",
  "align" = "align",
  "text_baseline" = "text_baseline",
  "text_font_style" = "font_style",
  "text_font" = "string",
  "text_font_size" = "font_size_string"
)
