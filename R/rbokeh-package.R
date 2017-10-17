utils::globalVariables(c("xleft", "ybottom", "xright", "ytop",
  "height", "x0", "y0", "x1", "y1", "legend", "xs", "ys"))


#' rbokeh: R interface for Bokeh
#'
#' R interface for creating plots in Bokeh.  Bokeh by Continuum Analytics, \url{http://bokeh.pydata.org/en/latest/}
#'
#' For full function documentation, \code{help(package = "rbokeh")}
#' @name rbokeh-package
#' @aliases rbokeh
#' @docType package
NULL

#' "co2" dataset
#'
#' @name co2_df
#' @docType data
#' @description
#' The \code{\link[datasets]{co2}} dataset in data frame form.
#' @usage co2_df
#' @keywords data
NULL

#' "Periodic Table" dataset
#'
#' @name elements
#' @docType data
#' @description
#' Data for periodic table of the elements
#' @usage elements
#' @keywords data
#' @examples
#' figure(title = "Periodic Table", data = elements,
#'   xgrid = FALSE, ygrid = FALSE, xlab = "", ylab = "",
#'   xlim = as.character(1:18), ylim = c("   ", "  ", " ", as.character(7:1)),
#'   height = 700, width = 1200) %>%
#'   ly_crect(cat_offset(group, group_offset), period, 0.9, 0.9,
#'     color = group_block, fill_alpha = 0.6, legend = FALSE,
#'     hover = list(name, atomic_number, group_block, atomic_mass,
#'       electronic_configuration)) %>%
#'   ly_text(cat_offset(group, sym_offset), period, text = symbol,
#'     font_style = "bold", font_size = "15pt",
#'     align = "left", baseline = "middle") %>%
#'   ly_text(cat_offset(group, sym_offset), cat_offset(period, 0.3), text = atomic_number_p,
#'     font_size = "9pt", align = "left", baseline = "middle") %>%
#'   ly_text(cat_offset(group, sym_offset), cat_offset(period, -0.2), text = name,
#'     font_size = "6pt", align = "left", baseline = "middle") %>%
#'   ly_text(cat_offset(group, sym_offset), cat_offset(period, -0.35), text = atomic_mass,
#'     font_size = "6pt", align = "left", baseline = "middle") %>%
#'   x_axis(axis = axis_spec(visible = FALSE)) %>%
#'   y_axis(axis = axis_spec(visible = FALSE))
NULL

#' Pipe figures
#'
#' @import htmlwidgets
#' @importFrom magrittr %>%
#' @importFrom rlang quo quos is_quosure enquo eval_tidy quo_name quo_is_null
#' @importFrom utils getFromNamespace
#' @importFrom glue glue
#' @name %>%
#' @rdname pipe
#' @export
#' @param lhs a Bokeh figure
#' @param rhs a layer to add to the figure
NULL

# #' Flight frequency dataset
# #'
# #' @name flightfreq
# #' @docType data
# #' @description
# #' Daily counts of domestic flights in the U.S. from 1999 to mid-2008
# #' @usage flightfreq
# #' @keywords data
# #' @example man-roxygen/ex-flightfreq.R
# NULL
