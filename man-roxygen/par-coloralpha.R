#' @param color color for the glyph - a hex code (with no alpha) or any of the 147 named CSS colors, e.g 'green', 'indigo' - for glyphs with both fill and line properties, see "Handling color" below
#' @param alpha the alpha transparency of the glyph between 0 (transparent) and 1 (opaque) - if glyph has both fill and color properties, see "Handling alpha" below
#' @section Handling color: The \code{color} parameter is a high-level plot attribute that provides default behavior for coloring glyphs.
#' \itemize{
#'   \item When using a glyph that only has line properties, this will be the color of the line.
#'   \item When using a glyph that has has line and fill properties, this will be the color of the line and the fill, with the alpha level of the fill reduced by 50\%.
#'   \item If full control over fill and line color is desired, the \code{fill_color} and \code{line_color} attributes can be specified explicitly and will override \code{color}.
#' }
#' @section Handling alpha: The \code{alpha} is a high-level plot attribute that sets the transparency of the glyph being plotted.
#' \itemize{
#'   \item When using a glyph that only has line properties, this will be the alpha of the line.
#'   \item When using a glyph that has has line and fill properties, this will be the alpha of the line and the alpha of the fill will be set to 50\% of this value.
#'   \item Individual fill and line alpha can be specified with \code{fill_alpha} and \code{line_alpha} and will override \code{alpha}.
#' }
