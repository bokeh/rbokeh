

# pch can be number of vector of characters

# type - like R's pch in plot (see details) 
# line_color
# fill_color
# line_dash - like R's lty

# details:
# 1, 2, 3, 4
# 
# "asterisk", "circle", "circle_cross", "circle_x", "cross", "diamond", "diamond_cross", "inverted_triangle", "square", "square_cross", "square_x", "triangle", "x"

# line_color
# line_width
# line_alpha
# fill_color
# fill_alpha

# line_join
# line_cap
# line_dash
# line_dash_offset
# text_font
# text_font_size
# text_font_style
# text_color
# text_alpha
# text_align
# text_baseline

#' @export
lay_points <- function(fig, x, y = NULL, data = NULL, groups = NULL, type = 1, size = 10, line_color = NULL, line_alpha = 1, line_width = 1, fill_color = NULL, fill_alpha = NULL, name = NULL, ...) {

  validateFig(fig, "lay_points")

  ## deal with vector inputs from a data source
  if(!is.null(data)) {
    x          <- getVarData(data, substitute(x))
    y          <- getVarData(data, substitute(y))
    groups     <- getVarData(data, substitute(groups))
    line_color <- getVarData(data, substitute(line_color))
    line_alpha <- getVarData(data, substitute(line_alpha))
    line_width <- getVarData(data, substitute(line_width))
    fill_color <- getVarData(data, substitute(fill_color))
    fill_alpha <- getVarData(data, substitute(fill_alpha))
  }

  ## translate different x, y types to vectors
  xy <- getXYData(x, y)

  opts <- c(list(line_color = line_color, line_alpha = line_alpha, 
    line_width = line_width, fill_color = fill_color, 
    fill_alpha = fill_alpha), list(...))

  ## figure out what glyph type to use
  if(is.null(type))
    type <- "circle"

  if(length(type) > 1) {
    message("'type' must be a single value... using first element")
    type <- type[1]
  }

  nextColor <- getNextColor(fig)

  ## character type will be plotted as text glyphs
  if(is.character(type)) {
    if(!type %in% markerNames) {
      ## must be a 'character' point type
      ## set text color according to theme
      if(is.null(opts$text_color))
        opts$text_color <- nextColor
      opts$text_align <- "center"
      opts$text_baseline <- "middle"
      ## add the text layer
      idx <- which(grepl("^line|^fill", names(opts)))
      if(length(idx) > 0)
        opts[idx] <- NULL

      return(do.call(lay_text, 
        c(list(fig = fig, text = rep(substr(type, 1, 1), length(x))), xy, opts)))
    }
    ## otherwise it's a traditional bokeh glyph (circle, etc.)
    ## get properties
    props <- glyphProps[[type]]
    curGlyphProps <- list(glyph = type, fill = props$fp, line = props$lp)
  } else {
    type <- as.integer(type)
    if(!type %in% markerPchTypes)
      stop("Type: ", type, " is not a valid type.  See showTypes() for an example of what is valid.", call. = FALSE)

    ## get properties
    curGlyphProps <- pchDict[[as.character(type)]]
    type <- curGlyphProps$glyph
  }

  ## sort out line / fill properties
  if(curGlyphProps$line) {
    if(is.null(opts$line_color))
      opts$line_color <- nextColor
  } else {
    opts["line_color"] <- list(NULL)
  }

  if(is.null(opts$fill_color)) {
    if(curGlyphProps$fill) {
      if(curGlyphProps$line) {
        opts$fill_color <- reduceSaturation(opts$line_color)
      } else {
        opts$fill_color <- nextColor
      }
    }
  }

  axisTypeRange <- getGlyphAxisTypeRange(xy$x, xy$y, glyph = type)

  if(length(xy$x) == 1)
    xy$x <- list(xy$x)
  if(length(xy$y) == 1)
    xy$y <- list(xy$y)

  makeGlyph(fig, type, name, 
      data = c(xy, list(size = size)),
      args = opts, axisTypeRange = axisTypeRange)
}
