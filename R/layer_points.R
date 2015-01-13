

# major aesthetic specification:
# type: what type of glyph to plot at each point
# color: the color
#   when using a type that has only an outline, this will be the color of the outline
#   when using a type that has outline and fill, this will be the color of the outline and the fill will be a slightly less-saturated value of the same color
# alpha: the alpha of both the line and the fill
# size: the size of the glyph
# type can be any of the following:
# 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25
# these matche R's pch setting (see point_types())
# except 11 and 14 are missing, and 16, 19, 20 are the same
# or asterisk, circle, circle_cross, circle_x, cross, diamond, diamond_cross, inverted_triangle, square, square_cross, square_x, triangle, x
# the integer-based types simply map to any of these named types but with different line and/or fill properties

# in addition, lower-level control can be specified over 

# grouping:
# any of the following properties can be specified as a "grouping" variable, with length as long as x and y, for which colors from the specified theme will be assigned to groups based on the unique values provided.  either a vector of valid values for that field can be given (e.g. for colors, a vector of valid css color names or hex codes) or a vector of factor levels which will be used to assign attributes based on the theme
# grouping variables: type, color, line_color, fill_color

#' @export
lay_points <- function(fig, x, y = NULL, data = NULL, type = 1, color = "blue", size = 10, line_color = NULL, line_alpha = 1, line_width = 1, fill_color = NULL, fill_alpha = NULL, lname = NULL, lgroup = NULL, lsubgroup = NULL, ...) {

  validateFig(fig, "lay_points")

  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))

  ## deal with vector inputs from a data source
  if(!is.null(data)) {
    x          <- eval(substitute(x), data)
    y          <- eval(substitute(y), data)
    line_color <- eval(substitute(line_color), data)
    line_alpha <- eval(substitute(line_alpha), data)
    line_width <- eval(substitute(line_width), data)
    fill_color <- eval(substitute(fill_color), data)
    fill_alpha <- eval(substitute(fill_alpha), data)    
  }

  ## translate different x, y types to vectors
  xy <- getXYData(x, y)
  xyNames <- getXYNames(x, y, xname, yname, list(...))

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

  makeGlyph(fig, type, lname, 
    data = c(xy, list(size = size)),
    args = opts, axisTypeRange = axisTypeRange, 
    xname = xyNames$x, yname = xyNames$y)
}
