# bokeh class

# fields will be all of the options
# fig$title <- "stuff"

optionNames <- c("dims", "title","xrange","yrange","plot_width","plot_height","x_axis_type","y_axis_type","x_mapper_type","y_mapper_type","background_fill","border_fill","min_border","min_border_left","min_border_right","min_border_top","min_border_bottom","h_symmetry","v_symmetry","outline_line_color", "xaxes", "yaxes", "tools")

linePropNames <- c("line_color", "line_width", "line_alpha", "line_join", "line_cap", "line_dash", "line_dash_offset")
fillPropNames <- c("fill_color", "fill_alpha")

## glyphs are simple enough we can get away with
## not having formal classes for them
## but here's some additional info about each
## lp = does this glyph have line properties?
## fp = does this glyph have fill properties?
## tp = does this glyph have text properties?
glyphProps <- list(
  ###### markers ######
  asterisk = list(lp = TRUE, fp = FALSE, tp = FALSE),
  circle = list(lp = TRUE, fp = TRUE, tp = FALSE),
  circle_cross = list(lp = TRUE, fp = TRUE, tp = FALSE),
  circle_x = list(lp = TRUE, fp = TRUE, tp = FALSE),
  cross = list(lp = TRUE, fp = FALSE, tp = FALSE),
  diamond = list(lp = TRUE, fp = TRUE, tp = FALSE),
  diamond_cross = list(lp = TRUE, fp = TRUE, tp = FALSE),
  inverted_triangle = list(lp = TRUE, fp = TRUE, tp = FALSE),
  square = list(lp = TRUE, fp = TRUE, tp = FALSE),
  square_cross = list(lp = TRUE, fp = TRUE, tp = FALSE),
  square_x = list(lp = TRUE, fp = TRUE, tp = FALSE),
  triangle = list(lp = TRUE, fp = TRUE, tp = FALSE),
  x = list(lp = TRUE, fp = TRUE, tp = FALSE),
  ###### glyphs ######
  annular_wedge = list(lp = TRUE, fp = TRUE, tp = FALSE),
  annulus = list(lp = TRUE, fp = TRUE, tp = FALSE),
  arc = list(lp = TRUE, fp = FALSE, tp = FALSE),
  bezier = list(lp = TRUE, fp = FALSE, tp = FALSE),
  image = list(lp = FALSE, fp = FALSE, tp = FALSE),
  image_rgba = list(lp = FALSE, fp = FALSE, tp = FALSE),
  image_url = list(lp = FALSE, fp = FALSE, tp = FALSE),
  line = list(lp = TRUE, fp = FALSE, tp = FALSE),
  multi_line = list(lp = TRUE, fp = FALSE, tp = FALSE),
  oval = list(lp = TRUE, fp = TRUE, tp = FALSE),
  patch = list(lp = TRUE, fp = TRUE, tp = FALSE),
  patches = list(lp = TRUE, fp = TRUE, tp = FALSE),
  quad = list(lp = TRUE, fp = TRUE, tp = FALSE),
  quadratic = list(lp = TRUE, fp = FALSE, tp = FALSE),
  ray = list(lp = TRUE, fp = FALSE, tp = FALSE),
  rect = list(lp = TRUE, fp = TRUE, tp = FALSE),
  segment = list(lp = TRUE, fp = FALSE, tp = FALSE),
  text = list(lp = FALSE, fp = FALSE, tp = TRUE),
  wedge = list(lp = TRUE, fp = TRUE, tp = FALSE)
)

markerNames <- c("asterisk", "circle", "circle_cross", "circle_x", "cross", "diamond", "diamond_cross", "inverted_triangle", "square", "square_cross", "square_x", "triangle", "x")

#' Start a Bokeh Figure
#' 
#' @param dims figure dimensions
#' @param title a title to display above the plot. - "title" is also the prefix for a set of Text Properties, so you can set the font for the title with the parameter text_font.
#' @param xrange the extent of the plotting area in the x-dimension.
#' @param yrange the extent of the plotting area in the y-dimension.
#' @param plot_width,plot_height width and height of the entire plot in pixels, including border space
#' @param x_axis_type,y_axis_type can be set to "datetime" to create datetime axis
#' @param x_mapper_type,y_mapper_type can be set to "log" to specifically set the mapper used for the axis
#' @param background_fill a color to fill the inner plot area with
#' @param border_fill a color to fill the border region around the plot area with.
#' @param min_border a minimum size in pixels for the border. This applies to all sides of the plot.
#' @param min_border_left set left border individually
#' @param min_border_right set right border individually
#' @param min_border_top set top border individually
#' @param min_border_bottom set bottom border individually
#' @param h_symmetry,v_symmetry whether to symmetrize plot borders on opposite horizontal or vertical sides of the plot.
#' @param outline_line_color Line Properties that controls the appearance of an outline around the plot, for instance you can set the color of the outline with this
#' @param xaxes where to put x axis labels
#' @param yaxes where to put y axis labels
#' @param tools interactivity tools options
#' @export
#' @import htmlwidgets
#' @import methods
figure <- function(
  dims = c(400, 400),
  title = character(0),
  xrange = numeric(0),
  yrange = numeric(0),
  plot_width = numeric(0),
  plot_height = numeric(0),
  x_axis_type = character(0),
  y_axis_type = character(0),
  x_mapper_type = character(0),
  y_mapper_type = character(0),
  background_fill = character(0),
  border_fill = character(0),
  min_border = numeric(0),
  min_border_left = numeric(0),
  min_border_right = numeric(0),
  min_border_top = numeric(0),
  min_border_bottom = numeric(0),
  h_symmetry = logical(0),
  v_symmetry = logical(0),
  outline_line_color = character(0),
  xaxes = "below",
  yaxes = "left",
  tools = character(0)
) {
  BokehFigure$new(dims = dims, title = title, xrange = xrange, yrange = yrange, plot_width = plot_width, plot_height = plot_height, x_axis_type = x_axis_type, y_axis_type = y_axis_type, x_mapper_type = x_mapper_type, y_mapper_type = y_mapper_type, background_fill = background_fill, border_fill = border_fill, min_border = min_border, min_border_left = min_border_left, min_border_right = min_border_right, min_border_top = min_border_top, min_border_bottom = min_border_bottom, h_symmetry = h_symmetry, v_symmetry = v_symmetry, outline_line_color = outline_line_color, xaxes = xaxes, yaxes = yaxes, tools = tools)
}

## each method for adding a glyph can be named
## so that you can remove or overwrite glyphs

BokehFigure <- setRefClass("BokehFigure",
  fields = list(
    ## figure options
    dims = "numeric",
    title = "character",
    xrange = "ANY",
    yrange = "ANY",
    plot_width = "numeric",
    plot_height = "numeric",
    x_axis_type = "character",
    y_axis_type = "character",
    x_mapper_type = "character",
    y_mapper_type = "character",
    background_fill = "character",
    border_fill = "character",
    min_border = "numeric",
    min_border_left = "numeric",
    min_border_right = "numeric",
    min_border_top = "numeric",
    min_border_bottom = "numeric",
    h_symmetry = "logical",
    v_symmetry = "logical",
    outline_line_color = "character",
    xaxes = "character",
    yaxes = "character",
    tools = "character",
    ## data and spec for each glyph
    .glyphData = "list",
    ## spec for each glyph
    .glyphSpecs = "list",
    ## keep track of x and y range of each glyph
    .glyphXRanges = "list",
    .glyphYRanges = "list",
    ## keep track of the axes ('cat' or 'num')
    .xAxisType = "character",
    .yAxisType = "character"
  ),
  methods = list(
    asterisk = function(x, y, size = NULL, name = NULL, ...) {
      .makeMarker(type = "asterisk", x, y, size, name = NULL, ...)
    },
    circle = function(x, y, size = NULL, name = NULL, ...) {
      .makeMarker(type = "circle", x, y, size, name = NULL, ...)
    },
    circle_cross = function(x, y, size = NULL, name = NULL, ...) {
      .makeMarker(type = "circle_cross", x, y, size, name = NULL, ...)
    },
    circle = function(x, y, size = NULL, name = NULL, ...) {
      .makeMarker(type = "circle", x, y, size, name = NULL, ...)
    },
    circle_x = function(x, y, size = NULL, name = NULL, ...) {
      .makeMarker(type = "circle_x", x, y, size, name = NULL, ...)
    },
    cross = function(x, y, size = NULL, name = NULL, ...) {
      .makeMarker(type = "cross", x, y, size, name = NULL, ...)
    },
    diamond = function(x, y, size = NULL, name = NULL, ...) {
      .makeMarker(type = "diamond_cross", x, y, size, name = NULL, ...)
    },
    diamond_cross = function(x, y, size = NULL, name = NULL, ...) {
      .makeMarker(type = "diamond_cross", x, y, size, name = NULL, ...)
    },
    inverted_triangle = function(x, y, size = NULL, name = NULL, ...) {
      .makeMarker(type = "inverted_triangle", x, y, size, name = NULL, ...)
    },
    square = function(x, y, size = NULL, name = NULL, ...) {
      .makeMarker(type = "square", x, y, size, name = NULL, ...)
    },
    square_cross = function(x, y, size = NULL, name = NULL, ...) {
      .makeMarker(type = "square_cross", x, y, size, name = NULL, ...)
    },
    square_x = function(x, y, size = NULL, name = NULL, ...) {
      .makeMarker(type = "square_x", x, y, size, name = NULL, ...)
    },
    triangle = function(x, y, size = NULL, name = NULL, ...) {
      .makeMarker(type = "triangle", x, y, size, name = NULL, ...)
    },
    x = function(x, y, size = NULL, name = NULL, ...) {
      .makeMarker(type = "x", x, y, size, name = NULL, ...)
    },
    ## to mimic R's 'points()'
    ## (a single interface to all markers)
    points = function(x, y, size = NULL, type = "circle", ...) {
      # type specified as an integer index
      if(is.numeric(type)) {
        try({
          type <- markerNames[type]
        })
      }
      if(!type %in% markerNames)
          stop("type = '", type, "' is not supported.  Please choose from: ", paste(markerNames, collapse = ", "), call. = FALSE)
      .makeMarker(type = type, x, y, size, name = NULL, ...)
    },
    annular_wedge = function(x, y, inner_radius = 0.3, outer_radius = 0.7, start_angle = 0, end_angle = 2*pi, direction = "anticlock", name = NULL, ...) {
      checkArcDirection(direction)
      typeRange <- getGlyphTypeRange(x, y, assertX = "numeric", assertY = "numeric")
      .makeGlyph(type = "annular_wedge", name = name,
        data = list(x = x, y = y, inner_radius = inner_radius, outer_radius = outer_radius, start_angle = start_angle, end_angle = end_angle, direction = direction),
        args = list(...), typeRange = typeRange)
    },
    annulus = function(x, y, 
      inner_radius = 0.3, outer_radius = 0.7, 
      name = NULL, ...) {
      typeRange <- getGlyphTypeRange(x, y, assertX = "numeric", assertY = "numeric")
      .makeGlyph(type = "annulus", name = name,
        data = list(x = x, y = y, inner_radius = inner_radius, outer_radius = outer_radius),
        args = list(...), typeRange = typeRange)
    },
    arc = function(x, y, radius = 0.5, start_angle = 0, end_angle = 2*pi, direction = "anticlock", name = NULL, ...) {
      checkArcDirection(direction)
      typeRange <- getGlyphTypeRange(x, y, assertX = "numeric", assertY = "numeric")
      .makeGlyph(type = "arc", name = name,
        data = list(x = x, y = y, radius = radius, start_angle = start_angle, end_angle = end_angle, direction = direction),
        args = list(...), typeRange = typeRange)
    },
    bezier = function(x0, y0, x1, y1, cx0, cy0, cx1, cy1, name = NULL, ...) {
      typeRange <- getGlyphTypeRange(c(x0, x1), c(y0, y1), assertX = "numeric", assertY = "numeric")
      .makeGlyph(type = "bezier", name = name,
        data = list(x0 = x0, y0 = y0, x1 = x1, y1 = y1, cx0 = cx0, cy0 = cy0, cx1 = cx1, cy1 = cy1),
        args = list(...), typeRange = typeRange)
    },
    image = function(image, rows, cols, x, y, dw, dh, palette = "Spectral-10", name = NULL, ...) {
      typeRange <- getGlyphTypeRange(c(x, dw), c(y, dh), assertX = "numeric", assertY = "numeric")
      .makeGlyph(type = "image", name = name,
        data = list(image = image, rows = rows, cols = cols, x = x, y = y, dw = dw, dh = dh, palette = palette),
        args = list(...), typeRange = typeRange)
    },
    image_rgba = function(image, rows, cols, x, y, dw, dh, name = NULL, ...) {
      typeRange <- getGlyphTypeRange(c(x, dw), c(y, dh), assertX = "numeric", assertY = "numeric")
      .makeGlyph(type = "image", name = name,
        data = list(image = image, rows = rows, cols = cols, x = x, y = y, dw = dw, dh = dh),
        args = list(...), typeRange = typeRange)
    },
    image_url = function(x, y, url, angle = 0, name = NULL, ...) {
      # can this have "categorical" axes?
      typeRange <- getGlyphTypeRange(x, y)
      .makeGlyph(type = "image_url", name = name,
        data = list(x = x, y = y, url = url, angle = angle),
        args = list(...), typeRange = typeRange)
    },
    line = function(x, y, name = NULL, ...) {
      typeRange <- getGlyphTypeRange(x, y)
      .makeGlyph(type = "line", name = name,
        data = list(x = x, y = y), args = list(...), typeRange = typeRange)
    },
    ## to match R
    lines = function(...) {
      line(...)
    },
    multi_line = function(xs, ys, name = NULL, ...) {
      typeRange <- getGlyphTypeRange(unlist(xs), unlist(ys))
      .makeGlyph(type = "multi_line", name = name,
        data = list(xs = xs, ys = ys), args = list(...), typeRange = typeRange)
    },
    oval = function(x, y, width, height, angle, name = NULL, ...) {
      typeRange <- getGlyphTypeRange(x, y)
      .makeGlyph(type = "oval", name = name,
        data = list(x = x, y = y, width = width, height = height, angle = angle), args = list(...), typeRange = typeRange)
    },
    patch = function(x, y, name = NULL, ...) {
      typeRange <- getGlyphTypeRange(x, y)
      .makeGlyph(type = "patch", name = name,
        data = list(x = x, y = y), args = list(...), typeRange = typeRange)
    },
    patches = function(xs, ys, name = NULL, ...) {
      typeRange <- getGlyphTypeRange(unlist(xs), unlist(ys))
      .makeGlyph(type = "patches", name = name,
        data = list(xs = xs, ys = ys), args = list(...), typeRange = typeRange)
    },
    quad = function(left, right, top, bottom, name = NULL, ...) {
      typeRange <- getGlyphTypeRange(c(left, right), c(bottom, top))
      .makeGlyph(type = "quad", name = name,
        data = list(left = left, right = right, top = top, bottom = bottom), args = list(...), typeRange = typeRange)
    },
    quadratic = function(x0, y0, x1, y1, cx, cy, name = NULL, ...) {
      typeRange <- getGlyphTypeRange(c(x0, x1), c(y0, y1), assertX = "numeric", assertY = "numeric")
      .makeGlyph(type = "quadratic", name = name,
        data = list(x0 = x0, y0 = y0, x1 = x1, y1 = y1, cx = cx, cy = cy),
        args = list(...), typeRange = typeRange)
    },
    ray = function(x, y, length, angle = 0, name = NULL, ...) {
      typeRange <- getGlyphTypeRange(x, y)
      .makeGlyph(type = "ray", name = name,
        data = list(x = x, y = y, length = length, angle = angle),
        args = list(...), typeRange = typeRange)
    },
    rect = function(x, y, width, height, angle = 0, name = NULL, ...) {
      typeRange <- getGlyphTypeRange(c(x, x + width), c(y, y + height))
      .makeGlyph(type = "rect", name = name,
        data = list(x = x, y = y, width = width, height = height, angle = angle),
        args = list(...), typeRange = typeRange)
    },
    segment = function(x0, y0, x1, y1, name = NULL, ...) {
      typeRange <- getGlyphTypeRange(c(x0, x1), c(y0, y1))
      .makeGlyph(type = "segment", name = name,
        data = list(x0 = x0, y0 = y0, x1 = x1, y1 = y1),
        args = list(...), typeRange = typeRange)
    },
    text = function(x, y, text, angle = 0, name = NULL, ...) {
      typeRange <- getGlyphTypeRange(x, y)
      .makeGlyph(type = "text", name = name,
        data = list(x = x, y = y, text = text, angle = angle),
        args = list(...), typeRange = typeRange)
    },
    wedge = function(x, y, radius = 0.7, start_angle = 0, end_angle = 2*pi, direction = "anticlock", name = NULL, ...) {
      checkArcDirection(direction)
      typeRange <- getGlyphTypeRange(x, y, assertX = "numeric", assertY = "numeric")
      .makeGlyph(type = "wedge", name = name,
        data = list(x = x, y = y, radius = radius, start_angle = start_angle, end_angle = end_angle, direction = direction),
        args = list(...), typeRange = typeRange)
    },
    ## get things ready to send to .makeGlyph
    .makeMarker = function(type, x, y, size, name, ...) {
      if(is.null(size))
        size <- 10
      typeRange <- getGlyphTypeRange(x, y)
      .makeGlyph(type, name, 
        data = list(x = x, y = y, size = size),
        args = list(...), typeRange = typeRange)
    },
    .makeGlyph = function(type, name, data, args, typeRange) {
      ## make sure axis types match anything 
      ## that has already been plotted
      validateAxisType(figType = .xAxisType, curType = typeRange$xAxisType, which = "x")
      validateAxisType(figType = .yAxisType, curType = typeRange$yAxisType, which = "y")

      .xAxisType <<- typeRange$xAxisType
      .yAxisType <<- typeRange$yAxisType

      ## give it a unique name if not supplied
      if(is.null(name))
        name <- genGlyphName(names(.glyphSpecs))

      if(length(.glyphSpecs) > 0)
        if(name %in% names(.glyphSpecs))
          message("A glyph already exists with name '", name, "' - this is being replaced")

      ## validate the spec args
      # TODO

      ## move all data scalars over to the spec
      dataLengths <- sapply(data, length)
      dataNames <- names(data)
      scalarInd <- which(dataLengths == 1)
      for(ii in scalarInd)
        args[[dataNames[ii]]] <- data[[ii]]
      data[scalarInd] <- NULL

      ## move all non-scalar args over to data
      ## except for "line_dash"
      argLengths <- sapply(args, length)
      argNames <- names(args)
      longInd <- which(argLengths > 1 & argNames != "line_dash")

      for(ii in longInd) {
        data[[argNames[ii]]] <- args[[ii]]
      }
      args[longInd] <- NULL

      ## spec needs to point to
      dataNames <- names(data)
      for(nm in dataNames)
        args[[nm]] <- nm

      ## add glyph type
      args$type <- type

      ## fix spec for "text" glyph
      if("text" %in% names(args)) {
         args$text <- list(field = "text")
      }

      .glyphSpecs[[name]] <<- args
      .glyphData[[name]] <<- data

      ## compute x and y range for this glyph
      .glyphXRanges[[name]] <<- typeRange$xRange
      .glyphYRanges[[name]] <<- typeRange$yRange

      ## for chaining...
      # invisible(.self)
    },
    show = function(debug = FALSE) {
      if(length(.glyphSpecs) == 0) {        
        message("This figure is empty...")
      } else {
        ## put options together
        options <- list()
        for(opt in optionNames) {
          fld <- .self$field(opt)
          if(length(fld) > 0)
            options[[opt]] <- fld
        }
        options$r_debug <- debug

        ## set xrange and yrange if not set
        if(length(xrange) == 0) {
          message("xrange not specified explicitly... calculating...")
          options$xrange <- getAllGlyphRange(.glyphXRanges, .xAxisType)
        }
        if(length(yrange) == 0) {
          message("yrange not specified explicitly... calculating...")
          options$yrange <- getAllGlyphRange(.glyphYRanges, .yAxisType)
        }

        ## create widget
        htmlwidgets::createWidget(
           name = 'rbokeh',
           list(
              data = unname(.glyphData),
              spec = unname(.glyphSpecs),
              options = options
           ),
           width = options$dims[1] + 50,
           height = options$dims[2] + 50,
           package = 'rbokeh'
        )
      }
    }
  )
)

checkArcDirection <- function(direction) {
  if(!direction %in% c("clock", "anticlock"))
    stop("'direction' must be 'clock' or 'anticlock'", call. = FALSE)
}

## take a set of glyph specification names
## and come up with the next increment of 'glyph[int]'
genGlyphName <- function(specNames) {
  # specNames <- c("asdf", "glyph1", "glyph23", "qwert", "aglyph7", "glyph12b")
  if(length(specNames) == 0) {
    name <- "glyph1"
  } else {
    glyphNames <- specNames[grepl("^glyph([0-9]+)$", specNames)]
    if(length(glyphNames) == 0) {
      name <- "glyph1"
    } else {
      nn <- as.integer(gsub("glyph", "", glyphNames))
      name <- paste("glyph", max(nn) + 1, sep = "")
    }
  }
  name
}

getGlyphTypeRange <- function(x, y, assertX = NULL, assertY = NULL, glyph = "") {
  xAxisType <- getGlyphAxisType(x)
  yAxisType <- getGlyphAxisType(y)

  if(glyph != "")
    glyphText <- paste("'", glyph, "' ")

  if(!is.null(assertX)) {
    if(xAxisType != assertX)
      stop("Glyph ", glyph, " expects a ", assertX, " x axis", call. = FALSE)
  }
  if(!is.null(assertY)) {
    if(yAxisType != assertY)
      stop("Glyph ", glyph, "expects a ", assertY, " y axis", call. = FALSE)
  }

  list(
    xAxisType = xAxisType,
    yAxisType = yAxisType,
    xRange = getGlyphRange(x, xAxisType),
    yRange = getGlyphRange(y, yAxisType)
  )
}

## determine whether axis is "numeric" or "categorical"
getGlyphAxisType <- function(a) {
  # this will surely get more complex...
  ifelse(is.character(a), "categorical", "numeric")
}

## determine the range of an axis for a glyph
getGlyphRange <- function(a, axisType = NULL, ...) {
  if(is.null(axisType))
    axisType <- getGlyphAxisType(a)
  ## ... can be size, etc. attributes
  if(axisType == "numeric") {
    range(a, na.rm = TRUE)
  } else {
    unique(a)
  }
}

validateAxisType <- function(figType, curType, which) {
  if(length(figType) > 0) {
    # make this more informative...
    if(figType != curType)
      stop(which, " axis type (numerical / categorical) does not match that of other elements in this figure", call. = FALSE)
  }
}

## take a collection of glyph ranges (x or y axis)
## and find the global range across all glyphs
getAllGlyphRange <- function(ranges, axisType = "numeric") {
  if(axisType == "numeric") {
    rangeMat <- do.call(rbind, ranges)
    hardRange <- c(min(rangeMat[,1], na.rm = TRUE), 
      max(rangeMat[,2], na.rm = TRUE))
    hardRange + c(-1, 1) * 0.07 * diff(hardRange)
  } else {
    sort(unique(do.call(c, ranges)))
  }
}

