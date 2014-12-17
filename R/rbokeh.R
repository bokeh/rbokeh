#' Start a Bokeh Figure
#' 
#' @param width figure width in pixels
#' @param height figure width in pixels
#' @param title a title to display above the plot. - "title" is also the prefix for a set of Text Properties, so you can set the font for the title with the parameter text_font.
#' @param xlim the extent of the plotting area in the x-dimension (will be computed automatically if not specified). 
#' @param ylim the extent of the plotting area in the y-dimension (will be computed automatically if not specified).
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
#' @param theme an rbokeh theme to use (tableau by default)
#' @export
#' @import htmlwidgets
#' @import methods
figure <- function(
  width = 480,
  height = 480,
  title = character(0),
  xlim = numeric(0),
  ylim = numeric(0),
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
  tools = character(0),
  theme = getOption("bokeh_theme")
) {
  BokehFigure$new(width = width, height = height, title = title, xlim = xlim, ylim = ylim, plot_width = plot_width, plot_height = plot_height, x_axis_type = x_axis_type, y_axis_type = y_axis_type, x_mapper_type = x_mapper_type, y_mapper_type = y_mapper_type, background_fill = background_fill, border_fill = border_fill, min_border = min_border, min_border_left = min_border_left, min_border_right = min_border_right, min_border_top = min_border_top, min_border_bottom = min_border_bottom, h_symmetry = h_symmetry, v_symmetry = v_symmetry, outline_line_color = outline_line_color, xaxes = xaxes, yaxes = yaxes, tools = tools, theme = theme)
}

## each method for adding a glyph can be named
## so that you can remove or overwrite glyphs

BokehFigure <- setRefClass("BokehFigure",
  fields = list(
    ## figure options
    width = "numeric",
    height = "numeric",
    title = "character",
    xlim = "ANY",
    ylim = "ANY",
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
    .yAxisType = "character",
    theme = "ANY"
  ),
  methods = list(
    ## to mimic R's 'points()'
    ## (a single interface to all markers)
    ##   - if 'type' is specified, it will override pch
    ##   - '...' is all line / fill properties
    ##   - 'col', 'bg', 'alpha', 'lwd' are R-like line / fill properties
    ##   - but things like line_fill, etc. will override these
    points = function(x, y = NULL, data = NULL, pch = 1, col = NULL, bg = NULL, alpha = NULL, lwd = NULL, cex = 1, size = NULL, name = NULL, type = NULL, ...) {

      if(!is.null(type))
        pch <- NULL

      ## deal with vector inputs from a data source
      if(!is.null(data)) {
        x     <- getVarData(data, substitute(x))
        y     <- getVarData(data, substitute(y))
        col   <- getVarData(data, substitute(col))
        bg    <- getVarData(data, substitute(bg))
        alpha <- getVarData(data, substitute(alpha))
        size  <- getVarData(data, substitute(size))
        cex   <- getVarData(data, substitute(cex))
      }

      ## translate different x, y types to vectors
      xy <- getXYData(x, y)

      ## figure out what glyph type to use
      if(is.character(pch)) {
        type <- "text"
      } else {
        type <- getPointGlyphType(pch, type)
      }

      ## sort out pch, type, and line / fill properties
      opts <- getPointOpts(type, pch, n = length(.self$.glyphSpecs), col = col, bg = bg, alpha, lwd = lwd, opts = list(...), theme = .self$theme)

      ## handle size
      if(is.null(size))
        size <- 10
      if(!is.null(cex)) {
        size <- size * cex
        if(!is.null(opts$line_width))
          opts$line_width <- opts$line_width * cex
      }

      if(type == "text") {
        if(is.null(pch))
          stop("must specify 'pch' if calling points() with type = 'text'", call. = FALSE)
        do.call(.self$text, c(xy, opts, list(text = rep(substr(pch, 1, 1), length(x)))))
      } else {
        axisTypeRange <- getGlyphAxisTypeRange(xy$x, xy$y, glyph = type)

        .makeGlyph(type, name, 
          data = c(xy, list(size = size)),
          args = opts, axisTypeRange = axisTypeRange)
      }
    },
    annular_wedge = function(x, y, inner_radius = 0.3, outer_radius = 0.7, start_angle = 0, end_angle = 2*pi, direction = "anticlock", name = NULL, ...) {
      checkArcDirection(direction)
      axisTypeRange <- getGlyphAxisTypeRange(x, y, assertX = "numeric", assertY = "numeric")
      .makeGlyph(type = "annular_wedge", name = name,
        data = list(x = x, y = y, inner_radius = inner_radius, outer_radius = outer_radius, start_angle = start_angle, end_angle = end_angle, direction = direction),
        args = list(...), axisTypeRange = axisTypeRange)
    },
    annulus = function(x, y, 
      inner_radius = 0.3, outer_radius = 0.7, 
      name = NULL, ...) {
      axisTypeRange <- getGlyphAxisTypeRange(x, y, assertX = "numeric", assertY = "numeric")
      .makeGlyph(type = "annulus", name = name,
        data = list(x = x, y = y, inner_radius = inner_radius, outer_radius = outer_radius),
        args = list(...), axisTypeRange = axisTypeRange)
    },
    arc = function(x, y, radius = 0.5, start_angle = 0, end_angle = 2*pi, direction = "anticlock", name = NULL, ...) {
      checkArcDirection(direction)
      axisTypeRange <- getGlyphAxisTypeRange(x, y, assertX = "numeric", assertY = "numeric")
      .makeGlyph(type = "arc", name = name,
        data = list(x = x, y = y, radius = radius, start_angle = start_angle, end_angle = end_angle, direction = direction),
        args = list(...), axisTypeRange = axisTypeRange)
    },
    bezier = function(x0, y0, x1, y1, cx0, cy0, cx1, cy1, name = NULL, ...) {
      axisTypeRange <- getGlyphAxisTypeRange(c(x0, x1), c(y0, y1), assertX = "numeric", assertY = "numeric")
      .makeGlyph(type = "bezier", name = name,
        data = list(x0 = x0, y0 = y0, x1 = x1, y1 = y1, cx0 = cx0, cy0 = cy0, cx1 = cx1, cy1 = cy1),
        args = list(...), axisTypeRange = axisTypeRange)
    },
    image = function(image, rows, cols, x = 0, y = 0, dw = 1, dh = 1, palette = "Spectral-10", name = NULL, ...) {
      axisTypeRange <- getGlyphAxisTypeRange(c(x, dw), c(y, dh), assertX = "numeric", assertY = "numeric")

      if(is.matrix(image)) {
        cols <- nrow(image)
        rows <- ncol(image)
        image <- array(image)
      }
      .makeGlyph(type = "image", name = name,
        data = list(image = image, rows = rows, cols = cols, x = x, y = y, dw = dw, dh = dh, palette = palette),
        args = list(...), axisTypeRange = axisTypeRange)
    },
    image_rgba = function(image, rows, cols, x, y, dw, dh, name = NULL, ...) {
      axisTypeRange <- getGlyphAxisTypeRange(c(x, dw), c(y, dh), assertX = "numeric", assertY = "numeric")
      .makeGlyph(type = "image", name = name,
        data = list(image = image, rows = rows, cols = cols, x = x, y = y, dw = dw, dh = dh),
        args = list(...), axisTypeRange = axisTypeRange)
    },
    image_url = function(x, y, url, angle = 0, name = NULL, ...) {
      # can this have "categorical" axes?
      axisTypeRange <- getGlyphAxisTypeRange(x, y)
      .makeGlyph(type = "image_url", name = name,
        data = list(x = x, y = y, url = url, angle = angle),
        args = list(...), axisTypeRange = axisTypeRange)
    },
    line = function(x, y, name = NULL, ...) {
      axisTypeRange <- getGlyphAxisTypeRange(x, y)
      .makeGlyph(type = "line", name = name,
        data = list(x = x, y = y), args = list(...), axisTypeRange = axisTypeRange)
    },
    ## to match R's lines
    lines = function(x, y = NULL, data = NULL, lty = 1, lwd = 1, ljoin = 1, col = NULL, alpha = NULL, name = NULL, ...) {

      if(!is.null(data)) {
        x   <- getVarData(data, substitute(x))
        y   <- getVarData(data, substitute(y))
        col <- getVarData(data, substitute(col))
      }

      xy <- getXYData(x, y)

      opts <- getLineOpts(lty, n = length(.self$.glyphSpecs), lwd = lwd, ljoin = ljoin, col = col, alpha = alpha, opts = list(...), theme = .self$theme)

      do.call(.self$line, c(xy, list(name = name), opts))
    },
    # abline = function(a = NULL, b = NULL, h = NULL, v = NULL, lty = 1, lwd = 1, ljoin = 1, col = NULL, alpha = NULL, name = NULL, ...) {

    # },
    multi_line = function(xs, ys, name = NULL, ...) {
      axisTypeRange <- getGlyphAxisTypeRange(unlist(xs), unlist(ys))
      .makeGlyph(type = "multi_line", name = name,
        data = list(xs = xs, ys = ys), args = list(...), axisTypeRange = axisTypeRange)
    },
    oval = function(x, y, width, height, angle, name = NULL, ...) {
      axisTypeRange <- getGlyphAxisTypeRange(x, y)
      .makeGlyph(type = "oval", name = name,
        data = list(x = x, y = y, width = width, height = height, angle = angle), args = list(...), axisTypeRange = axisTypeRange)
    },
    patch = function(x, y, name = NULL, ...) {
      axisTypeRange <- getGlyphAxisTypeRange(x, y)
      .makeGlyph(type = "patch", name = name,
        data = list(x = x, y = y), args = list(...), axisTypeRange = axisTypeRange)
    },
    patches = function(xs, ys, name = NULL, ...) {
      axisTypeRange <- getGlyphAxisTypeRange(unlist(xs), unlist(ys))
      .makeGlyph(type = "patches", name = name,
        data = list(xs = xs, ys = ys), args = list(...), axisTypeRange = axisTypeRange)
    },
    quad = function(left, right, top, bottom, name = NULL, ...) {
      axisTypeRange <- getGlyphAxisTypeRange(c(left, right), c(bottom, top))
      .makeGlyph(type = "quad", name = name,
        data = list(left = left, right = right, top = top, bottom = bottom), args = list(...), axisTypeRange = axisTypeRange)
    },
    quadratic = function(x0, y0, x1, y1, cx, cy, name = NULL, ...) {
      axisTypeRange <- getGlyphAxisTypeRange(c(x0, x1), c(y0, y1), assertX = "numeric", assertY = "numeric")
      .makeGlyph(type = "quadratic", name = name,
        data = list(x0 = x0, y0 = y0, x1 = x1, y1 = y1, cx = cx, cy = cy),
        args = list(...), axisTypeRange = axisTypeRange)
    },
    ray = function(x, y, length, angle = 0, name = NULL, ...) {
      axisTypeRange <- getGlyphAxisTypeRange(x, y)
      .makeGlyph(type = "ray", name = name,
        data = list(x = x, y = y, length = length, angle = angle),
        args = list(...), axisTypeRange = axisTypeRange)
    },
    rect = function(x, y, data = NULL, width = 1, height = 1, col = NULL, border = NULL, angle = 0, name = NULL, ...) {

      # to avoid warnings with members named 'width' and 'height'
      if(!is.null(data)) {
        x      <- getVarData(data, substitute(x))
        y      <- getVarData(data, substitute(y))
        col    <- getVarData(data, substitute(col))
        border <- getVarData(data, substitute(border))
        angle  <- getVarData(data, substitute(angle))
        wdth  <- getVarData(data, substitute(width))
        hght <- getVarData(data, substitute(height))
      } else {
        wdth <- width
        hght <- height
      }
      xy <- getXYData(x, y)

      opts <- list(...)
      if(is.null(col))
        col <- opts$fill_color
      if(is.null(border))
        border <- opts$line_color
      opts$fill_color <- col
      opts$line_color <- border
      if(is.numeric(xy$x)) {
        axisTypeRange <- getGlyphAxisTypeRange(c(xy$x, xy$x + wdth), c(xy$y, xy$y + hght))
      } else {
        axisTypeRange <- getGlyphAxisTypeRange(xy$x, xy$y)
      }
      .makeGlyph(type = "rect", name = name,
        data = c(xy, list(width = wdth, height = hght, angle = angle)),
        args = opts, axisTypeRange = axisTypeRange)
    },
    segment = function(x0, y0, x1, y1, name = NULL, ...) {
      axisTypeRange <- getGlyphAxisTypeRange(c(x0, x1), c(y0, y1))
      .makeGlyph(type = "segment", name = name,
        data = list(x0 = x0, y0 = y0, x1 = x1, y1 = y1),
        args = list(...), axisTypeRange = axisTypeRange)
    },
    # segments = function(x0, y0, x1 = x0, y1 = y0, col = NULL, lty = NULL, lwd = NULL) {},
    text = function(x, y = NULL, text = NULL, data = NULL, col = "black", angle = 0, name = NULL, ...) {

      if(!is.null(data)) {
        x    <- getVarData(data, substitute(x))
        y    <- getVarData(data, substitute(y))
        text <- getVarData(data, substitute(text))
        col  <- getVarData(data, substitute(col))
      }
      xy <- getXYData(x, y)

      opts <- list(...)
      opts$text_color <- col
      if(is.null(text))
        text <- seq_along(xy$x)
      axisTypeRange <- getGlyphAxisTypeRange(xy$x, xy$y)
      .makeGlyph(type = "text", name = name,
        data = c(xy, list(text = text, angle = angle)),
        args = list(...), axisTypeRange = axisTypeRange)
    },
    wedge = function(x, y, radius = 0.7, start_angle = 0, end_angle = 2*pi, direction = "anticlock", name = NULL, ...) {
      checkArcDirection(direction)
      axisTypeRange <- getGlyphAxisTypeRange(x, y, assertX = "numeric", assertY = "numeric")
      .makeGlyph(type = "wedge", name = name,
        data = list(x = x, y = y, radius = radius, start_angle = start_angle, end_angle = end_angle, direction = direction),
        args = list(...), axisTypeRange = axisTypeRange)
    },
    .makeGlyph = function(type, name, data, args, axisTypeRange) {
      ## see if any options won't be used
      checkOpts(args, type)

      ## make sure axis types match anything 
      ## that has already been plotted
      validateAxisType(figType = .xAxisType, curType = axisTypeRange$xAxisType, which = "x")
      validateAxisType(figType = .yAxisType, curType = axisTypeRange$yAxisType, which = "y")

      .xAxisType <<- axisTypeRange$xAxisType
      .yAxisType <<- axisTypeRange$yAxisType

      ## give it a unique name if not supplied
      if(is.null(name))
        name <- genGlyphName(names(.glyphSpecs))

      if(length(.glyphSpecs) > 0)
        if(name %in% names(.glyphSpecs))
          message("A glyph already exists with name '", name, "' - this is being replaced")

      ## validate the spec args
      # validateOpts(opts, type)

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

      ## spec needs to point to corresponding data
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
      .glyphXRanges[[name]] <<- axisTypeRange$xRange
      .glyphYRanges[[name]] <<- axisTypeRange$yRange

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

        ## set xlim and ylim if not set
        if(length(xlim) == 0) {
          message("xlim not specified explicitly... calculating...")
          options$xrange <- getAllGlyphRange(.glyphXRanges, .xAxisType)
        } else {
          options$xrange <- xlim
        }
        if(length(ylim) == 0) {
          message("ylim not specified explicitly... calculating...")
          options$yrange <- getAllGlyphRange(.glyphYRanges, .yAxisType)
        } else {
          options$yrange <- ylim
        }

        options$dims <- c(options$width, options$height)
        options$width <- NULL
        options$height <- NULL

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
    },
    hist = function(x, breaks = "Sturges", freq = TRUE,
      include.lowest = TRUE, right = TRUE,
      density = NULL, angle = 45, col = NULL, border = NULL,
      # main = paste("Histogram of" , xname),
      # xlab = xname, ylab,
      # axes = TRUE, labels = FALSE,
      warn.unused = FALSE,
      alpha = 1,
      ...) {

      hh <- graphics::hist.default(x = x, breaks = breaks,freq = freq, 
      include.lowest = include.lowest, right = right,
      density = density, angle = angle, col = col, border = border,
      # main = main
      # xlab = ylab, ylab = ylab,
      # axes = axes, labels = labels,
      warn.unused = warn.unused,
      plot = FALSE)

      thm <- getOption("bokeh_theme")

      opts <- list(...)
      if(!is.null(border)) {
        opts$line_color <- border
      } else {
        opts$line_color <- thm$glyph[1]
      }
      if(!is.null(col)) {
        opts$fill_color <- col
      } else {
        opts$fill_color <- reduceSaturation(opts$line_color)
      }

      if(!is.null(alpha)) {
        opts$line_alpha <- opts$fill_alpha <- alpha
      }

      y <- if(freq) {
        hh$counts
      } else {
        hh$density
      }
      do.call(.self$quad, c(list(left = hh$breaks[-length(hh$breaks)], right = hh$breaks[-1], top = y, bottom = 0), opts))
    }
  )
)

