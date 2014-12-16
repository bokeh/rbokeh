
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



handlePch <- function(pch, n, col, bg, alpha, lwd, opts, theme) {
  if(length(pch) > 1) {
    message("'pch' must be a single value... using first element")
    pch <- pch[1]
  }

  optNames <- names(opts)

  if(is.numeric(pch)) {
    pch <- as.integer(pch)
    if(!pch %in% setdiff(0:25, c(11, 14)))
      stop("'pch' must be a value from 0 to 25 (excluding 11 and 14", call. = FALSE)
    pchProps <- pchDict[[as.character(pch)]]
    type <- pchProps$glyph

    ## use line_color, line_width, etc. if specified
    ## else use specified 'col' and 'bg'
    ## otherwise turn to theme
    ## currently, pick color from theme based on 
    ## how many elements already exist in the plot
    ## this can surely be given more thought
    if(pchProps$line) {
      if(!is.null(col))
        opts$line_color <- col
      if(is.null(opts$line_color))
        opts$line_color <- theme$glyph[(n + 1) %% length(theme$glyph)]
      if(!is.null(lwd))
        opts$line_width <- lwd
      if(is.null(opts$line_width))
        opts$line_width <- 1
      if(!is.null(alpha))
        opts$line_alpha <- alpha
    } else {
      ## remove line props (if any) from opts
      if("line_color" %in% optNames) {
        message("'pch' setting is overriding specification of line_color")
      }
      opts["line_color"] <- list(NULL)
    }

    if(pchProps$fill) {
      if(!is.null(bg))
        opts$fill_color <- bg
      if(is.null(opts$fill_color)) {
        if(pchProps$line) {
          opts$fill_color <- reduceSaturation(opts$line_color)
        } else {
          opts$fill_color <- theme$glyph[(n + 1) %% length(theme$glyph)]
        }
      }
      if(!is.null(alpha))
        opts$fill_alpha <- alpha
    } else {
      ## remove fill props (if any) from opts
      if("fill_color" %in% optNames) {
        message("'pch' setting is overriding specification of fill_color")
      }
      opts["fill_color"] <- list(NULL)
    }
  } else if(is.character(pch)) {
    type <- "text"

    opts$text_align = "center"
    opts$text_baseline = "middle"

    if(!is.null(col))
      opts$text_color <- col
    if(is.null(opts$text_color))
      opts$text_color <- theme$glyph[(n + 1) %% length(theme$glyph)]
    if(!is.null(alpha))
      opts$text_alpha <- alpha
  }

  list(type = type, opts = opts)
}

handleLty <- function(lty, n, lwd, ljoin, col, alpha, opts, theme) {
  lty <- as.character(lty)
  lty <- lty[1]
  if(!lty %in% names(ltyDict))
    stop("'lty' must be one of: ", paste(names(ltyDict), collapse = ", "), call. = FALSE)

  if(is.null(opts$line_cap))
    opts$line_cap <- "round"

  ljoin <- as.character(ljoin)
  if(ljoin %in% names(ljoinDict))
    opts$line_join <- ljoinDict[[ljoin]]

  opts$line_dash <- ltyDict[[lty]]$line_dash
  opts$line_color <- col
  if(is.null(opts$line_color))
    opts$line_color <- theme$glyph[(n + 1) %% length(theme$line)]
  opts$line_width <- lwd
  if(!is.null(alpha))
    opts$line_alpha <- alpha

  opts
}

## take a hex color and reduce its saturation by a factor
## (used to get fill for pch=21:25)
reduceSaturation <- function(col, factor = 0.5) {
  col2 <- do.call(rgb2hsv, structure(as.list(col2rgb(col)[,1]), names = c("r", "g", "b")))
  col2['s', ] <- col2['s', ] * factor  
  do.call(hsv, as.list(col2[,1]))
}

## handle different x, y input types
## this should be more "class"-y
## but this will suffice
getXYData <- function(x, y) {
  if(is.null(y)) {
    if(is.list(x)) {
      return(list(x = x[[1]], y = x[[2]]))
    } else if(is.vector(x)) {
      return(list(x = seq_along(x), y = x))
    }
  }
  list(x = x, y = y)        
}

