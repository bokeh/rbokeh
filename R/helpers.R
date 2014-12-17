
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

## get the axis type and range for x and y axes
getGlyphAxisTypeRange <- function(x, y, assertX = NULL, assertY = NULL, glyph = "") {
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

getPointGlyphType <- function(pch, type) {
  ## pch overrides type
  if(!is.null(pch) && !is.null(type)) {
    type <- NULL
    message("both 'pch' and 'type' were specified - honoring 'pch' over 'type'")
  }
  if(is.null(pch) && is.null(type))
    type <- "circle"

  if(length(pch) > 1) {
    message("'pch' must be a single value... using first element")
    pch <- pch[1]
  }

  ## if 'type' isn't specified, get type from pch
  if(is.null(type)) {
    # translate pch to type
    if(is.numeric(pch)) {
      pch <- as.integer(pch)
      if(!pch %in% setdiff(0:25, c(11, 14)))
        stop("'pch' must be a value from 0 to 25 (excluding 11 and 14", call. = FALSE)
      type <- pchDict[[as.character(pch)]]$glyph
    } else {
      type <- "text"
    }
  }

  if(!type %in% markerNames)
    stop("type = '", type, "' is not supported.  Please choose from: ", paste(markerNames, collapse = ", "), call. = FALSE)

  type
}

getPointOpts <- function(type, pch, n, col, bg, alpha, lwd, opts, theme) {

  curGlyphProps <- glyphProps[[type]]

  ## use line_color, line_width, etc. if specified
  ## else use specified 'col' and 'bg'
  ## otherwise turn to theme
  ## currently, pick color from theme based on 
  ## how many elements already exist in the plot
  ## this can surely be given more thought
  if(curGlyphProps$lp) {
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
  }

  if(curGlyphProps$fp) {
    if(!is.null(bg))
      opts$fill_color <- bg
    if(is.null(opts$fill_color)) {
      if(curGlyphProps$lp) {
        opts$fill_color <- reduceSaturation(opts$line_color)
      } else {
        opts$fill_color <- theme$glyph[(n + 1) %% length(theme$glyph)]
      }
    }
    if(!is.null(alpha))
      opts$fill_alpha <- alpha
  }

  ## if pch is specified, honor whether or not it has fill or line
  if(!is.null(pch)) {
    pchProps <- pchDict[[as.character(pch)]]
    if(!is.null(pchProps)) {
      if(!pchProps$line)
        opts["line_color"] <- list(NULL)
      if(!pchProps$fill) {
        opts["fill_color"] <- list(NULL)
      }
    }
  }


  if(curGlyphProps$tp) {
    if(!is.null(alpha))
      opts$text_alpha <- alpha
    opts$text_align = "center"
    opts$text_baseline = "middle"

    if(!is.null(col))
      opts$text_color <- col
    if(is.null(opts$text_color))
      opts$text_color <- theme$glyph[(n + 1) %% length(theme$glyph)]
    if(!is.null(alpha))
      opts$text_alpha <- alpha
  }

  opts
}

## give a little warning if any options are specified that won't be used
checkOpts <- function(opts, type) {
  curGlyphProps <- glyphProps[[type]]

  validOpts <- NULL
  if(curGlyphProps$lp)
    validOpts <- c(validOpts, linePropNames)
  if(curGlyphProps$fp)
    validOpts <- c(validOpts, fillPropNames)
  if(curGlyphProps$tp)
    validOpts <- c(validOpts, textPropNames)

  if(length(opts) > 0) {
    # only get names of opts that are not NULL
    idx <- which(sapply(opts, function(x) !is.null(x)))
    if(length(idx) > 0) {
      notUsed <- setdiff(names(opts)[idx], validOpts)
      if(length(notUsed) > 0)
        message("note - arguments not used: ", paste(notUsed, collapse = ", "))    
    }    
  }
}

getLineOpts <- function(lty, n, lwd, ljoin, col, alpha, opts, theme) {
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

## get variables when specified as names of a data frame
## and need to be deparsed and extracted
getVarData <- function(data, var) {
  tmp <- data[[paste(deparse(var), collapse = "")]]
  if(is.null(tmp))
    tmp <- eval(var)
  tmp
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

